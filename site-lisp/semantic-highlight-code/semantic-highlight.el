;;; ashu-semantic-highlight.el --- clangd-backed semantic highlighting -*- lexical-binding: t; -*-

;; This is intentionally NOT regex-based highlighting.
;; clangd decides what each token means; this package only decides how to draw it.

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'eglot)
(require 'jsonrpc)

(defgroup ashu-semantic nil
  "Semantic highlighting driven by clangd through Eglot."
  :group 'faces
  :prefix "ashu-semantic-")

(defcustom ashu-semantic-idle-delay 0.18
  "Seconds of idle time before requesting fresh semantic tokens."
  :type 'number
  :group 'ashu-semantic)

;; These are token *categories* understood by clangd/LSP, not source-code
;; patterns.  clangd performs the actual parsing and classification.
(defconst ashu-semantic-token-types
  '("namespace"
    "type" "class" "enum" "interface" "struct" "typeParameter"
    "parameter" "variable" "property" "enumMember" "event"
    "function" "method" "macro" "keyword" "modifier"
    "comment" "string" "number" "regexp" "operator"
    "decorator" "bracket" "label" "unknown" "concept")
  "Semantic token kinds this client tells clangd it can display.")

(defconst ashu-semantic-token-modifiers
  '("declaration" "definition" "readonly" "static" "deprecated"
    "abstract" "async" "modification" "documentation" "defaultLibrary"
    ;; clangd extensions
    "deduced" "virtual" "dependentName"
    "usedAsMutableReference" "usedAsMutablePointer"
    "constructorOrDestructor" "userDefined"
    "functionScope" "classScope" "fileScope" "globalScope")
  "Semantic token modifiers this client tells clangd it understands.")

;; VS Code Dark Modern uses Dark+'s syntax palette.  Match its token colors so
;; semantic identifiers blend with the standard VS Code dark editor scheme.
(defface ashu-semantic-namespace-face
  '((t (:foreground "#4EC9B0")))
  "Namespaces." :group 'ashu-semantic)

(defface ashu-semantic-type-face
  '((t (:foreground "#4EC9B0")))
  "Types, classes, structs, enums, concepts and template parameters."
  :group 'ashu-semantic)

(defface ashu-semantic-variable-face
  '((t (:foreground "#9CDCFE")))
  "Variables." :group 'ashu-semantic)

(defface ashu-semantic-parameter-face
  '((t (:foreground "#9CDCFE")))
  "Function parameters." :group 'ashu-semantic)

(defface ashu-semantic-property-face
  '((t (:foreground "#9CDCFE")))
  "Fields and properties." :group 'ashu-semantic)

(defface ashu-semantic-function-face
  '((t (:foreground "#DCDCAA")))
  "Functions and methods." :group 'ashu-semantic)

(defface ashu-semantic-enum-member-face
  '((t (:foreground "#4FC1FF")))
  "Enum constants." :group 'ashu-semantic)

(defface ashu-semantic-macro-face
  '((t (:foreground "#569CD6")))
  "Macros." :group 'ashu-semantic)

(defface ashu-semantic-keyword-face
  '((t (:foreground "#C586C0")))
  "Keywords and language modifiers." :group 'ashu-semantic)

(defface ashu-semantic-string-face
  '((t (:foreground "#CE9178")))
  "Strings and regexps." :group 'ashu-semantic)

(defface ashu-semantic-number-face
  '((t (:foreground "#B5CEA8")))
  "Numeric literals." :group 'ashu-semantic)

(defface ashu-semantic-comment-face
  '((t (:foreground "#6A9955")))
  "Comments and inactive code." :group 'ashu-semantic)

(defface ashu-semantic-punctuation-face
  '((t (:foreground "#D4D4D4")))
  "Operators and brackets." :group 'ashu-semantic)

(defface ashu-semantic-label-face
  '((t (:foreground "#C8C8C8")))
  "Labels." :group 'ashu-semantic)

(defface ashu-semantic-unknown-face
  '((t (:foreground "#D4D4D4")))
  "Tokens clangd classifies as unknown/dependent." :group 'ashu-semantic)

(defface ashu-semantic-deprecated-face
  '((t (:foreground "#F44747" :strike-through t)))
  "Deprecated semantic tokens." :group 'ashu-semantic)

(defcustom ashu-semantic-type-face-alist
  '(("namespace" . ashu-semantic-namespace-face)
    ("type" . ashu-semantic-type-face)
    ("class" . ashu-semantic-type-face)
    ("enum" . ashu-semantic-type-face)
    ("interface" . ashu-semantic-type-face)
    ("struct" . ashu-semantic-type-face)
    ("typeParameter" . ashu-semantic-type-face)
    ("concept" . ashu-semantic-type-face)
    ("parameter" . ashu-semantic-parameter-face)
    ("variable" . ashu-semantic-variable-face)
    ("property" . ashu-semantic-property-face)
    ("event" . ashu-semantic-property-face)
    ("enumMember" . ashu-semantic-enum-member-face)
    ("function" . ashu-semantic-function-face)
    ("method" . ashu-semantic-function-face)
    ("macro" . ashu-semantic-macro-face)
    ("keyword" . ashu-semantic-keyword-face)
    ("modifier" . ashu-semantic-keyword-face)
    ("string" . ashu-semantic-string-face)
    ("regexp" . ashu-semantic-string-face)
    ("number" . ashu-semantic-number-face)
    ("comment" . ashu-semantic-comment-face)
    ("operator" . ashu-semantic-punctuation-face)
    ("bracket" . ashu-semantic-punctuation-face)
    ("label" . ashu-semantic-label-face)
    ("unknown" . ashu-semantic-unknown-face))
  "Map clangd semantic token kinds to Emacs faces."
  :type '(alist :key-type string :value-type face)
  :group 'ashu-semantic)

(defcustom ashu-semantic-modifier-face-alist
  '(("deprecated" . ashu-semantic-deprecated-face))
  "Optional extra faces for clangd semantic token modifiers.
Unknown modifiers remain available to the inspector but do not change color."
  :type '(alist :key-type string :value-type face)
  :group 'ashu-semantic)

(defvar-local ashu-semantic--overlays nil)
(defvar-local ashu-semantic--timer nil)
(defvar-local ashu-semantic--generation 0)

(defun ashu-semantic--advertise-capabilities (original server)
  "Augment Eglot client capabilities so clangd sends semantic tokens."
  (let* ((caps (copy-tree (funcall original server)))
         (workspace (copy-tree (plist-get caps :workspace)))
         (text-document (copy-tree (plist-get caps :textDocument)))
         (semantic
          `(:dynamicRegistration :json-false
            :requests (:full (:delta :json-false))
            :overlappingTokenSupport :json-false
            :multilineTokenSupport :json-false
            :tokenTypes ,(vconcat ashu-semantic-token-types)
            :tokenModifiers ,(vconcat ashu-semantic-token-modifiers)
            :formats ["relative"])))
    ;; We deliberately do not ask the server for refresh callbacks; this
    ;; package refreshes after local edits and on explicit request.
    (setq workspace
          (plist-put workspace :semanticTokens '(:refreshSupport :json-false)))
    (setq text-document
          (plist-put text-document :semanticTokens semantic))
    (setq caps (plist-put caps :workspace workspace))
    (setq caps (plist-put caps :textDocument text-document))
    caps))

;; The advice must exist before Eglot connects, because LSP capabilities are
;; negotiated once during initialization.
(unless (advice-member-p #'ashu-semantic--advertise-capabilities
                         'eglot-client-capabilities)
  (advice-add 'eglot-client-capabilities
              :around #'ashu-semantic--advertise-capabilities))

(defun ashu-semantic--clear ()
  "Remove semantic overlays from the current buffer."
  (mapc #'delete-overlay ashu-semantic--overlays)
  (setq ashu-semantic--overlays nil))

(defun ashu-semantic--text-document-identifier ()
  "Return the current buffer's LSP TextDocumentIdentifier."
  (cond
   ((fboundp 'eglot--TextDocumentIdentifier)
    (eglot--TextDocumentIdentifier))
   ((and buffer-file-name (fboundp 'eglot-path-to-uri))
    (list :uri (eglot-path-to-uri buffer-file-name)))
   ((and buffer-file-name (fboundp 'eglot--path-to-uri))
    (list :uri (eglot--path-to-uri buffer-file-name)))
   (t
    (error "Cannot construct an LSP document identifier for this buffer"))))

(defun ashu-semantic--legend ()
  "Return clangd's semantic token legend for the current Eglot server."
  (let* ((provider (eglot-server-capable :semanticTokensProvider))
         (legend (and (listp provider) (plist-get provider :legend))))
    (unless legend
      (error
       (concat
        "clangd did not advertise semantic tokens. "
        "Restart Emacs or run M-x eglot-reconnect after loading this package")))
    legend))

(defun ashu-semantic--modifier-names (bits modifier-vector)
  "Decode modifier BITS using MODIFIER-VECTOR."
  (cl-loop for index from 0 below (length modifier-vector)
           when (not (zerop (logand bits (ash 1 index))))
           collect (aref modifier-vector index)))

(defun ashu-semantic--faces-for (type modifiers)
  "Return the faces to use for semantic TYPE and MODIFIERS."
  (let ((base (alist-get type ashu-semantic-type-face-alist nil nil #'string=))
        extras)
    (dolist (modifier modifiers)
      (when-let ((face (alist-get modifier
                                 ashu-semantic-modifier-face-alist
                                 nil nil #'string=)))
        (push face extras)))
    (delq nil (cons base (nreverse extras)))))

(defun ashu-semantic--make-overlay (beg end type modifiers)
  "Paint BEG..END according to clangd TYPE and MODIFIERS."
  (when (< beg end)
    (let* ((faces (ashu-semantic--faces-for type modifiers))
           (overlay (make-overlay beg end nil nil nil)))
      (when faces
        (overlay-put overlay 'face faces))
      ;; High enough to color identifiers over ordinary font-lock, but not
      ;; intended to suppress diagnostics such as Flymake underlines.
      (overlay-put overlay 'priority 50)
      (overlay-put overlay 'evaporate t)
      (overlay-put overlay 'ashu-semantic t)
      (overlay-put overlay 'ashu-semantic-type type)
      (overlay-put overlay 'ashu-semantic-modifiers modifiers)
      (overlay-put overlay 'help-echo
                   (if modifiers
                       (format "clangd: %s [%s]"
                               type (string-join modifiers ", "))
                     (format "clangd: %s" type)))
      (push overlay ashu-semantic--overlays))))

(defun ashu-semantic--apply (response legend)
  "Decode semantic token RESPONSE using LEGEND and repaint this buffer."
  (let* ((data (plist-get response :data))
         (token-types (plist-get legend :tokenTypes))
         (token-modifiers (plist-get legend :tokenModifiers)))
    (unless (and (vectorp data)
                 (vectorp token-types)
                 (vectorp token-modifiers))
      (error "Malformed semantic token response from clangd"))

    (ashu-semantic--clear)

    (save-restriction
      (widen)
      (save-excursion
        (goto-char (point-min))
        (let ((column 0))
          (cl-loop
           for i from 0 below (length data) by 5
           for delta-line = (aref data i)
           for delta-start = (aref data (+ i 1))
           for token-length = (aref data (+ i 2))
           for type-index = (aref data (+ i 3))
           for modifier-bits = (aref data (+ i 4))
           do
           (if (> delta-line 0)
               (progn
                 (forward-line delta-line)
                 (setq column delta-start))
             (setq column (+ column delta-start)))

           (when (< type-index (length token-types))
             (let* ((type (aref token-types type-index))
                    (modifiers
                     (ashu-semantic--modifier-names
                      modifier-bits token-modifiers))
                    beg end)
               ;; Eglot already negotiated the server's UTF-8/16/32 position
               ;; encoding and exposes the correct inverse position function.
               (funcall eglot-move-to-linepos-function column)
               (setq beg (point))
               (funcall eglot-move-to-linepos-function
                        (+ column token-length))
               (setq end (point))
               (ashu-semantic--make-overlay beg end type modifiers)))))))))

(defun ashu-semantic-refresh ()
  "Request a full semantic-token pass from clangd and repaint the buffer."
  (interactive)
  (unless ashu-semantic-highlighting-mode
    (user-error "ashu-semantic-highlighting-mode is not enabled"))

  (let ((server (eglot-current-server)))
    (unless server
      (user-error "No Eglot server is managing this buffer"))

    ;; Push any edits that Eglot is still batching before asking clangd to
    ;; classify the current text.  This keeps request ordering correct.
    (when (fboundp 'eglot--signal-textDocument/didChange)
      (eglot--signal-textDocument/didChange))

    (let* ((legend (ashu-semantic--legend))
           (document (ashu-semantic--text-document-identifier))
           (buffer (current-buffer))
           (tick (buffer-chars-modified-tick))
           (generation (cl-incf ashu-semantic--generation)))
      (jsonrpc-async-request
       server
       :textDocument/semanticTokens/full
       `(:textDocument ,document)
       :success-fn
       (lambda (response)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             ;; Never paint a response for text that has already changed.
             (when (and ashu-semantic-highlighting-mode
                        (= generation ashu-semantic--generation)
                        (= tick (buffer-chars-modified-tick)))
               (ashu-semantic--apply response legend)))))
       :error-fn
       (lambda (&rest error-data)
         (when (buffer-live-p buffer)
           (with-current-buffer buffer
             (message "Semantic highlighting request failed: %S" error-data))))))))

(defun ashu-semantic--run-scheduled-refresh (buffer generation)
  "Refresh BUFFER if GENERATION is still current."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq ashu-semantic--timer nil)
      (when (and ashu-semantic-highlighting-mode
                 (= generation ashu-semantic--generation)
                 (eglot-current-server))
        (ashu-semantic-refresh)))))

(defun ashu-semantic--schedule-refresh (&rest _ignored)
  "Debounce semantic highlighting after a buffer edit."
  (when (timerp ashu-semantic--timer)
    (cancel-timer ashu-semantic--timer))
  (let ((buffer (current-buffer))
        (generation (cl-incf ashu-semantic--generation)))
    (setq ashu-semantic--timer
          (run-at-time
           ashu-semantic-idle-delay nil
           #'ashu-semantic--run-scheduled-refresh
           buffer generation))))

(defun ashu-semantic-describe-token ()
  "Show clangd's semantic classification for the token at point."
  (interactive)
  (let ((overlay
         (seq-find (lambda (ov) (overlay-get ov 'ashu-semantic))
                   (overlays-at (point)))))
    (if overlay
        (message "clangd token: %s%s"
                 (overlay-get overlay 'ashu-semantic-type)
                 (let ((mods (overlay-get overlay 'ashu-semantic-modifiers)))
                   (if mods
                       (format "  modifiers: %s" (string-join mods ", "))
                     "")))
      (message "No clangd semantic token at point"))))

;;;###autoload
(define-minor-mode ashu-semantic-highlighting-mode
  "Color source code using clangd semantic tokens instead of regex guesses."
  :lighter " SemHL"
  (cond
   (ashu-semantic-highlighting-mode
    ;; Avoid two semantic highlighters fighting over the same text.
    (when (and (fboundp 'eglot-semantic-tokens-mode)
               (bound-and-true-p eglot-semantic-tokens-mode))
      (eglot-semantic-tokens-mode -1))
    (add-hook 'after-change-functions #'ashu-semantic--schedule-refresh nil t)
    (ashu-semantic-refresh))
   (t
    (remove-hook 'after-change-functions #'ashu-semantic--schedule-refresh t)
    (when (timerp ashu-semantic--timer)
      (cancel-timer ashu-semantic--timer))
    (setq ashu-semantic--timer nil)
    (cl-incf ashu-semantic--generation)
    (ashu-semantic--clear))))

(provide 'ashu-semantic-highlight)
;;; ashu-semantic-highlight.el ends here
