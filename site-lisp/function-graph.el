;;; function-graph.el --- Show intra-file C/C++ call graphs -*- lexical-binding: t; -*-

;; This is deliberately self-contained: it does not need Graphviz, a running
;; language server, or an internet connection.  It recognises C/C++ function
;; bodies and only draws edges to functions that are also defined in the file.

(require 'cl-lib)
(require 'subr-x)
(require 'button)

(cl-defstruct (ashu-function-graph--function
               (:constructor ashu-function-graph--make-function))
  name short-name start end calls)

(defconst ashu-function-graph--non-call-names
  '("alignof" "catch" "const_cast" "decltype" "delete" "dynamic_cast"
    "for" "if" "new" "reinterpret_cast" "sizeof" "static_cast" "switch"
    "throw" "typeid" "while")
  "Words that look like calls but are part of C++ syntax.")

(defvar-local ashu-function-graph--source-buffer nil
  "Source buffer represented by the current function graph.")

(defvar ashu-function-graph-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map special-mode-map)
    (define-key map (kbd "g") #'ashu-function-graph-refresh)
    map)
  "Keymap for `ashu-function-graph-mode'.")

(defvar ashu-function-graph--button-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map button-map)
    ;; `button-map' uses mouse-2 by default.  A tree browser should feel like
    ;; a normal outline, so allow a single left click as well.
    (define-key map [mouse-1] #'push-button)
    map)
  "Keymap used by function names in the call tree.")

(defface ashu-function-graph-branch-highlight
  '((t :background "#5b4700" :foreground "#fff4b8"))
  "Temporary highlight used when following a shared call-tree branch.")

(define-derived-mode ashu-function-graph-mode special-mode "Function Tree"
  "Major mode for a clickable intra-file function call tree.

Press \<ashu-function-graph-mode-map>\[ashu-function-graph-refresh] to redraw
the graph after changing the source file.")

(defun ashu-function-graph--mask-non-code (text)
  "Return TEXT with comments and quoted literals replaced by spaces.
Newlines are retained, so positions in the returned string still match TEXT."
  (let ((masked (copy-sequence text))
        (index 0)
        (length (length text))
        state)
    (while (< index length)
      (let ((character (aref text index))
            (next (and (< (1+ index) length) (aref text (1+ index)))))
        (pcase state
          ('line-comment
           (unless (eq character ?\n)
             (aset masked index ?\s))
           (when (eq character ?\n)
             (setq state nil)))
          ('block-comment
           (unless (eq character ?\n)
             (aset masked index ?\s))
           (when (and (eq character ?*) (eq next ?/))
             (aset masked (1+ index) ?\s)
             (setq index (1+ index)
             state nil)))
          ('string
           (unless (eq character ?\n)
             (aset masked index ?\s))
           (cond
            ((eq character ?\\)
             (when next
               (aset masked (1+ index) ?\s)
               (setq index (1+ index))))
            ((eq character ?\")
             (setq state nil))))
          ('character
           (unless (eq character ?\n)
             (aset masked index ?\s))
           (cond
            ((eq character ?\\)
             (when next
               (aset masked (1+ index) ?\s)
               (setq index (1+ index))))
            ((eq character ?')
             (setq state nil))))
          (_
           (cond
            ((and (eq character ?/) (eq next ?/))
             (aset masked index ?\s)
             (aset masked (1+ index) ?\s)
             (setq index (1+ index)
                   state 'line-comment))
            ((and (eq character ?/) (eq next ?*))
             (aset masked index ?\s)
             (aset masked (1+ index) ?\s)
             (setq index (1+ index)
                   state 'block-comment))
            ((eq character ?\")
             (aset masked index ?\s)
             (setq state 'string))
            ((eq character ?')
             (aset masked index ?\s)
             (setq state 'character))))))
      (setq index (1+ index)))
    masked))

(defun ashu-function-graph--matching-brace (text opening-brace)
  "Return the position of OPENING-BRACE's matching brace in TEXT, or nil."
  (let ((depth 1)
        (index (1+ opening-brace))
        (length (length text)))
    (while (and (> depth 0) (< index length))
      (pcase (aref text index)
        (?{ (setq depth (1+ depth)))
        (?} (setq depth (1- depth))))
      (setq index (1+ index)))
    (when (zerop depth)
      (1- index))))

(defun ashu-function-graph--header-start (text opening-brace)
  "Return the start position of the declaration before OPENING-BRACE."
  (let ((semicolon (cl-position ?\; text :end opening-brace :from-end t))
        (close-brace (cl-position ?} text :end opening-brace :from-end t))
        (open-brace (cl-position ?{ text :end opening-brace :from-end t)))
    (1+ (max -1 (or semicolon -1) (or close-brace -1) (or open-brace -1)))))

(defun ashu-function-graph--header-function-name (header)
  "Return the qualified function name found in HEADER, or nil.
HEADER must be the text immediately before a possible function body."
  (let ((case-fold-search nil)
        (position 0)
        candidate)
    ;; The final identifier before a parameter list is the function name.  A
    ;; qualifier is retained for labels, while the last component is used to
    ;; match ordinary calls in a body.
    (while (string-match
            "\\(\\(?:[[:alpha:]_][[:alnum:]_]*::\\)*~?[[:alpha:]_][[:alnum:]_]*\\)[[:space:]\n]*("
            header position)
      (unless candidate
        (setq candidate (match-string 1 header)))
      (setq position (match-end 0)))
    (unless (member candidate ashu-function-graph--non-call-names)
      candidate)))

(defun ashu-function-graph--short-name (name)
  "Return NAME without its C++ namespace or class qualification."
  (car (last (split-string name "::" t))))

(defun ashu-function-graph--functions (text)
  "Collect C/C++ function bodies defined in TEXT.
The returned positions are zero-based string positions."
  (let ((masked (ashu-function-graph--mask-non-code text))
        (index 0)
        (length (length text))
        functions)
    (while (< index length)
      (if (eq (aref masked index) ?{)
          (let* ((end (ashu-function-graph--matching-brace masked index))
                 (header-start (ashu-function-graph--header-start masked index))
                 (name (ashu-function-graph--header-function-name
                        (substring masked header-start index))))
            (if (and end name)
                (progn
                  (push (ashu-function-graph--make-function
                         :name name
                         :short-name (ashu-function-graph--short-name name)
                         :start header-start
                         :end end)
                        functions)
                  ;; A full function body cannot contain another definition
                  ;; that belongs to this file's call graph.
                  (setq index (1+ end)))
              (setq index (1+ index))))
        (setq index (1+ index))))
    (nreverse functions)))

(defun ashu-function-graph--populate-calls (functions text)
  "Populate FUNCTIONS' call lists from TEXT and return FUNCTIONS."
  (let ((masked (ashu-function-graph--mask-non-code text))
        (by-short-name (make-hash-table :test #'equal)))
    (dolist (function functions)
      (let* ((short-name (ashu-function-graph--function-short-name function))
             (existing (gethash short-name by-short-name)))
        (puthash short-name (cons function existing) by-short-name)))
    (dolist (function functions)
      (let ((body-start (1+ (or (cl-position ?{ masked
                                               :start (ashu-function-graph--function-start function)
                                               :end (ashu-function-graph--function-end function))
                                (ashu-function-graph--function-start function))))
            (body-end (ashu-function-graph--function-end function))
            calls)
        (save-match-data
          (let ((position body-start))
            (while (string-match
                    "\\_<\\([[:alpha:]_][[:alnum:]_]*\\)\\_>[[:space:]\n]*("
                    masked position)
              (let ((name (match-string 1 masked)))
                (when (and (< (match-beginning 0) body-end)
                           (not (member name ashu-function-graph--non-call-names)))
                  (dolist (target (gethash name by-short-name))
                    (cl-pushnew target calls)))
                (setq position (match-end 0))))))
        (setf (ashu-function-graph--function-calls function)
              (nreverse calls))))
    functions))

(defun ashu-function-graph--visit-function (button)
  "Visit the source definition represented by BUTTON."
  (let ((source-buffer (button-get button 'ashu-source-buffer))
        (function (button-get button 'ashu-function)))
    (unless (buffer-live-p source-buffer)
      (user-error "The source buffer for this function is no longer available"))
    (pop-to-buffer source-buffer)
    (widen)
    (goto-char (+ (point-min) (ashu-function-graph--function-start function)))
    (skip-chars-forward " \t\n")
    (recenter)))

(defun ashu-function-graph--insert-function-button (function source-buffer)
  "Insert a clickable label for FUNCTION from SOURCE-BUFFER."
  (insert-text-button
   (ashu-function-graph--function-name function)
   'action #'ashu-function-graph--visit-function
   'ashu-function function
   'ashu-source-buffer source-buffer
   'follow-link t
   'face 'link
   'keymap ashu-function-graph--button-map
   'help-echo "mouse-1 or RET: visit this function"))

(defun ashu-function-graph--follow-tree-branch (button)
  "Jump to and briefly highlight the previously expanded branch in BUTTON."
  (let* ((branch (button-get button 'ashu-branch))
         (start (car branch))
         (end (cdr branch)))
    (unless (and (markerp start) (marker-buffer start))
      (user-error "This call-tree branch is no longer available"))
    (goto-char start)
    (when (get-buffer-window (current-buffer) 0)
      (recenter))
    (when (and (markerp end) (marker-buffer end))
      (let ((overlay (make-overlay start end nil nil t)))
        (overlay-put overlay 'face 'ashu-function-graph-branch-highlight)
        (run-at-time 1.5 nil
                     (lambda (branch-overlay)
                       (when (overlayp branch-overlay)
                         (delete-overlay branch-overlay)))
                     overlay)))))

(defun ashu-function-graph--insert-tree-reference (function anchors)
  "Insert a link from FUNCTION to its earlier expanded branch in ANCHORS."
  (insert-text-button
   (ashu-function-graph--function-name function)
   'action #'ashu-function-graph--follow-tree-branch
   'ashu-branch (gethash function anchors)
   'follow-link t
   'face 'font-lock-constant-face
   'keymap ashu-function-graph--button-map
   'help-echo "mouse-1 or RET: jump to this expanded branch"))

(defun ashu-function-graph--roots (functions)
  "Return FUNCTIONS which have no callers in the current file."
  (let ((called (make-hash-table :test #'eq)))
    (dolist (function functions)
      (dolist (target (ashu-function-graph--function-calls function))
        (puthash target t called)))
    (cl-remove-if (lambda (function) (gethash function called)) functions)))

(defun ashu-function-graph--insert-tree
    (function source-buffer prefix last path shown anchors)
  "Insert FUNCTION and its callees as a call tree.
PATH tracks the active branch for cycle detection and SHOWN prevents duplicate
subtrees when several functions call the same target.  ANCHORS records each
expanded branch's buffer range for shared-call links."
  (let* ((branch-start (copy-marker (point)))
         (branch (cons branch-start nil)))
    (puthash function branch anchors)
  (insert prefix (if last "└─ " "├─ "))
  (ashu-function-graph--insert-function-button function source-buffer)
  (puthash function t shown)
  (insert "\n")
  (let* ((children (ashu-function-graph--function-calls function))
         (child-prefix (concat prefix (if last "   " "│  ")))
         (count (length children))
         (index 0))
    (dolist (child children)
      (setq index (1+ index))
      (let ((child-last (= index count)))
        (cond
         ((memq child path)
          (insert child-prefix (if child-last "└─ " "├─ "))
          (ashu-function-graph--insert-tree-reference child anchors)
          (insert "  ↩ cycle\n"))
         ((gethash child shown)
          (insert child-prefix (if child-last "└─ " "├─ "))
          (ashu-function-graph--insert-tree-reference child anchors)
          (insert "  ↩\n"))
         (t
          (ashu-function-graph--insert-tree
           child source-buffer child-prefix child-last (cons function path) shown anchors)))))
    (setcdr branch (copy-marker (point))))))

(defun ashu-function-graph--render-tree (functions file source-buffer)
  "Insert FUNCTIONS as a clickable call tree for FILE in the current buffer."
  (let ((shown (make-hash-table :test #'eq))
        (anchors (make-hash-table :test #'eq))
        (roots (ashu-function-graph--roots functions)))
    (insert (propertize (format "Function call tree — %s\n" (file-name-nondirectory file))
                        'face '(:weight bold :height 1.2)))
    (insert "Click a function name to visit its definition; click a ↩ reference to jump to its branch.  g refreshes; q closes.\n\n")
    (dolist (root roots)
      (ashu-function-graph--insert-tree root source-buffer "" t nil shown anchors)
      (insert "\n"))
    ;; A component made entirely of recursive calls has no root.  Add any
    ;; function not already represented so the tree always covers the file.
    (dolist (function functions)
      (unless (gethash function shown)
        (insert (propertize "Unreached or recursive component\n" 'face 'shadow))
        (ashu-function-graph--insert-tree function source-buffer "" t nil shown anchors)
        (insert "\n")))))

(defun ashu-function-graph--display (source-buffer functions)
  "Display a clickable call tree of FUNCTIONS from SOURCE-BUFFER."
  (let* ((file (or (buffer-file-name source-buffer) (buffer-name source-buffer)))
         (graph-buffer (get-buffer-create
                        (format "*Function Tree: %s*" (file-name-nondirectory file)))))
    (with-current-buffer graph-buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (ashu-function-graph-mode)
        (setq-local ashu-function-graph--source-buffer source-buffer)
        (ashu-function-graph--render-tree functions file source-buffer)
        (goto-char (point-min))))
    (pop-to-buffer graph-buffer)))

;;;###autoload
(defun ashu-show-function-call-graph ()
  "Show calls between the C/C++ functions defined in the current file.

Only calls whose targets are also defined in the current buffer receive an
edge; library and header-only calls are intentionally left out."
  (interactive)
  (let* ((source-buffer (current-buffer))
         (text (buffer-substring-no-properties (point-min) (point-max)))
         (functions (ashu-function-graph--populate-calls
                     (ashu-function-graph--functions text) text)))
    (if functions
        (ashu-function-graph--display source-buffer functions)
      (user-error "No C/C++ function bodies found in this buffer"))))

(defun ashu-function-graph-refresh ()
  "Redraw the graph using its source buffer's current contents."
  (interactive)
  (if (buffer-live-p ashu-function-graph--source-buffer)
      (with-current-buffer ashu-function-graph--source-buffer
        (ashu-show-function-call-graph))
    (user-error "The source buffer for this graph is no longer available")))

(global-set-key (kbd "C-c O") #'ashu-show-function-call-graph)

(provide 'function-graph)
;;; function-graph.el ends here
