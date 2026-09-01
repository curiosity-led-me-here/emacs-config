;;; night-mode.el --- Accessible global Night mode -*- lexical-binding: t; -*-

(defvar ashu-night-mode nil
  "Non-nil when `night' is active across editable file buffers.")

(defvar-local ashu-night--active nil
  "Non-nil when the current buffer has Night mode styling applied.")

(defvar-local ashu-night--saved-font-lock-mode nil
  "Whether Font Lock was enabled before Night mode was applied.")

(defvar-local ashu-night--saved-semantic-highlighting-mode nil
  "Whether custom semantic highlighting was enabled before Night mode.")

(defvar-local ashu-night--error-face-remap nil
  "Face-remapping cookie used by `night' for Flymake errors.")

(defun ashu-night--set-error-underline (enable)
  "Show Flymake errors with a low, double white underline when ENABLE is non-nil."
  (when ashu-night--error-face-remap
    (face-remap-remove-relative ashu-night--error-face-remap)
    (setq ashu-night--error-face-remap nil))
  (when enable
    (setq ashu-night--error-face-remap
          (face-remap-add-relative
           'flymake-error
           '(:underline (:style double-line :color "white" :position 0))))))

(defun ashu-night--editable-file-buffer-p ()
  "Return non-nil when the current buffer is an editable file buffer."
  (and buffer-file-name
       (not (minibufferp))
       (not buffer-read-only)))

(defun ashu-night--enable-current-buffer ()
  "Apply Night mode to the current buffer, preserving its prior state."
  (when (ashu-night--editable-file-buffer-p)
    (setq-local ashu-night--active t)
    (when font-lock-mode
      (setq-local ashu-night--saved-font-lock-mode t)
      (font-lock-mode -1))
    (when (bound-and-true-p ashu-semantic-highlighting-mode)
      (setq-local ashu-night--saved-semantic-highlighting-mode t)
      (ashu-semantic-highlighting-mode -1))
    (ashu-night--set-error-underline t)))

(defun ashu-night--disable-current-buffer ()
  "Restore the current buffer's appearance after Night mode."
  (when ashu-night--active
    (ashu-night--set-error-underline nil)
    (when ashu-night--saved-font-lock-mode
      (font-lock-mode 1))
    (when (and ashu-night--saved-semantic-highlighting-mode
               (fboundp 'ashu-semantic-highlighting-mode))
      (ashu-semantic-highlighting-mode 1))
    (setq-local ashu-night--active nil
                ashu-night--saved-font-lock-mode nil
                ashu-night--saved-semantic-highlighting-mode nil)))

(defun ashu-night--apply-to-current-buffer ()
  "Apply global Night mode to the current buffer when appropriate."
  (when ashu-night-mode
    (ashu-night--enable-current-buffer)))

(defun ashu-night--apply-to-all-buffers ()
  "Apply the current Night-mode state to every live buffer."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (if ashu-night-mode
          (ashu-night--enable-current-buffer)
        (ashu-night--disable-current-buffer)))))

(defun night ()
  "Toggle Night mode across all editable file buffers."
  (interactive)
  (setq ashu-night-mode (not ashu-night-mode))
  (ashu-night--apply-to-all-buffers)
  (message "Night mode %s" (if ashu-night-mode "enabled" "disabled")))

;; Apply the global setting to files opened after `night' was enabled.  The
;; Eglot hook catches C/C++ buffers whose semantic highlighter starts later.
(add-hook 'after-change-major-mode-hook #'ashu-night--apply-to-current-buffer t)
(add-hook 'eglot-managed-mode-hook #'ashu-night--apply-to-current-buffer t)

(provide 'night-mode)
;;; night-mode.el ends here
