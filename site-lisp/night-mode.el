;;; night-mode.el --- Accessible diagnostic styling for Night mode -*- lexical-binding: t; -*-

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

(defun night ()
  "Toggle ordinary and clangd semantic highlighting in this buffer."
  (interactive)
  (let ((enable
         (not (or font-lock-mode
                  (bound-and-true-p ashu-semantic-highlighting-mode)))))
    (font-lock-mode (if enable 1 -1))
    (when (fboundp 'ashu-semantic-highlighting-mode)
      (ashu-semantic-highlighting-mode (if enable 1 -1)))
    ;; `night' is active while ordinary and semantic highlighting are off.
    (ashu-night--set-error-underline (not enable))))

(provide 'night-mode)
;;; night-mode.el ends here
