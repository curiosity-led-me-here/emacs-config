;;; function-jump.el --- Jump from C/C++ calls to definitions -*- lexical-binding: t; -*-

(require 'xref)

(defun ashu-jump-to-function-definition ()
  "Jump from the function call at point to its definition.
When clangd finds more than one candidate, show the normal Xref chooser."
  (interactive)
  (call-interactively #'xref-find-definitions))

;; `C-x y' is unbound in this configuration.  Eglot supplies the Xref backend
;; for simpc-mode, so this follows C++ functions, methods, and declarations.
(global-set-key (kbd "C-x y") #'ashu-jump-to-function-definition)

(provide 'function-jump)
;;; function-jump.el ends here
