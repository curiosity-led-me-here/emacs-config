;;; completion-popup.el --- Automatic completion menu for code -*- lexical-binding: t; -*-

;; Corfu presents candidates supplied by completion-at-point-functions.  In a
;; simpc-mode buffer, Eglot registers clangd as that completion source, so the
;; popup includes local variables, members, functions, types, and classes.

(require 'package)
(package-initialize)
(require 'corfu)
(require 'corfu-auto)

(setq corfu-auto t
      corfu-auto-delay 0.15
      corfu-auto-prefix 2
      corfu-cycle t
      corfu-preselect 'prompt)

(global-corfu-mode 1)

;; Keep completion feeling like a normal IDE: Tab accepts or expands the
;; selected suggestion and Return accepts a highlighted suggestion.
(define-key corfu-map (kbd "TAB") #'corfu-complete)
(define-key corfu-map [tab] #'corfu-complete)
(define-key corfu-map (kbd "RET") #'corfu-insert)

(provide 'completion-popup)
;;; completion-popup.el ends here
