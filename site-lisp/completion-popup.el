;;; completion-popup.el --- Inline completion previews for code -*- lexical-binding: t; -*-

;; Eglot registers clangd as a completion-at-point source in simpc-mode.  The
;; built-in preview shows clangd's best variable, member, function, class, or
;; type suggestion as faint text at point instead of opening a popup window.

(require 'completion-preview)

;; Corfu was previously enabled here.  Turn it off on reload so existing
;; buffers immediately switch from its menu to the inline preview.
(when (fboundp 'global-corfu-mode)
  (global-corfu-mode -1))

(setq completion-preview-minimum-symbol-length 2
      completion-preview-idle-delay 0.15
      completion-preview-exact-match-only nil
      completion-preview-message-format nil)

(global-completion-preview-mode 1)

(provide 'completion-popup)
;;; completion-popup.el ends here
