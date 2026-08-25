
(load-theme 'modus-vivendi t)

(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq inhibit-startup-screen t)

(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 150)

;; Remove GUI clutter
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Dark built-in theme
(load-theme 'modus-vivendi t)

;; Font
(set-face-attribute 'default nil
                    :family "Menlo"
                    :height 160)

;; Cleaner appearance
(setq inhibit-startup-screen t)
(setq-default cursor-type 'bar)
(setq visible-bell t)

;; Show line numbers only in programming files
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Highlight matching parentheses
(show-paren-mode 1)

;; Remember recently opened files
(recentf-mode 1)
