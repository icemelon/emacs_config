(defconst elisp-directory (expand-file-name "~/.emacs.d/site-lisp/"))
(add-to-list 'load-path elisp-directory)
(progn (cd "~/.emacs.d/site-lisp/") (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize) ;; You might already have this line

;; enable line number
(require 'linum)
(global-linum-mode 1)
(setq-default column-number-mode 1)
(setq linum-format "%d")

;; use Shift+arrow_keys to move cursor around split panes
(windmove-default-keybindings)
;; when cursor is on edge, move to the other side, as in a torus space
(setq windmove-wrap-around t )

;; color theme
(load-file "~/.emacs.d/color-theme/themes/material-theme.el")

;; column indicator
(require 'fill-column-indicator)
(setq-default fci-rule-column 80)
(setq-default fci-rule-width 1)
(setq-default fci-rule-color "tan3")
(define-globalized-minor-mode global-fci-mode fci-mode (lambda () (fci-mode 1)))
(global-fci-mode 1)

;; set tab style
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

(require 'cc-mode)
(require 'google-c-style)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c++-mode-common-hook 'google-set-c-style)
(add-hook 'c++-mode-common-hook 'google-make-newline-indent)

(add-hook 'python-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)
            (setq tab-width 2)
            (setq python-indent 2)))

(add-hook 'go-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)))

;; auto complete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
(ac-config-default)
(global-auto-complete-mode t)

;(define-globalized-minor-mode real-global-auto-complete-mode
;  auto-complete-mode (lambda ()
;                       (if (not (minibufferp (current-buffer)))
;                         (auto-complete-mode 1))
;                       ))
;(real-global-auto-complete-mode t)
;(auto-completion better-defaults emacs-lisp git
;                 (shell :variables shell-default-height 30 shell-default-position 'bottom)
;                 syntax-checking version-control d go rust elixir swift yaml latex markdown osx spacemacs-layouts)
(setq company-tooltip-align-annotations t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#263238" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "nil" :family "Monaco")))))
