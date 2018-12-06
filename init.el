(defconst elisp-directory (expand-file-name "~/.emacs.d/site-lisp/"))
(add-to-list 'load-path elisp-directory)
(progn (cd "~/.emacs.d/site-lisp/") (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/")
             t)
;; activate all the packages (in particular autoloads)
(package-initialize) ;; You might already have this line
(when (not package-archive-contents)
    (package-refresh-contents))

(setq package-list '(protobuf-mode auto-complete ido go-mode cuda-mode yaml-mode
                                   window-number neotree use-package flycheck flycheck-ycmd
                                   ycm company-ycm company-ycmd yasnippet eldoc))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; enable use-package
(require 'use-package)

;; enable line number
(require 'linum)
(global-linum-mode 1)
(setq-default column-number-mode 1)
(setq linum-format "%d ")

;; set meta key to window
(setq x-super-keysym 'meta)
;; redefine home and end key
(global-set-key (kbd "<home>") 'beginning-of-line)
(global-set-key (kbd "<end>") 'end-of-line)

;; enable ido
(require 'ido)
(ido-mode t)

;; enable window-number
(require 'window-number)
(window-number-mode 1)
(window-number-meta-mode t)

;; enable neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)

;; use Shift+arrow_keys to move cursor around split panes
(windmove-default-keybindings)
;; when cursor is on edge, move to the other side, as in a torus space
(setq windmove-wrap-around t )

;; color theme
(load-file "~/.emacs.d/color-theme/themes/material-theme.el")

;; column indicator
(setq-default fill-column 80)
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
(require 'protobuf-mode)
(require 'yaml-mode)
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)
(add-hook 'c++-mode-common-hook 'google-set-c-style)
(add-hook 'c++-mode-common-hook 'google-make-newline-indent)

(add-hook 'python-mode-hook
          (lambda()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent 4)))

(add-hook 'go-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)))

(add-hook 'racket-mode-hook
          (lambda ()
            (define-key racket-mode-map (kbd "C-c r") 'racket-run)))

(add-hook 'yaml-mode-hook
        (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(add-to-list 'auto-mode-alist '("\\.h$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cu$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.cuh$" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.rkt$" . racket-mode))
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))


;;;;;;;;;;;;;;;;;;;;;;; auto complete ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'auto-complete-config)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d//ac-dict")
;; (ac-config-default)
;; (global-auto-complete-mode t)

;; ;; (define-globalized-minor-mode real-global-auto-complete-mode
;; ;;  auto-complete-mode (lambda ()
;; ;;                       (if (not (minibufferp (current-buffer)))
;; ;;                         (auto-complete-mode 1))
;; ;;                       ))
;; ;; (real-global-auto-complete-mode t)
;; ;; (auto-completion better-defaults emacs-lisp git
;; ;;                 (shell :variables shell-default-height 30 shell-default-position 'bottom)
;; ;;                 syntax-checking version-control d go rust elixir swift yaml latex markdown osx spacemacs-layouts)
;; (setq company-tooltip-align-annotations t)


;;;;;;;;;;;;;;;;;;;;;;; ycm ;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Snippets
(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :init (yas-global-mode t))

;; Autocomplete
(use-package company
  :defer 10
  :diminish company-mode
  :bind (:map company-active-map
              ("M-j" . company-select-next)
              ("M-k" . company-select-previous))
  :preface
  ;; enable yasnippet everywhere
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  (defun company-mode/backend-with-yas (backend)
    (if (or 
         (not company-mode/enable-yas) 
         (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  :init (global-company-mode t)
  :config
  ;; no delay no autocomplete
  (validate-setq
   company-idle-delay 0
   company-minimum-prefix-length 2
   company-tooltip-limit 20)

  (validate-setq company-backends 
                 (mapcar #'company-mode/backend-with-yas company-backends)))


;; ;; Code-comprehension server
;; (use-package ycmd
;;   :ensure t
;;   ;; :init (add-hook 'c++-mode-hook #'ycmd-mode)
;;   :init (add-hook 'after-init-hook #'global-ycmd-mode)
;;   :config
;;   (set-variable 'ycmd-server-command '("/use/local/bin/python3" (expand-file-name "~/.emacs.d/ycmd/ycmd")))
;;   (set-variable 'ycmd-global-config (expand-file-name "~/.emacs.d/ycm_conf.py"))
;;   (set-variable 'ycmd-extra-conf-whitelist '("~/repo/*"))

;;   (use-package company-ycmd
;;     :ensure t
;;     :init (company-ycmd-setup)
;;     :config (add-to-list 'company-backends (company-mode/backend-with-yas 'company-ycmd))))

(require 'ycmd)
(add-hook 'after-init-hook #'global-ycmd-mode)
(set-variable 'ycmd-server-command `("/usr/local/bin/python3" ,(file-truename "~/.emacs.d/ycmd/ycmd")))
(set-variable 'ycmd-global-config (file-truename "~/.emacs.d/.ycm_extra_conf.py"))

(require 'company-ycmd)
(company-ycmd-setup)
(add-to-list 'company-backends (company-mode/backend-with-yas 'company-ycmd))
;; (add-hook 'after-init-hook 'global-company-mode)

;; On-the-fly syntax checking
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :init (global-flycheck-mode t))

(use-package flycheck-ycmd
  :commands (flycheck-ycmd-setup)
  :init (add-hook 'ycmd-mode-hook 'flycheck-ycmd-setup))

;; Show argument list in echo area
(use-package eldoc
  :diminish eldoc-mode
  :init (add-hook 'ycmd-mode-hook 'ycmd-eldoc-setup))

;; (require 'eldoc)
;; (autoload 'eldoc-in-minibuffer-mode "eldoc-eval")
;; (eldoc-in-minibuffer-mode 1)

;;;;;;;;;;;;;;;;;;;;;;; customize font ;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(column-number-mode 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 150 :width normal :foundry "nil" :family "Roboto Mono")))))
;;  '(default ((t (:inherit nil :stipple nil :background "#263238" :foreground "#ffffff" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 140 :width normal :foundry "nil" :family "Roboto Mono")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (racket-mode dr-racket-like-unicode cuda-mode auto-complete))))


(defun sort-buffers ()
  "Put the buffer list in alphabetical order."
  (interactive)
  (dolist (buff (buffer-list-sorted)) (bury-buffer buff))
  (when (interactive-p) (list-buffers))
  )

(defun buffer-list-sorted ()
  (sort (buffer-list) 
        (function
         (lambda
           (a b) (string<
                  (buffer-file-name a)
                  (buffer-file-name b)
                  )))))

(global-set-key "\M-s" 'sort-buffers)
