;;; package -- summaryp
;;; commentary:
;;; Code:


(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
			 ("melpa" . "https://melpa.org/packages/")
			 ("nongnu" . "https://elpa.nongnu.org/nongnu/")
			 ("org" . "https://orgmode.org/elpa/")))

(setq inhibit-startup-message t)
(tool-bar-mode 1)
(menu-bar-mode 1)
(scroll-bar-mode -1)

(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

(unless (package-installed-p 'vc-use-package)
  (package-vc-install "https://github.com/slotThe/vc-use-package"))

(require 'vc-use-package)
;; Requires
(defun requires ()
  
  "Just a little way to define all the requires."
  (require 'package)
  (require 'package)
  (require 'whitespace)
  (require 'lsp-mode)
  (require 'treesit))


;; (defun prog-pkgs ()
;;   (use-package dape
;;     :ensure t)
;;   (use-package lsp-modej
;;     :ensure t)
;;   (use-package emacs
;;     :init
;;     (setq completion-cycle-threshold 3))
;;   (use-package company
;;     :ensure t)

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(defun packages ()

  (use-package modern-tab-bar
  :ensure (modern-tab-bar :host github :repo "aaronjensen/emacs-modern-tab-bar" :protocol ssh)
  :init
  (setq tab-bar-show t
        tab-bar-new-button nil
        tab-bar-close-button-show nil)

  (modern-tab-bar-mode))
  ;; (use-package compat
  ;;   :ensure t)

  ;; (package-refresh-contents)
  ;; (package-install 'compat)
  ;; PHP
  (use-package base16-theme
    :ensure t)
  (use-package multi-vterm
    :ensure t)
  (use-package exec-path-from-shell
    :ensure t)
  (use-package php-mode
    :ensure t)
  (use-package php-runtime
    :ensure t)
  (use-package composer
    :ensure t)
  ;; (use-package psysh
  ;;   :ensure t)
  ;; (use-package php-cs-fixer
  ;;   :ensure t)
  ;; END PHP
  (use-package stimmung-themes
    :ensure t)

  ;; (use-package zenburn-theme
  ;;   :ensure t)
  ;; (use-package projectile
  ;;   :ensure t)

  (use-package parchment-theme
    :ensure t)

  (use-package srcery-theme
    :ensure t)
  
  (use-package eat
    :ensure t)
  ;; (use-packages rainbow-delimiters
  ;;   :ensure t)
  (use-package pg :vc (:fetcher github :repo emarsden/pg-el))
  (use-package pgmacs :vc (:fetcher github :repo emarsden/pgmacs))
  (use-package prettier
    :ensure t)
  ;; comments
  (use-package arjen-grey-theme
    :ensure t)
  (use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))
  (use-package pet
    :ensure t)
  (use-package solaire-mode
    :ensure t)
  (use-package inf-ruby
    :ensure t)
  (use-package realgud
    :ensure t)
  (use-package editorconfig
    :ensure t
    :config (editorconfig-mode +1))
  (use-package indium
    :ensure t)
  (use-package dap-mode
    :after lsp-mode
    :commands dap-debug
    :hook ((python-mode . dap-ui-mode)
	   (python-mode . dap-mode)))
  (use-package dape
    :preface
    (setq dape-key-prefix "\C-\M-d")

    :config
    (dape-breakpoint-global-mode)
    (setq dape-buffer-window-arrangement 'right)
    (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)
    (setq dape-inlay-hints t)
    (add-hook 'dape-compile-hook 'kill-buffer)
    :ensure t)

  ;; Enable repeat mode for more ergonomic `dape' use
  (use-package repeat
    :ensure t
    :config
    (repeat-mode))
  (use-package ef-themes :ensure t)
  (use-package lsp-mode :ensure t)
  "Just a little function to install all the packages."
  (use-package treemacs :ensure t)
  (use-package go-mode :ensure t)
  (use-package vterm :ensure t)
  (use-package ein :ensure t)
  (use-package ob-restclient :ensure t)
  (use-package verb :ensure t)
  (use-package ace-window
    :ensure t)
  (use-package typescript-mode
    :after lsp-mode
    :mode ("\.ts$")
    :hook (typescript-mode . lsp-deferred)
    :ensure t)
  (use-package dired-subtree
    :ensure t)
  (use-package sublime-themes
    :ensure t)

  (use-package lsp-ui
    :ensure t)
  (use-package creamsody-theme :ensure t)
  (use-package autothemer
  :ensure t)
  (use-package load-env-vars
  :ensure t)
  (use-package corfu
    :ensure t
    :custom
    (corfu-auto t))
  (use-package emacs
    :init
    (setq completion-cycle-threshold 3))
  (use-package company
    :ensure t)
  ;; (use-package powerline
  ;;   :ensure t)
  (use-package eww
    :ensure t)
  (use-package elixir-mode
    :ensure t)
  (use-package inf-elixir
    :ensure t)
  (use-package python-black
    :ensure t
    :after python)
  (use-package py-isort
    :ensure t)
  (use-package lsp-pyright
    :ensure t
    :hook (python-mode . (lambda ()
			   (require 'lsp-pyright)))
    :init (when (executable-find "python3")
	    (setq lsp-pyright-python-executable-cmd "python3")))
  (use-package python-isort
    :ensure t)
  (use-package pyvenv
    :ensure t
    :config (pyvenv-mode 1))
  (use-package dockerfile-mode
    :ensure t)
  (use-package terraform-mode
    :ensure t)
  (use-package org
    :ensure t)
  (use-package yaml-mode
    :ensure t)
  (use-package magit
    :ensure t)
  (use-package exec-path-from-shell
    :ensure t)
  (use-package vertico
    :ensure t)

  (use-package vertico-posframe
    :ensure t)
  (use-package consult
    :ensure t)
  (use-package flycheck-inline
    :ensure t)
  (use-package rjsx-mode
    :ensure t)
  (use-package typescript-mode
    :ensure t)
  (use-package web-mode
    :ensure t)
  (use-package lsp-mode
    :commands (lsp lsp-deferred)
    :ensure t)
  (use-package neotree
    :ensure t)
  (use-package ace-window
    :ensure t)
  (use-package flycheck-golangci-lint
    :ensure t)
  (use-package ample-theme
    :ensure t)


  ;; Setting up all the packages that I should have for developing with clojure
  (use-package cider
    :ensure t)
  (use-package inf-clojure
    :ensure t)
  (use-package clojure-ts-mode
    :ensure t)

  ;; SCALA TIME
  (use-package scala-mode
    :interpreter ("scala" . scala-mode)
    :ensure t)
  (use-package sbt-mode
    :commands sbt-start sbt-command
    :config
    ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
    ;; allows using SPACE when in the minibuffer
    (substitute-key-definition
     'minibuffer-complete-word
     'self-insert-command
     minibuffer-local-completion-map)
    ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
    (setq sbt:program-options '("-Dsbt.supershell=false"))
    :ensure t)
  ;; (use-package lsp-metals
  ;;   :ensure t)
  (use-package autothemer
    :ensure t)

  ;; (require 'pgmacs)

  ;; (use-package pgmacs
  ;;   :ensure t)
  )


(defun setqs ()
  "Run all the seq."
  ;; (setq vertico-posframe-height 50)
  ;; (setq vertico-count 45)
  (setq ring-bell-function 'ignore)
  (setq-default tab-width 8)
  (setq frame-resize-pixelwise t)
  (setq lsp-ui-doc-position 'at-point)
  (setq default-frame-alist '((undecorated . t)))
  (setq lsp-ui-doc-max-height 150)
  (setq ts-indent-level 2)
  (setq js-indent-level 2)
  (setq corfu-auto-prefix 1)
  (setq corfu-auto-delay 1))

(defun enlarge-current-window ()
  (interactive)
  (enlarge-window (round (* (window-width) .1)) t))

(defun shrink-current-window ()
  (interactive)
  (shrink-window (round (* (window-width) .1)) t))

;; Keybindings
;;(keymap-global-set (
(defun keymaps ()
  "Set all the keymaps."
  (keymap-global-set "C-c w h" 'windmove-left)
  (keymap-global-set "C-c w l" 'windmove-right)
  (keymap-global-set "C-c w j" 'windmove-up)
  (keymap-global-set "C-c w k" 'windmove-down)
  (keymap-global-set "C-x !" 'flymake-show-buffer-diagnostics)
  (keymap-global-set "C-x t t" 'tab-bar-switch-to-next-tab)
  (keymap-global-set "C-x n t" 'tab-new)
  (keymap-global-set "C-x t l" 'lsp-ui-doc-show)
  (keymap-global-set "C-x t L" 'lsp-ui-doc)
  (keymap-global-set "C-x t e" 'flymake-show-diagnostic)
  (keymap-global-set  "C-x a b" 'dap-breakpoint-toggle)
  (keymap-global-set  "C-x v t" 'multi-vterm)
  (keymap-global-set "C-c w e" 'enlarge-current-window)
  (keymap-global-set "C-c w d" 'shrink-current-window)
  (keymap-global-set  "C-x f" 'consult-ripgrep)
  (with-eval-after-load 'org
    (define-key org-mode-map (kbd "C-c C-r") verb-command-map)))
;; End keybindings

(defun bootstrap ()
  "Bootstrap."
  (defvar bootstrap-version)
  (let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
	(bootstrap-version 6))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer (url-retrieve-synchronously "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el" 'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

(defun my-set-margins (amount)
  "Set margins in current buffer."
  (setq left-margin-width amount)
  (setq right-margin-width amount))

(defun set-margins ()
  (interactive)
  (set-window-margins (selected-window) 24 24))

(defun theming ()
  (set-margins)
  (defun my-open-dired-on-new-frame (frame)
  "Open Dired in the left sidebar when a new FRAME is created."
  (select-frame frame)  ;; Ensure we're working on the new frame
  (dired-sidebar-toggle-sidebar)  ;; Open Dired Sidebar
  (ibuffer-sidebar-toggle-sidebar))

  (add-hook 'after-make-frame-functions 'my-open-dired-on-new-frame)
  (global-set-key (kbd "<escape>") 'keyboard-quit)
  ;;(global-set-key (kbd "<escape>") 'ignore)
  (display-fill-column-indicator-mode t)
  (require 'neotree)
  ;; (solaire-global-mode +1)
  ;; (require 'powerline)
  ;; (powerline-default-theme)
  ;; (projectile-mode 1)

  (setq scroll-step            1
      scroll-conservatively  10000)
  "All the configs for theming and ui."
  (setq neo-window-fixed-size nil)
    
  (add-to-list 'default-frame-alist '(font . "cherry-12"))
  (set-frame-font "cherry-12" nil t)
  (set-face-attribute
   'default nil
   :font "cherry"
   :height 12
   :weight 'regular)

  ;; (add-to-list 'default-frame-alist '(font . "IBM Plex Mono-12"))
  ;; (set-frame-font "IBM Plex Mono-12" nil t)
  ;; (set-face-attribute
  ;;  'default nil
  ;;  :font "IBM Plex Mono"
  ;;  :height 12
  ;;  :weight 'regular)
  
  (set-frame-parameter (selected-frame) 'alpha '(100 100))
  (setq-default left-margin-width 0 right-margin-width 0 internal-border-width 0) ; Define new widths.
  (set-window-buffer nil (current-buffer)) ; Use them now.
  (custom-set-variables '(neo-window-position (quote right)))

  (add-hook 'prog-mode
	    'display-line-numbers-mode 1)

  (setq whitespace-style '(face tabs spaces trailing lines space-mark tab-mark newline))
  (setq whitespace-display-mappings '((space-mark ?\  [?·])
                                      (tab-mark ?\t [?» ?\t] [?\\ ?\t])))
  (setq whitespace-line-column 300)


  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  ;;(load-theme 'automata t)
  ;;(load-theme 'nano-dark t)
  

  ;;(load-theme 'timu-spacegrey t)
  ;;(load-theme 'spacegray t)
  ;;(load-theme 'doom-zenburn t)
  ;;(load-theme 'brin t)
  ;;(load-theme 'spolsky t)
  (load-theme 'base16-vesper t)
  ;;(load-theme 'base16-rose-pine t)
  ;;(load-theme 'zenburn t)
  ;;(load-theme 'ample-light t)
  ;;(load-theme 'ample-flat t)
  ;;(load-theme 'srcery t)

  (set-face-background 'default "#000000")
  (global-whitespace-mode 1)

  
  (setq whitespace-style
	'(face trailing tabs spaces lines-tail newline empty
               indentation::space indentation::tab space-mark tab-mark newline-mark))

  (setq whitespace-display-mappings
	'((space-mark   ?\     [?\u00B7]     [?.])      ; space → ·
          (newline-mark ?\n    [?\u21B5 ?\n] [?$ ?\n])  ; newline → ↵
          (tab-mark     ?\t    [?\u2192 ?\t] [?\\ ?\t]) ; tab → →
          ))

  (custom-set-faces
   ;; All backgrounds black
   '(whitespace-space           ((t (:background "#000000" :foreground "#2e2e2e"))))
   '(whitespace-tab             ((t (:background "#000000" :foreground "#444444"))))
   '(whitespace-trailing        ((t (:background "#000000" :foreground "#ff5555" :weight bold))))
   '(whitespace-line            ((t (:background "#000000" :foreground "#ff79c6"))))
   '(whitespace-newline         ((t (:background "#000000" :foreground "#5f5f5f"))))
   '(whitespace-indentation     ((t (:background "#000000" :foreground "#3e3e3e"))))
   '(whitespace-empty           ((t (:background "#000000" :foreground "#ff6c6b")))))



  (set-face-background 'fringe "#000000")
  (set-face-attribute 'line-number nil
                      :background "#000000") ;; optional
  
  (set-face-attribute 'header-line nil
                      :background "#000000"
                      :box nil)

  (set-face-attribute 'mode-line-inactive nil
                    :background "#000000")

  (set-face-attribute 'mode-line nil
                      :background "#000000"
		      :box nil)

  (add-to-list 'default-frame-alist '(alpha 100 100))
  
  ;; Remove the default mode line at the bottom
  ;; (setq mode-line-format '("%e" mode-line-front-space mode-line-mule-info
  ;;                        mode-line-client mode-line-modified mode-line-remote
  ;;                        mode-line-frame-identification mode-line-buffer-identification
  ;;                        " " mode-line-position
  ;;                        (vc-mode vc-mode) " " mode-line-misc-info
  ;;                        mode-line-end-spaces))
  
  ;; ;; Define a custom mode line at the top
  ;; (defvar my-mode-line-format
  ;; '(" "
  ;;   mode-name             ;; Show the mode name
  ;;   " | "
  ;;   buffer-file-name      ;; Show the buffer file name
  ;;   " | "
  ;;   (:eval (if (buffer-modified-p)
  ;;              (propertize "Unsaved" 'face '(:foreground "red"))
  ;;            (propertize "Saved" 'face '(:foreground "green"))))
  ;;   " | "
  ;;   (:eval (format-time-string "%Y-%m-%d %H:%M:%S" (current-time)))))  ;; Show current date and time

  ;; (setq-default header-line-format my-mode-line-format)

  ;; (load-theme 'modus-operandi t)
  (setq ring-bell-function 'ignore)
  (setq lsp-headerline-breadcrumb-enable t)
  (setq lsp-headerline-breadcrumb-icons-enable 0)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (global-hl-line-mode 1)
  (line-number-mode 1)
  (setq display-line-numbers 'absolute)
  (vertico-mode 1)
  (vertico-buffer-mode 1)
  (tooltip-mode -1)           ; Disable tooltips
  (set-fringe-mode 10)        ; Give some breathing roo
  ;;(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
  ;;(vertico-posframe-mode 1)
  (global-flycheck-mode)
  (setq neo-theme (if (display-graphic-p) 'arrow 'arrow))
  ;;(setq-default mode-line-format 'nil)
  (add-hook 'flycheck-mode-hook 'flycheck-inline-mode))
;; End Theme

(defun prog-time ()
  "All the programming languages stuff."
  (require 'lsp-mode)
  (require 'prettier)

  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored "package.json"))

  ;; For `eat-eshell-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-mode)

  ;; For `eat-eshell-visual-command-mode'.
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)

  (add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

  (auto-revert-mode 1)
  (add-hook 'typescript-mode #'lsp-deferred)
  (setq typsecript-indent-level 2)

  (add-hook 'php-mode-hook #'lsp-deferred)
  (defun my-php-mode-setup ()
    "Custom PHP mode setup to use 4 spaces for indentation."
    (setq tab-width 4)
    (setq c-basic-offset 4)
    (setq indent-tabs-mode nil)) ;; Use spaces instead of tabs

  (add-hook 'php-mode-hook 'my-php-mode-setup)

  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'line-number-mode)
  (defun lsp-go-install-save-hooks ()
    (add-hook 'before-save-hook lsp-format-buffer t t)
    (add-hook 'before-save-hook lsp-organize-imports t t))
  (add-hook 'go-mode-hook #'lsp-go-install-save-hooks)
  (add-hook 'go-mode-hook (lambda () (display-line-numbers-mode 1)))

  (add-hook 'python-mode-hook 'python-ts-mode)
  (add-hook 'python-mode-hook 'pet-mode -10)
  (add-hook 'python-mode-hook 'python-black-on-save-mode)
  (add-hook 'python-mode-hook (lambda ()
				(setq display-line-numbers 'absolute)))
  (add-hook 'kotlin-mode-hook #'lsp-deferred)
  (add-hook 'python-mode-hook #'company-mode)
  (add-hook 'python-mode-hook (lambda() (company-mode 0)))
  (add-hook 'python-mode-hook (lambda () (display-line-numbers-mode 1)))

  (add-hook 'elixir-mode-hook #'lsp-deferred)
  (add-hook 'elixir-mode-hook (lambda () (display-line-numbers-mode 1)))

  (add-hook 'typescript-mode-hook 'prettier-mode)
  (add-hook 'typescript-mode-hook (lambda () (display-line-numbers-mode 1)))

  (add-hook 'after-init-hook #'global-prettier-mode)
  (add-hook 'typescript-mode #'prettier-mode)

  ;;(add-hook 'clojure-mode-hook 'raindow-delimiters-mode)
  ;;(add-hook 'emacs-lisp-mode-hook 'raindow-delimiters-mode)

  ;; Terraform time
  ;;(add-hook 'terraform-mode-hook #'terraform-format-on-save-mode)
  ;; End terraform
  ;; Yaml Time
  (add-hook 'yaml-mode-hook #'lsp-deferred))



(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
(when (daemonp)
  (exec-path-from-shell-initialize))

(defun databases ()
       (setq sql-connection-alist
	     '((messages-qa
		(sql-product 'postgres)
		(sql-port 5432)
		(sql-server "100.66.36.10")
		(sql-user "OPE_IMMSJ")
		(sql-password "dgM4YXmtzXZKg")
		(sql-database "QA_IMMSJ"))

               (messages-dev
		(sql-product 'postgres)
		(sql-port 5432)
		(sql-server "100.66.49.10")
		(sql-user "OPE_IMMSJ")
		(sql-password "fbdkSoJbT8qk5")
		(sql-database "DEV_IMMSJ"))
	       
               (messages-local
		(sql-product 'postgres)
		(sql-port 5434)
		(sql-server "localhost")
		(sql-user "OPE_IMMSJ")
		(sql-password "fbdkSoJbT8qk5")
		(sql-database "DEV_IMMSJ"))

               (messages-pp
		(sql-product 'postgres)
		(sql-port 5432)
		(sql-server "100.66.21.10")
		(sql-user "OPE_IMMSJ")
		(sql-password "dgM4YXmtzXZKg")
		(sql-database "PP_IMMSJ"))
	       
               (messages-pr
		(sql-product 'postgres)
		(sql-port 5432)
		(sql-server "100.66.5.10")
		(sql-user "OPE_IMMSJ")
		(sql-password "FD9r3LefsRu")
		(sql-database "PR_IMMSJ"))

               (admin-qa
		(sql-product 'postgres)
		(sql-port 5432)
		(sql-server "100.66.36.10")
		(sql-user "OPE_IMADMIN")
		(sql-password "8bgSgUouNOQRd")
		(sql-database "QA_IMADMIN"))

               (admin-dev
		(sql-product 'postgres)
		(sql-port 5432)
		(sql-server "100.66.49.10")
		(sql-user "OPE_IMADMIN")
		(sql-password "XGbKsb9Cb8Cj0")
		(sql-database "DEV_IMADMIN"))
	       
               (admin-pp
		(sql-product 'postgres)
		(sql-port 5432)
		(sql-server "100.66.21.10")
		(sql-user "OPE_IMADMIN")
		(sql-password "8bgSgUouNOQRd")
		(sql-database "PP_IMADMIN"))))

       (defun db-messages-qa ()
	 (interactive)
	 (db-connect 'postgres 'messages-qa)
	 (setq sql-password "fbdkSoJbT8qk5"))

       
       (defun db-messages-dev ()
	 (interactive)
	 (db-connect 'postgres 'messages-dev))
       
       (defun db-messages-local ()
	 (interactive)
	 (db-connect 'postgres 'messages-local))

       (defun db-messages-pp ()
	 (interactive)
	 (db-connect 'postgres 'messages-pp))

       (defun db-messages-pr ()
	 (interactive)
	 (db-connect 'postgres 'messages-pr))

       (defun db-admin-qa ()
	 (interactive)
	 (db-connect 'postgres 'admin-qa))

       (defun db-admin-dev ()
	 (interactive)
	 (db-connect 'postgres 'admin-dev))

       (defun db-admin-pp ()
	 (interactive)
	 (db-connect 'postgres 'admin-pp))
       
       (defun db-connect (product connection)
	 ;; remember to set the sql-product, otherwise, it will fail for the first time
	 ;; you call the function
	 (setq sql-product product)
	 (sql-connect connection)))
       

(defun configure ()
  "Execute all the config FNS."
  (requires)
  (packages)
  (setqs)
  (keymaps)
  ;; ;; (company)				;  (bootstrap)
  (theming)
  (databases)

  (defun start-app ()
    "List applications in ~/Applications and /Applications, then open the selected one."
    (interactive)
    (let* ((app-paths (split-string
                       (shell-command-to-string
			"find /Applications ~/Applications \\( -type l -o -type d \\) -name '*.app' -maxdepth 1 2>/dev/null | sort")
                       "\n" t))
           (choices (mapcar (lambda (path)
                              (let ((name (downcase (file-name-base path))))
				(cons name path)))
                            app-paths))
           (selection (completing-read "Launch app: " (mapcar #'car choices))))
      (when (and selection (not (string-empty-p selection)))
	(let ((full-path (cdr (assoc selection choices))))
          (start-process "open-app" nil "open" full-path)))))

  (defun firefox ()
    (interactive)
    (start-process "Firefox" nil "open" "/Users/wmb/Applications/Firefox.app"))

  (defun spotify ()
    (interactive) 
    (start-process "Spotify" nil "open" "/Users/wmb/Applications/Spotify.app"))

  (defun brave ()
    (interactive)
    (start-process "Brave" nil "open" "/Applications/Brave Browser.app"))

  (defun slack ()
    (interactive)
    (start-process "Slack" nil "open" "/Users/wmb/Applications/Slack.app"))

  (defun discord ()
    (interactive)
    (start-process "Discord" nil "open" "/Users/wmb/Applications/Discord.app"))

  (defun wpp ()
    (interactive)
    (start-process "Discord" nil "open" "/Applications/WhatsApp.app"))
  
  (defvar bootstrap-version)
  (let ((bootstrap-file
	 (expand-file-name
          "straight/repos/straight.el/bootstrap.el"
          (or (bound-and-true-p straight-base-dir)
              user-emacs-directory)))
	(bootstrap-version 7))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
           "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
           'silent 'inhibit-cookies)
	(goto-char (point-max))
	(eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage))
  (straight-use-package '(myron-themes :host github :repo "neeasade/myron-themes" :files ("*.el" "themes/*.el")))
  (straight-use-package '(nano-theme :type git :host github
                                   :repo "rougier/nano-theme"))
  
  (prog-time))

(configure)
(provide 'init)

(defun dump-rules ()
  (interactive)
  (shell-command-to-string
   "pg_dump -h localhost --port 5434--column-inserts --data-only -U sinacofi --format=tar migration_msg > $(date '+%Y-%m-%d').msg.history.dump.tar"))




(defun git-switch (branch)
  (shell-command (concat "git switch " branch))
  (shell-command (concat "git pull " branch)))


(defun git-merge (branch)
  ;; Simple script to merge two branches locally
  (git-switch branch)
  (git-switch "-")
  (shell-command (concat "git merge " branch)))


(defun git-rebase (branch)
  ;; rebase to an specific branch
  (git-switch branch)
  (git-switch "-")
  (shell-command (concat "git rebase " branch)))

(defun external-term ()
  "Start urxvt in the current file's dir"
  (interactive)
  (start-process "open /Applications/iTerm.app" nil "open /Applications/iTerm.app"))
  

;; Fix annoying vertical window splitting.
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2015-08/msg00339.html
(with-eval-after-load "window"
  (defcustom split-window-below nil
    "If non-nil, vertical splits produce new windows below."
    :group 'windows
    :type 'boolean)

  (defcustom split-window-right nil
    "If non-nil, horizontal splits produce new windows to the right."
    :group 'windows
    :type 'boolean)

  (fmakunbound #'split-window-sensibly)

  (defun split-window-sensibly
      (&optional window)
    (setq window (or window (selected-window)))
    (or (and (window-splittable-p window t)
             ;; Split window horizontally.
             (split-window window nil (if split-window-right 'left  'right)))
        (and (window-splittable-p window)
             ;; Split window vertically.
             (split-window window nil (if split-window-below 'above 'below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             ;; If WINDOW is the only window on its frame and is not the
             ;; minibuffer window, try to split it horizontally disregarding the
             ;; value of `split-width-threshold'.
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (split-window window nil (if split-window-right
                                              'left
                                            'right))))))))

(setq-default split-height-threshold  4
              split-width-threshold   160) ; the reasonable limit for horizontal splits


;;; init.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-vc-selected-packages
   '((pgmacs :vc-backend Git :url "https://github.com/emarsden/pgmacs")
     (pg :vc-backend Git :url "https://github.com/emarsden/pg-el"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'set-goal-column 'disabled nil)
