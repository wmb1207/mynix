;;; packages.el --- package definitions and installer -*- lexical-binding: t; -*-

(require 'cl-lib) ;; for cl-loop, cl-remove-if, etc.

(setq packages
      '((:package base16-theme)
	(:package ibuffer-sidebar
		  :ensure t)
	(:package  detached
		   :ensure t
		   :init
		   (detached-init)
		   :bind (;; Replace `async-shell-command' with `detached-shell-command'
			  ([remap async-shell-command] . detached-shell-command)
			  ;; Replace `compile' with `detached-compile'
			  ([remap compile] . detached-compile)
			  ([remap recompile] . detached-compile-recompile)
			  ;; Replace built in completion of sessions with `consult'
			  ([remap detached-open-session] . detached-consult-session))
		   :custom ((detached-show-output-on-attach t)
			    (detached-terminal-data-command system-type)))
	(:package acme-theme
		  :ensure t)
	(:package direnv
		  :ensure t)
	(:package gptel
		  :ensure t)
	(:package tango-plus-theme
		  :ensure t)
	(:package tango-2-theme
		  :ensure t)
	(:package tuareg
		  :ensure t)
	(:package merlin
		  :ensure t
		  :config
		  (add-hook 'tuareg-mode-hook #'merlin-mode)
		  ;;(add-hook 'merxblin-mode-hook #'corfu-mode)
		  ;; we're using flycheck instead
		  (setq merlin-error-after-save nil))
	
	(:package merlin-eldoc
	  :ensure t
	  :hook ((tuareg-mode) . merlin-eldoc-setup))
	

	;;(:package nano-modeline)
	(:package emms
		  :ensure t)
	(:package plan9-theme
		  :ensure t)
	(:package org-present
	  :ensure t
	  :config
	  (add-hook 'org-present-mode-hook
		    (lambda ()
		      (org-present-big)
		      (org-display-inline-images)
		      (org-present-hide-cursor)
		      (org-present-read-only)))
	  (add-hook 'org-present-mode-quit-hook
		    (lambda ()
              (org-present-small)
              (org-remove-inline-images)
              (org-present-show-cursor)
              (org-present-read-write))))
	(:package flyover)
	(:package lsp-mode)
	(:package nix-mode)
        (:package multi-vterm)
        (:package exec-path-from-shell)
        (:package php-mode)
        (:package php-runtime)
        (:package composer)
        (:package stimmung-themes)
        (:package parchment-theme)
        (:package srcery-theme)
        (:package eat)
        (:package rainbow-delimiters)
        (:package prettier)
        (:package arjen-grey-theme)
        (:package dired-sidebar :commands (dired-sidebar-toggle-sidebar))
        (:package pet)
        (:package solaire-mode)
        (:package inf-ruby)
        (:package realgud)
        (:package editorconfig :config (editorconfig-mode +1))
        (:package indium)
        (:package dap-mode :after lsp-mode :commands dap-debug
                  :hook ((python-mode . dap-ui-mode)
                         (python-mode . dap-mode)))
        (:package dape
         :preface (setq dape-key-prefix "\C-\M-d")
         :config (dape-breakpoint-global-mode)
                 (setq dape-buffer-window-arrangement 'right)
                 (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)
                 (setq dape-inlay-hints t)
                 (add-hook 'dape-compile-hook 'kill-buffer))
        (:package clojure-mode)
        ;; (:package Inf-clojure
        ;;  :hook ((clojure-mode . inf-clojure-minor-mode))
        ;;  :config (setq inf-clojure-program "bb"))
        (:package repeat
         :config (repeat-mode))
        (:package ef-themes)
        (:package lsp-mode)
        (:package treemacs)
        (:package go-mode)
        (:package vterm)
        (:package ein)
        (:package ob-restclient)
        (:package verb)
        (:package ace-window)
        (:package typescript-mode
         :after lsp-mode
         :mode ("\\.ts$")
         :hook (typescript-mode . lsp-deferred))
        (:package dired-subtree)
        (:package sublime-themes)
        (:package lsp-ui)
        (:package creamsody-theme)
        (:package autothemer)
        (:package load-env-vars)
        ;; (:package corfu
        ;;  :custom (corfu-auto t))
        (:package emacs
		  :init (setq completion-cycle-threshold 3))
	;; (:package corfu
	;; 	  :init
	;; 	  (global-corfu-mode)
	;; 	  :custom
	;; 	  ;; auto popup
	;; 	  (corfu-auto t)
	;; 	  (corfu-auto-delay 0.0)
	;; 	  (corfu-auto-prefix 1)
	;; 	  (corfu-quit-no-match 'separator)
	;; 	  (corfu-preview-current nil) ;; optional
	;; 	  :config
	;; 	  (corfu-popupinfo-mode))
        (:package eww)
        (:package elixir-mode)
        (:package inf-elixir)
        (:package python-black :after python)
        (:package py-isort)
        (:package lsp-pyright
         :hook (python-mode . (lambda () (require 'lsp-pyright)))
         :init (when (executable-find "python3")
                 (setq lsp-pyright-python-executable-cmd "python3")))
        (:package python-isort)
        (:package pyvenv
         :config (pyvenv-mode 1))
        (:package dockerfile-mode)
        (:package terraform-mode)
        (:package org)
        (:package yaml-mode)
        (:package magit)
        (:package vertico)
        (:package vertico-posframe)
        (:package consult)
        (:package flycheck-inline)
        (:package rjsx-mode)
        (:package web-mode)
        (:package lsp-mode
         :commands (lsp lsp-deferred))
        (:package neotree)
        (:package ace-window)
        (:package flycheck-golangci-lint)
        (:package ample-theme)
        (:package cider)
        (:package inf-clojure)
        (:package clojure-ts-mode)
        (:package scala-ts-mode
		  :interpreter ("scala" . scala-mode))
	(:package lsp-metals
		  :ensure t)
        (:package sbt-mode
         :commands (sbt-start sbt-command)
         :config (substitute-key-definition
                   'minibuffer-complete-word
                   'self-insert-command
                   minibuffer-local-completion-map)
                 (setq sbt:program-options '("-Dsbt.supershell=false")))
        (:package autothemer)))

(defun plist-remove-key (plist key)
  "Return a new plist with KEY removed."
  (let ((res '()))
    (while plist
      (let ((k (pop plist))
            (v (pop plist)))
        (unless (eq k key)
          (setq res (append res (list k v))))))
    res))

(defun valid-packages? (pkgs)
  (unless (listp pkgs)
    (error "Argument must be a list"))
  (dolist (elem pkgs)
    (unless (and (listp elem)
                 (plist-member elem :package))
      (error "Each element must be a plist containing :package key, but got: %S" elem))))

(defun install-packages (packages)
  "Install/load packages using `use-package` with automatic :ensure t.
PACKAGES is a list of plists with a :package key naming the package symbol,
plus other use-package keywords.

Example element:
  (:package magit :bind (\"C-x g\" . magit-status))

This function adds `:ensure t` automatically."
  (dolist (pkg packages)
    (let* ((pkg-name (plist-get pkg :package))
           (orig-args (plist-remove-key pkg :package))
           ;; Insert :ensure t forcibly, overriding any existing :ensure
           (args (plist-put orig-args :ensure t))
           (form (append (list 'use-package pkg-name) args)))
      (message "Installing/loading package: %s" pkg-name)
      (eval form))))

(provide 'packages)
