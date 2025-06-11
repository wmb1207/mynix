;;; packages.el --- package definitions and installer -*- lexical-binding: t; -*-

(require 'cl-lib) ;; for cl-loop, cl-remove-if, etc.

(setq packages
      '((:package base16-theme)
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
        (:package inf-clojure
         :hook ((clojure-mode . inf-clojure-minor-mode))
         :config (setq inf-clojure-program "bb"))
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
        (:package corfu
         :custom (corfu-auto t))
        (:package emacs
         :init (setq completion-cycle-threshold 3))
        (:package company)
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
        (:package scala-mode
         :interpreter ("scala" . scala-mode))
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
