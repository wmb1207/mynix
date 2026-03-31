;;; package -- summary
;;; commentary:
;;; Code:

;; Package management via MELPA (no Nix on BSD)
(require 'package)
(setq package-enable-at-startup t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("gnu"   . "https://elpa.gnu.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(add-to-list 'load-path "~/.emacs.d/manual-packages/")

(require 'packages)
(install-packages packages)

(setq treesit-language-source-alist
      '((tsx "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src")))

(require 'whitespace)

(defun setqs ()
  "Run all the seq."
  (setq ring-bell-function 'ignore)
  (setq-default tab-width 8)
  (setq frame-resize-pixelwise t)
  (setq lsp-ui-doc-position 'at-point)
  (setq default-frame-alist '((undecorated . t)))
  (setq lsp-ui-doc-max-height 150)
  (setq ts-indent-level 2)
  (setq js-indent-level 2))

(defun enlarge-current-window ()
  (interactive)
  (enlarge-window (round (* (window-width) .1)) t))

(defun shrink-current-window ()
  (interactive)
  (shrink-window (round (* (window-width) .1)) t))

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
  (keymap-global-set  "C-x f" 'consult-ripgrep))

(defun my-set-margins (amount)
  "Set margins in current buffer."
  (setq left-margin-width amount)
  (setq right-margin-width amount))

(defun set-margins ()
  (interactive)
  (set-window-margins (selected-window) 0 0))

(defun theming ()
  (set-margins)
  (global-set-key (kbd "<escape>") 'keyboard-quit)
  (display-fill-column-indicator-mode t)
  (require 'neotree)

  (setq scroll-step            1
      scroll-conservatively  10000)
  "All the configs for theming and ui."
  (setq neo-window-fixed-size nil)
  (add-to-list 'default-frame-alist '(internal-border-width . 0))
  (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-11"))

  (set-frame-parameter (selected-frame) 'alpha '(100 100))
  (setq-default left-margin-width 0 right-margin-width 0 internal-border-width 0)
  (set-window-buffer nil (current-buffer))
  (custom-set-variables '(neo-window-position (quote right)))
  (add-hook 'prog-mode
	    'display-line-numbers-mode 1)
  (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
  (load-theme 'creamsody-darker t)

  (defun my-reset-font ()
    (set-face-attribute 'default nil
                        :font "DejaVu Sans Mono"
                        :height 110
                        :weight 'regular))
  (my-reset-font)
  (add-hook 'after-load-theme-hook #'my-reset-font)

  (set-face-background 'default "#1c1a18")
  (global-whitespace-mode 1)

  (setq whitespace-style
  	'(face trailing tabs spaces newline empty
               indentation::space indentation::tab space-mark tab-mark newline-mark))
  (setq whitespace-display-mappings
  	'((space-mark   ?\     [?\u00B7]     [?.])
          (newline-mark ?\n    [?\u21B5 ?\n] [?$ ?\n])
          (tab-mark     ?\t    [?\u2192 ?\t] [?\\ ?\t])))

  (dolist (face '(whitespace-space whitespace-tab whitespace-newline
                  whitespace-trailing whitespace-empty whitespace-indentation
                  whitespace-space-before-tab whitespace-space-after-tab))
    (set-face-attribute face nil :background "#1c1a18"))

  (when (display-graphic-p)
    (set-face-background 'fringe "#1c1a18")
    (set-frame-font "DejaVu Sans Mono-11" nil t))

  (set-frame-parameter (selected-frame) 'alpha '(95 . 95))
  (add-to-list 'default-frame-alist '(alpha . (95 . 95)))
  (add-to-list 'default-frame-alist '(mouse-color . "white"))
  (setq ring-bell-function 'ignore)
  (setq lsp-headerline-breadcrumb-enable nil)
  (setq lsp-headerline-breadcrumb-icons-enable nil)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (global-hl-line-mode 1)
  (line-number-mode 1)
  (setq display-line-numbers 'absolute)
  (vertico-mode 1)
  (vertico-buffer-mode 1)
  (tooltip-mode -1)
  (set-fringe-mode 0)
  (global-flycheck-mode)
  (setq neo-theme (if (display-graphic-p) 'arrow 'arrow))
  (flycheck-define-checker php-phpmd
  "A PHP code complexity checker using phpmd."
  :command ("phpmd" source "text" "codesize")
  :error-patterns
  ((warning line-start (file-name) ":" line ": " (message) line-end))
  :modes (php-mode php-ts-mode))
  (add-to-list 'flycheck-checkers 'php-phpmd)
  (set-mouse-color "white")

  (defun my-simple-modeline ()
  "Return a very small mode-line string."
  (let* ((buf (buffer-name))
         (mod (if (buffer-modified-p) "*" ""))
         (pos (format "L%s C%s"
                      (line-number-at-pos)
                      (current-column)))
         (time (format-time-string "%H:%M")))
    (format " %s%s  |  %s  |  %s " buf mod pos time)))

  (setq-default
   mode-line-format
   '((:eval (my-simple-modeline))))
  (setq-default header-line-format nil)
  (set-face-attribute 'mode-line nil :box nil :height 0.9)
  (set-face-attribute 'mode-line-inactive nil :box nil :height 0.9)

 (set-face-attribute 'tab-bar nil :box nil :height 0.9)
 (set-face-attribute 'tab-bar-tab nil :box nil)
 (set-face-attribute 'tab-bar-tab-inactive nil :box nil)

 (defvar my-ui-scale 0.9)

 (set-face-attribute 'mode-line nil :height my-ui-scale :box nil)
 (set-face-attribute 'mode-line-inactive nil :height my-ui-scale :box nil)
 (set-face-attribute 'minibuffer-prompt nil :height my-ui-scale)
 (set-face-attribute 'header-line nil :height my-ui-scale :box nil)
 (set-face-attribute 'tooltip nil :height my-ui-scale)

 (setq-default line-spacing 0.0)
 (set-fringe-mode 0)
 (require 'beframe)
 (setq beframe-global-buffers '("*scratch*" "*Messages*" "*Backtrace*"))
 (beframe-mode 1)
 (define-key global-map (kbd "C-c b") #'beframe-prefix-map)
 (add-to-list 'default-frame-alist '(internal-border-width . 0)))

(defun prog-time ()
  "All the programming languages stuff."

  (setq dape-key-prefix "\C-\M-d")
  (dape-breakpoint-global-mode)
  (setq dape-buffer-window-arrangement 'right)
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)
  (setq dape-inlay-hints t)
  (add-hook 'dape-compile-hook 'kill-buffer)

  (require 'lsp-mode)
  (require 'prettier)
  (setq lsp-completion-enable t)

  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored "package.json"))

  (add-hook 'eshell-load-hook #'eat-eshell-mode)
  (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
  (add-hook 'prog-mode-hook (lambda () (display-line-numbers-mode 1)))

  (auto-revert-mode 1)
  (add-hook 'typescript-mode #'lsp-deferred)
  (setq typsecript-indent-level 2)

  (add-hook 'php-mode-hook #'php-ts-mode)
  (add-hook 'php-mode-hook #'lsp-deferred)
  (add-hook 'php-ts-mode-hook #'lsp-deferred)
  (defun my-php-mode-setup ()
    (setq tab-width 4)
    (setq c-basic-offset 4)
    (setq indent-tabs-mode nil)
    (add-hook 'before-save-hook
              (lambda () (untabify (point-min) (point-max))) nil t))
  (add-hook 'php-mode-hook 'my-php-mode-setup)
  (add-hook 'php-ts-mode-hook 'my-php-mode-setup)

  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook #'line-number-mode)
  (defun my-go-mode-setup ()
    (lsp-deferred)
    (add-hook 'before-save-hook #'lsp-format-buffer nil t)
    (add-hook 'before-save-hook #'lsp-organize-imports nil t))
  (add-hook 'go-mode-hook #'my-go-mode-setup)
  (add-hook 'go-mode-hook (lambda () (display-line-numbers-mode 1)))

  (add-hook 'python-mode-hook (lambda () (require 'lsp-pyright)))
  (add-hook 'python-mode-hook 'python-ts-mode)
  (add-hook 'python-mode-hook 'pet-mode -10)
  (add-hook 'python-mode-hook 'python-black-on-save-mode)
  (add-hook 'python-mode-hook (lambda () (setq display-line-numbers 'absolute)))
  (add-hook 'kotlin-mode-hook #'lsp-deferred)
  (add-hook 'python-mode-hook (lambda () (display-line-numbers-mode 1)))

  (add-hook 'elixir-mode-hook #'lsp-deferred)
  (add-hook 'elixir-mode-hook (lambda () (display-line-numbers-mode 1)))

  (add-hook 'typescript-mode-hook 'prettier-mode)
  (add-hook 'typescript-mode-hook 'lsp-deferred)
  (add-hook 'typescript-mode-hook (lambda () (display-line-numbers-mode 1)))

  (add-hook 'after-init-hook #'global-prettier-mode)
  (add-hook 'typescript-mode #'prettier-mode)
  (add-hook 'yaml-mode-hook #'lsp-deferred)
  (cognitive-complexity-mode 1)

  (add-hook 'tuareg-mode-hook
	    (lambda()
	      (setq-local comment-style 'multi-line)
	      (setq-local comment-continue "   "))))

(when (daemonp)
  (defun my/setup-gui-frame (frame)
    (with-selected-frame frame
      (when (display-graphic-p)
        (message "GUI initialized"))))
  (add-hook 'after-make-frame-functions #'my/setup-gui-frame))

(defun configure ()
  "Execute all the config FNS."
  (display-time-mode 1)
  (setq display-time-format "%H:%M %d/%m/%Y")
  (setq display-time-interval 60)
  (setqs)
  (keymaps)
  (theming)
  ;; straight.el for packages not on MELPA
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
  (straight-use-package '(cognitive-complexity :host github :repo "emacs-vs/cognitive-complexity"))
  (prog-time))

(configure)
(provide 'init)

(defun git-switch (branch)
  (shell-command (concat "git switch " branch))
  (shell-command (concat "git pull " branch)))

(defun git-merge (branch)
  (git-switch branch)
  (git-switch "-")
  (shell-command (concat "git merge " branch)))

(defun git-rebase (branch)
  (git-switch branch)
  (git-switch "-")
  (shell-command (concat "git rebase " branch)))

(defun external-term ()
  "Start urxvt in the current file's dir."
  (interactive)
  (start-process "urxvt" nil "urxvt"))

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
             (split-window window nil (if split-window-right 'left  'right)))
        (and (window-splittable-p window)
             (split-window window nil (if split-window-below 'above 'below)))
        (and (eq window (frame-root-window (window-frame window)))
             (not (window-minibuffer-p window))
             (let ((split-width-threshold 0))
               (when (window-splittable-p window t)
                 (split-window window nil (if split-window-right
                                              'left
                                            'right))))))))

(defun my/open-project-sidebars ()
  (unless (bound-and-true-p my/project-sidebars-opened)
    (setq my/project-sidebars-opened t)
    (when (fboundp 'dired-sidebar-toggle-sidebar)
      (dired-sidebar-toggle-sidebar))
    (when (fboundp 'ibuffer-sidebar-show-sidebar)
      (ibuffer-sidebar-show-sidebar))))

(setq-default split-height-threshold  4
              split-width-threshold   160)
;;; init.el ends here
