(setq gc-cons-threshold (* 50 1000 1000)) ; 50mb

(require 'package)
(require 'image)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp/")

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-defer t)
(setq use-package-always-ensure t)

(use-package envrc
  :demand t
  :config
  (envrc-global-mode))

(use-package savehist
  :demand t
  :config
  (savehist-mode))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :demand t
  :config
  (exec-path-from-shell-initialize))

;; Modal Editing - Meow
(defun meow-setup ()
  "Configure Meow modal editing keybindings."
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("<escape>" . ignore)))

(use-package meow
  :demand t
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package projectile
  :demand t
  :init
  (setq projectile-project-search-path '("~/dev/" "/opt/projects/Conio/" "/opt/projects/Conio/clients"))
  :config
  (projectile-mode +1)
  
  (projectile-register-project-type 'zig '("build.zig")
                                    :project-file "build.zig"
                                    :compile "zig build"
                                    :test "zig build test"
                                    :run "zig build run")
  (projectile-register-project-type 'rust '("Cargo.toml")
                                    :project-file "Cargo.toml"
                                    :compile "rustc"
                                    :test "rustc"
                                    :run "rustc")
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map)))

(use-package magit)

(use-package company
  :hook (after-init . global-company-mode))

(use-package flycheck)

(use-package multiple-cursors)

(use-package xclip
  :demand t
  :config
  (xclip-mode 1))

(use-package lsp-mode
  :ensure t
  :init
  (setq lsp-keymap-prefix "C-c l"
        lsp-enable-file-watchers nil
        lsp-file-watch-threshold 2500
        lsp-completion-enable t
	lsp-enable-snippet nil
	lsp-inlay-hint-enable t)
  :hook ((zig-mode python-mode c-mode go-mode rust-mode) . lsp-deferred)
  :commands lsp-deferred)

(use-package lsp-treemacs)

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (setq lsp-pyright-venv-path "$VENV_DIR"
			       lsp-pyright-type-checking-mode "strict")
                         (lsp-deferred))))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (setq dap-print-io t)
  (require 'dap-dlv-go))

(use-package zig-mode
  :mode "\\.zig\\'"
  :hook (zig-mode . (lambda ()
                      (setq compile-command "zig build"))))

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook ((rust-mode . (lambda () 
                        (setq indent-tabs-mode nil
                              lsp-rust-analyzer-cargo-watch-command "clippy"
                              lsp-rust-analyzer-server-display-inlay-hints t
			      lsp-rust-analyzer-cargo-extra-env nil)))
         (rust-mode . prettify-symbols-mode))
  :init
  (setq rust-format-on-save t))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (before-save . gofmt-before-save)
  :init
  (setq lsp-go-use-gofumpt t
        lsp-go-analyses '((shadow . t)
                          (simplifycompositelit . :json-false))))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook (yaml-mode . (lambda ()
                       (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package json-mode
  :mode "\\.json\\'")

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode))
  :init
  (setq markdown-command "multimarkdown"))

(use-package mermaid-mode
  :mode "\\.mmd\\'")

(use-package just-mode
  :mode "\\(?:J\\|j\\)ustfile\\'")

(use-package nix-mode
  :mode "\\.nix\\'")

(use-package doom-modeline
  :demand t
  :init
  (setq doom-modeline-project-detection 'projectile
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-env-version t)
  :config
  (doom-modeline-mode 1))

(use-package nerd-icons)


;;;; Editor Behaviour
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)

;; Theme and appearance
(load-theme 'wombat t)
(set-face-attribute 'default nil :height 100)
(add-to-list 'default-frame-alist '(alpha . 97))

(setq custom-file (make-temp-file "emacs-custom"))
(setq backup-directory-alist '(("." . "~/emacsbackup")))

;; Reload current buffer
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer :ignore-auto :noconfirm))

;; kill all buffers
(defun kill-all-buffers ()
  "Kill all buffers, leaving only the *scratch* buffer."
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (switch-to-buffer "*scratch*"))

;; Key binding
(global-set-key (kbd "C-c f") 'find-name-dired)
(global-set-key (kbd "C-c d") 'dired)
(global-set-key (kbd "C-c g") 'grep-find)
(global-set-key (kbd "C-c t") 'lsp-treemacs-symbols)
(global-set-key (kbd "C-c m") 'mc/edit-lines)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c k") 'kill-all-buffers)
(global-set-key (kbd "C-c r") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-c c") 'compile)

;; Enable 'a' key in dired to open in same buffer
(put 'dired-find-alternate-file 'disabled nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000)))) ; 2mb after startup

(provide 'init)
