(require 'package)
(require 'image)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'use-package)

;; Packages
;;(use-package use-package-ensure-system-package
;;  :ensure t)

(defun meow-setup ()
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
  :ensure t
  :config
  (meow-setup)
  (meow-global-mode 1)
  )

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


(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

(use-package savehist
  :init
  (savehist-mode))

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/dev/" "/opt/projects/Conio/" "/opt/projects/Conio/clients"))
  (projectile-register-project-type 'zig '("build.zig")
                                    :project-file "build.zig"
                                    :compile "zig build"
                                    :test "zig build test"
                                    :run "zig build run"
                                    )
  (projectile-register-project-type 'rust '("Cargo.toml")
                                    :project-file "Cargo.toml"
                                    :compile "rustc"
                                    :test "rustc"
                                    :run "rustc"
                                    )
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :hook
  (after-init . projectile-global-mode))

(use-package magit
  :ensure t
  )

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-file-watchers nil)
  (setq lsp-file-watch-threshold 2500)
  (setq lsp-completion-enable t)
  (setq lsp-enable-snippet nil)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (zig-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (go-mode . lsp-deferred)
	 (rust-mode . lsp-deferred)
         ;; if you want which-key integration
         ;; (lsp-mode . lsp-enable-which-key-integration)
         )
  :commands lsp-deferred)

(use-package lsp-treemacs
  :ensure t)

(use-package dap-mode
  :ensure t  
  :config
  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (tooltip-mode 1)
  (dap-ui-controls-mode 1)
  (setq dap-print-io t)
  (require 'dap-dlv-go)
  )

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  )

(use-package zig-mode
  :ensure t
  :init
  (add-hook 'zig-mode-hook
            (lambda ()
              (setq compile-command "zig build"))
            )
  )

(use-package rust-mode
  :ensure t
  :init
  (add-hook 'rust-mode-hook
            (lambda () (setq indent-tabs-mode nil)))
  (add-hook 'rust-mode-hook
          (lambda () (prettify-symbols-mode)))
  (setq rust-format-on-save t)
  )

(use-package flycheck
  :ensure t
  )

(use-package multiple-cursors
  :ensure t
  )

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (setq lsp-pyright-venv-path "$VENV_DIR")
                         (lsp-deferred)))
  )

(use-package yaml-mode
  :ensure t
  :init
  (add-hook 'yaml-mode-hook
            (lambda ()
              (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  )

(use-package json-mode
  :ensure t
  )

(use-package go-mode
  :ensure t
  :init
  (add-hook 'before-save-hook 'gofmt-before-save)
  )

(use-package xclip
  :ensure t
  :init
  (setq xclip-mode 1)
  )

(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown"))

(use-package mermaid-mode
  :ensure t
  )

(use-package just-mode
  :ensure t)

(use-package doom-modeline
  :ensure t
  :init
  (doom-modeline-mode 1)
  (setq doom-modeline-project-detection 'projectile)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-color-icon t)
  (setq doom-modeline-env-version t)
  )

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package nerd-icons
  :ensure t
  )

;;(use-package envrc
;;  :hook (after-init . envrc-global-mode))

;;(use-package highlight-indent-guides
;;  :ensure t
;;  :init
;;  (setq highlight-indent-guides-method 'character)
;;  (setq highlight-indent-guides-responsive 'top)
;;  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
;;  )

;;(use-package indent-guide
;;  :ensure t
;;  :init
;;  (setq indent-guide-global-mode t)
;;  )

;; Emacs config
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(wombat))
 '(ispell-dictionary nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(set-face-attribute 'default nil :height 100)
(add-to-list 'default-frame-alist '(alpha 97)) ;; doesnt work on emacs29 and X
(setq custom-file (make-temp-file "emacs-custom"))
(setq backup-directory-alist '(("." . "~/emacsbackup")))

;; Keys
(global-set-key (kbd "C-c f") 'find-name-dired)
(global-set-key (kbd "C-c d") 'dired)
(global-set-key (kbd "C-c g") 'grep-find)
(global-set-key (kbd "C-c t") 'lsp-treemacs-symbols)
(global-set-key (kbd "C-c m") 'mc/edit-lines)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c k") 'kill-all-buffers)
(global-set-key (kbd "C-c r") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-c c") 'compile)

;; Auto Mode Alist

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;; Use a on dired
(put 'dired-find-alternate-file 'disabled nil)
