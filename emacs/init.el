(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(require 'use-package)


(defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer 
          (delq (current-buffer) 
                (remove-if-not 'buffer-file-name (buffer-list)))))

;; Reload current buffer
(defun revert-buffer-no-confirm ()
    "Revert buffer without confirmation."
    (interactive)
    (revert-buffer :ignore-auto :noconfirm))

(defun kill-all-buffers ()
  "Kill all buffers, leaving only the *scratch* buffer."
  (interactive)
  (mapc 'kill-buffer (buffer-list))
  (switch-to-buffer "*scratch*"))

;; Packages
(use-package use-package-ensure-system-package
  :ensure t)

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
  :bind (:map projectile-mode-map
              ("s-p" . projectile-command-map)
              ("C-c p" . projectile-command-map))
  :hook
  (after-init . projectile-global-mode))

(use-package lsp-mode
  :ensure t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-enable-file-watchers nil)
  (setq lsp-file-watch-threshold 2500)
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (zig-mode . lsp-deferred)
	 (python-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
	 (go-mode . lsp-deferred)
	 (haskell-mode . lsp-deferred)
         ;; if you want which-key integration
         ;; (lsp-mode . lsp-enable-which-key-integration)
	 )
  :commands lsp-deferred)

(use-package lsp-treemacs
  :ensure t)

(use-package zig-mode
  :ensure t
  :init
  (add-hook 'zig-mode-hook
          (lambda ()
            (setq compile-command "zig build"))
  )
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
  )

(use-package haskell-mode
  :ensure t
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

;; Emacs config
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
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

(set-face-attribute 'default nil :height 200)
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
