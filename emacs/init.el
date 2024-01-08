(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'exec-path "~/dev/zls/zig-out/bin") ;; Zls path
(require 'use-package)

(defun zig-test-buffer-2 ()
  "Test buffer using `zig test`."
  (interactive)
  (zig--run-cmd "test" (file-local-name (buffer-file-name)) "--main-mod-path" projectile-project-root "-O" zig-test-optimization-mode))

;; Packages
(use-package use-package-ensure-system-package
  :ensure t)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/dev/" "/opt/projects/Conio/"))
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
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (zig-mode . lsp-deferred)
	 (python-mode . lsp-deferred)
	 (c-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :init
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-delay 1.0))

(use-package lsp-treemacs
  :ensure t)

(use-package dap-mode
  :ensure t
  :config
  (require 'dap-lldb)) ;; M-x dap-cpptools-setup

(use-package zig-mode
  :ensure t
  :ensure-system-package ("~/dev/zls" . "git clone https://github.com/zigtools/zls ~/dev/zls && cd ~/dev/zls && zig build -Doptimize=ReleaseSafe"))

(use-package flycheck
  :ensure t)

(use-package multiple-cursors
  :ensure t)

;;(use-package envrc
;;   :ensure t
;;   :config
;;   (envrc-global-mode))
;;
(use-package python-mode
  :ensure t
  :preface 
  (defun projectile-set-lsp-venv ()
  "Set pyenv version matching project name."
  (let ((project (projectile-project-name)))
    (if (file-exists-p (concat (projectile-project-root) "venv/bin/pylsp"))
	(progn
	  (setq lsp-pylsp-server-command (concat (projectile-project-root) "venv/bin/pylsp"))
	  (lsp-workspace-restart))
      (progn
	(setq lsp-pylsp-server-command "~/.local/bin/pylsp")
	(lsp-workspace-restart))
      )
  ))
  :init
  ;;(setq lsp-pylsp-server-command "/opt/projects/Conio/bitscrooge/venv/bin/pylsp")
  (setq lsp-pylsp-server-command "~/.local/bin/pylsp")
  (add-hook 'projectile-after-switch-project-hook #'projectile-set-lsp-venv)
  )

;;(use-package virtualenvwrapper
;;  :ensure t
;;  ;;:config
;;  ;;(pyenv-mode t)
;;  :preface
;;  (defun projectile-pyenv-mode-set ()
;;  "Set pyenv version matching project name."
;;  (let ((project (projectile-project-name)))
;;    (setq venv-location (concat projetc "venv/"))
;;    ))
;;  :init
;;  (add-hook 'projectile-after-switch-project-hook 'projectile-pyenv-mode-set)
;;  )
;;  
;; To run python inside a container using lsp-mode we're using direnv and https://github.com/snbuback/container-env
;;(use-package direnv
;;  :ensure t
;;  :init
;;  (setq direnv-always-show-summary t)
;;  :config
;;  (direnv-mode)
;;  :ensure-system-package
;;  (direnv))

(use-package yaml-mode
  :init
  (add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))
  )

(use-package telega
  :ensure nil
  :init
  (setq telega-use-docker t))

;; Keep in mind that all of these packages are loaded at startup, even if you
;; do not configure them.
;;(setq package-selected-packages
;;      '(zig-mode
;;	python-mode
;;	magit
;;	lsp-mode
;;	flycheck
;;	))
;;

;; Emacs config
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)
(global-linum-mode t)

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

(setq custom-file (make-temp-file "emacs-custom"))

;; Keys
(global-set-key (kbd "C-c z") 'zig-test-buffer-2)
(global-set-key (kbd "C-c f") 'find-name-dired)
(global-set-key (kbd "C-c d") 'dired)
(global-set-key (kbd "C-c g") 'grep-find)
(global-set-key (kbd "C-c t") 'lsp-treemacs-symbols)
(global-set-key (kbd "C-c m") 'mc/edit-lines)

;; Auto Mode Alist
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.zig\\'" . zig-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
