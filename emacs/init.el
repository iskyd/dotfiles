(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'exec-path "~/dev/zls/zig-out/bin") ;; Zls path
(require 'use-package)
(require 'lsp-mode)

(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (zig-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp-deferred)
(use-package lsp-ui :commands lsp-ui-mode)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(setq lsp-ui-doc-show-with-cursor t)
(setq lsp-ui-doc-delay 1.0)

(global-set-key (kbd "C-c z") 'zig-test-buffer)
(global-set-key (kbd "C-c f") 'find-name-dired)
(global-set-key (kbd "C-c d") 'dired)

;; Keep in mind that all of these packages are loaded at startup, even if you
;; do not configure them.
(setq package-selected-packages
      '(zig-mode
        magit
	telega ;; Pull telega server, docker pull zevlg/telega-server:latest
	python-mode
	))
;; (setq lsp-modeline-diagnostics-enable t)
(setq telega-use-docker t)
(setq inhibit-startup-screen t)
(menu-bar-mode 0)
(tool-bar-mode 0)

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

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook
          (lambda ()
            (define-key yaml-mode-map "\C-m" 'newline-and-indent)))

(use-package elpy
  :ensure t
  :init
  (elpy-enable))
