(require 'package)
(package-initialize)
(require 'use-package)

(use-package multiple-cursors
  :ensure t)

(use-package xclip
  :ensure t
  :init
  (setq xclip-mode 1)
  )

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
(add-to-list 'default-frame-alist '(alpha 97))
(setq custom-file (make-temp-file "emacs-custom"))

(global-set-key (kbd "C-c r") 'revert-buffer-no-confirm)
(global-set-key (kbd "C-c k") 'kill-all-buffers)
(global-set-key (kbd "C-c m") 'mc/edit-lines)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-c d") 'dired)
