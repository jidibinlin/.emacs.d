;;; init-font.el --- Description -*- lexical-binding: t; -*-

(defvar conia-font (font-spec :family "Iosevka Comfy Fixed" :size 16))
(set-face-attribute 'default nil :font conia-font)

(use-package nerd-icons
	:ensure t
	:demand t
	:config
	(setf (alist-get 'go-mode  nerd-icons-mode-icon-alist)
				'(nerd-icons-sucicon "nf-seti-go2" :face nerd-icons-blue))
	(setf (alist-get 'go-ts-mode  nerd-icons-mode-icon-alist)
				'(nerd-icons-sucicon "nf-seti-go2" :face nerd-icons-blue))
	(setf (alist-get "go"  nerd-icons-extension-icon-alist)
				'(nerd-icons-sucicon "nf-seti-go2" :face nerd-icons-blue)))

(provide 'init-font)
;;; init-font.el ends here
