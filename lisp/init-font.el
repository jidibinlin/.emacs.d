;;; init-font.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 jidibinlin
;;
;; Author: jidibinlin <2694273649@qq.com>
;; Maintainer: jidibinlin <2694273649@qq.com>
;; Created: November 01, 2024
;; Modified: November 01, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/qibin/init-font
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar conia-font (font-spec :family "Iosevka Comfy Fixed" :size 16))
(defvar conia-variable-pitch-font (font-spec :family "Iosevka Etoile" :size 16))

(set-face-attribute 'default nil :font conia-font)
(set-face-attribute 'variable-pitch nil :font conia-variable-pitch-font)

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
