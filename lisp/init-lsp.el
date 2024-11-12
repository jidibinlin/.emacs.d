;;; init-lsp.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 jidibinlin
;;
;; Author: jidibinlin <2694273649@qq.com>
;; Maintainer: jidibinlin <2694273649@qq.com>
;; Created: November 02, 2024
;; Modified: November 02, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/qibin/init-lsp
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package eglot
  :init
  (setq eglot-autoshutdown t)
  (setq eglot-send-changes-idle-time 0.05)
  (setq-default eglot-events-buffer-size 0)
	(setq-default eglot-sync-connect 0)
	:demand t
	:config
	(add-to-list 'eglot-stay-out-of 'imenu)
	;; format on save
	(cl-defun conia/format--with-eglot (beg end &key buffer callback &allow-other-keys)
		(with-current-buffer buffer
			(or (with-demoted-errors "%s"
						(always (eglot-format beg end)))
					(ignore (funcall callback)))))
	(cl-defun conia/apheleia-formatter-eglot
			(&rest plist &key buffer callback &allow-other-keys)
		(conia/format--with-eglot nil nil :buffer buffer plist))

	(defun conia/enable-eglot-format-onsave ()
		(setq-local apheleia-formatter 'eglot))
	(add-hook 'eglot--managed-mode-hook #'conia/enable-eglot-format-onsave)

	(defun conia/eglot--register-apheleia-formatter()
		(add-to-list 'apheleia-formatters
								 '(eglot . conia/apheleia-formatter-eglot)))
	(add-hook 'elpaca-after-init-hook  #'conia/eglot--register-apheleia-formatter))

(use-package consult-eglot
  :ensure t)

(when (< emacs-major-version 30)
	(use-package eglot-booster
		:ensure (:host github :repo "jdtsmith/eglot-booster")
		:after eglot
		:hook (elpaca-after-init . eglot-booster-mode)))

(provide 'init-lsp)
;;; init-lsp.el ends here
