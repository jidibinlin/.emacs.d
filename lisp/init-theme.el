;;; init-theme.el init-theme -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 jidibinlin
;;
;; Author: jidibinlin <2694273649@qq.com>
;; Maintainer: jidibinlin <2694273649@qq.com>
;; Created: November 01, 2024
;; Modified: November 01, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/qibin/init-theme
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(use-package doom-themes
	:ensure t)

(use-package nano-theme
	:ensure (:host github :repo "rougier/nano-theme"))

;; use-package with Elpaca:
(use-package dashboard
  :ensure t
	:init
	(setq-default dashboard-center-content t)
	(setq dashboard-items '((recents . 5)
													(projects . 5)
													(agenda . 5)
													(registers . 5)))
	(setq dashboard-item-shortcuts '((recents . "r")
																	 (projects . "p")
																	 (agenda . "a")
																	 (registers . "e")))
	(setq-default dashboard-display-icons-p t)
	(setq-default dashboard-icon-type 'nerd-icons)
	(setq-default dashboard-set-heading-icons t)
	(setq-default dashboard-set-file-icons t)
	:config
	(add-hook 'elpaca-after-init-hook #'dashboard-insert-startupify-lists)
	(add-hook 'elpaca-after-init-hook #'dashboard-initialize)
	(dashboard-setup-startup-hook)
	(remove-hook #'after-init-hook #'dashboard-insert-startupify-lists)
	(remove-hook #'emacs-startup-hook #'dashboard-initialize))

(use-package page-break-lines
	:ensure t
	:hook (elpaca-after-init . global-page-break-lines-mode)
	:config
	(add-to-list 'page-break-lines-modes 'prog-mode)
	(add-to-list 'page-break-lines-modes 'fundamental-mode))

(defvar choosed-theme 'nano-light)

(defun conia/load-theme (&optional theme)
	(interactive)
	(if theme
			(load-theme theme :no-confirm)
		(load-theme choosed-theme :no-confirm)))

(add-hook 'elpaca-after-init-hook #'conia/load-theme)

(provide 'init-theme)
;;; init-theme.el ends here
