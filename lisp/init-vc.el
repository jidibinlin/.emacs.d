;;; init-vc.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 jidibinlin
;;
;; Author: jidibinlin <2694273649@qq.com>
;; Maintainer: jidibinlin <2694273649@qq.com>
;; Created: November 01, 2024
;; Modified: November 01, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/qibin/init-vc
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package transient
  :ensure (:version (lambda (_) "0.7.6")))

(use-package magit
  :ensure t)

(use-package vc
  :defer t
  :ensure nil
  :custom-face
  (vc-up-to-date-state ((t (:inherit font-lock-keyword-face :bold t))))
  (vc-edited-state ((t (:inherit font-lock-warning-face :bold t))))
  (vc-conflict-state ((t (:inherit error :bold t)))))

(use-package ediff
	:init
	(setq-default ediff-diff-options "-w"
								ediff-split-window-function #'split-window-horizontally
								ediff-window-setup-function #'ediff-setup-windows-plain))

(provide 'init-vc)
;;; init-vc.el ends here
