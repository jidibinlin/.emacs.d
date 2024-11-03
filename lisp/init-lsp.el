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
  :commands eglot eglot-ensure
  :init
  (setq eglot-autoshutdown t)
  :config
  (cl-callf plist-put eglot-events-buffer-config :size 0))

(use-package consult-eglot
  :defer t
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
