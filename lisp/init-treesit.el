;;; init-treesit.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 jidibinlin
;;
;; Author: jidibinlin <2694273649@qq.com>
;; Maintainer: jidibinlin <2694273649@qq.com>
;; Created: November 01, 2024
;; Modified: November 01, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/qibin/init-treesit
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(use-package treesit-auto
  :ensure t
  :demand t
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(provide 'init-treesit)
;;; init-treesit.el ends here
