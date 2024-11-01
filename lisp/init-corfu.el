;;; init-corfu.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 jidibinlin
;;
;; Author: jidibinlin <2694273649@qq.com>
;; Maintainer: jidibinlin <2694273649@qq.com>
;; Created: November 01, 2024
;; Modified: November 01, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/qibin/init-corfu
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package corfu
  :hook (after-init . global-corfu-mode)
  :ensure t
  :config
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 2
        corfu-count 5
        corfu-scroll-margin 5
        corfu-max-width 45
        corfu-cycle t
        corfu-preselect 'first
        corfu-on-exact-match nil
        corfu-quit-no-match  corfu-quit-at-boundary))


(provide 'init-corfu)
;;; init-corfu.el ends here
