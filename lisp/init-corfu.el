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
  :hook ((elpaca-after-init . global-corfu-mode)
				 (meow-insert-exit . corfu-quit))
  :bind (:map corfu-map
              ("M-j" . corfu-next)
              ("M-k" . corfu-previous))
  :ensure t
  :init
  (setq corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 2
        corfu-count 10
        corfu-scroll-margin 5
        corfu-max-width 60
        corfu-cycle t
        corfu-preselect 'first
        corfu-on-exact-match nil
        corfu-quit-no-match  'separator)
  :config
  (defun conia/switch-corfu-preselect-prompt-for-eshell ()
    (setq-local corfu-preselect 'prompt)))

(use-package cape
  :defer t
  :ensure t
  :init
  (advice-add #'tempel-complete :around #'cape-wrap-nonexclusive)
  (advice-add #'tempel-expand :around #'cape-wrap-nonexclusive)
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive))

(use-package nerd-icons-corfu
  :defer t
  :after corfu
  :ensure t
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'init-corfu)
;;; init-corfu.el ends here
