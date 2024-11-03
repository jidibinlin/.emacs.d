;;; init-snippets.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 jidibinlin
;;
;; Author: jidibinlin <2694273649@qq.com>
;; Maintainer: jidibinlin <2694273649@qq.com>
;; Created: November 02, 2024
;; Modified: November 02, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/qibin/init-snippets
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package tempel
  :bind (:map tempel-map
              ("TAB" . tempel-next)
              ("S-TAB" . tempel-previous))
  :init
  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-complete
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf))

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
  :after tempel
  :ensure t)

(use-package eglot-tempel
  :hook (after-init . eglot-tempel-mode)
  :ensure t
  :init
  (defun tempel-eglot-completion-at-point()
    (cape-wrap-super #'eglot-completion-at-point #'tempel-complete))
  :config
  (defun tweak-eglot-managed-capf ()
    (setq-local completion-at-point-functions
                (remove #'tempel-complete completion-at-point-functions))
    (setq-local completion-at-point-functions
                (remove #'eglot-completion-at-point completion-at-point-functions))
    (push #'tempel-eglot-completion-at-point completion-at-point-functions))
  (add-hook 'eglot-managed-mode-hook #'tweak-eglot-managed-capf))

(provide 'init-snippets)
;;; init-snippets.el ends here
