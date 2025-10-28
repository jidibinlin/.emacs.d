;;; init-ai.el --- ai -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configuration for AI-related packages like GPTel, Copilot, etc.
;;
;;; Code:

(use-package evil
  :demand t
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :demand t
  :config
  (evil-collection-init))

(provide 'init-evil)
