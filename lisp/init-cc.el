;;; init-cc.el --- cc -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configuration for C/C++ development using Eglot and tree-sitter
;;
;;; Code:

(use-package eglot-inactive-regions
	:ensure t
  :after eglot
	:hook ((c-ts-mode . eglot-inactive-regions-mode)
				 (c++-ts-mode . eglot-inactive-regions-mode))
	:custom
	(eglot-inactive-regions-style 'darken-foreground)
  (eglot-inactive-regions-opacity 0.4))

(use-package c-ts-mode
	:config
	(add-hook 'c-ts-mode-hook #'eglot-ensure -100))

(use-package c++-ts-mode
	:config
	(add-hook 'c++-ts-mode-hook #'eglot-ensure -100))

(provide 'init-cc)
;;; init-cc.el ends here
