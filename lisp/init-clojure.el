;;; init-clojure.el --- summary -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Clojure development setup with CIDER and tree-sitter support
;;
;;; Code:

(use-package eglot-java
  :hook (java-mode . eglot-java-mode))

(use-package jarchive
  :hook ((java-mode java-ts-mode) . jarchive-mode))

(use-package clojure-ts-mode
	:hook (((clojure-ts-mode clojure-mode) . jarchive-mode)
				 (clojure-ts-mode . rainbow-delimiters-mode)
				 (clojure-ts-mode . eglot-ensure))
	:config
	(with-eval-after-load 'eglot
		(add-to-list 'eglot-server-programs
								 '((clojure-ts-mode) . ("clojure-lsp")))))

(use-package cider
	:hook ((cider-repl-mode . toggle-truncate-lines)
				 (cider-mode . conia/cider--disable-completion))
	:config
	(defun conia/cider--disable-completion()
		(remove-hook 'completion-at-point-functions
								 #'cider-completion-at-point t)))

(provide 'init-clojure)
;;; init-clojure.el ends here
