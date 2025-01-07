;;; init-clojure.el --- summary -*- lexical-binding: t -*-

;; Author: qibin
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; commentary

;;; Code:

(use-package eglot-java
	:ensure t
  :hook (java-mode . eglot-java-mode))

(use-package jarchive
	:ensure t
  :hook ((java-mode java-ts-mode) . jarchive-mode))

(use-package clojure-ts-mode
	:ensure t
	:hook (((clojure-ts-mode clojure-mode) . jarchive-mode)
				 (clojure-ts-mode . rainbow-delimiters-mode)
				 (clojure-ts-mode . eglot-ensure))
	:config
	(with-eval-after-load 'eglot
		(add-to-list 'eglot-server-programs
								 '((clojure-ts-mode) . ("clojure-lsp")))))

(use-package cider
	:ensure t
	:hook ((cider-repl-mode . toggle-truncate-lines)
				 (cider-mode . conia/cider--disable-completion))
	:config
	(defun conia/cider--disable-completion()
		(remove-hook 'completion-at-point-functions
								 #'cider-completion-at-point t)))

(provide 'init-clojure)
;;; init-clojure.el ends here
