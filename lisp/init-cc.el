;;; init-cc.el --- cc -*- lexical-binding: t -*-

;; Author: 
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
