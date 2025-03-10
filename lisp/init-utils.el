;;; init-utils.el --- summary -*- lexical-binding: t -*-

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

(require 'envir)

(use-package makefile-executor
	:ensure t
	:hook (makefile-mode . makefile-executor-mode)
	:demand t
	:config
	(defun +make/run ()
		"Run a make task in the current project. If multiple makefiles are available,
you'll be prompted to select one."
		(interactive)
		(if (project-current)
				(makefile-executor-execute-project-target)
			(let ((makefile (cl-loop with buffer-file = (or buffer-file-name default-directory)
															 for file in (list "Makefile" "makefile")
															 if (locate-dominating-file buffer-file file)
															 return file)))
				(unless makefile
					(user-error "Cannot find a makefile in the current project"))
				(let ((default-directory (file-name-directory makefile)))
					(makefile-executor-execute-target makefile))))))

(use-package rime
	:ensure t
	:demand t
	:bind  (:map rime-active-mode-map
							 ("<escape>" . nil))
	:custom
	(default-input-method "rime")
	(rime-show-candidate 'posframe)
	:config
	(add-to-list 'rime-disable-predicates 'meow-normal-mode-p)
	(defun conia/enable-rime ()
		(rime-activate nil)
		(rime-lib-select-schema "luna_pinyin_simp"))
	(add-hook 'elpaca-after-init-hook #'conia/enable-rime))

(use-package keycast
	:ensure t)

(use-package visual-replace
	:ensure t
	:hook (elpaca-after-init . visual-replace-global-mode))

(use-package dumb-jump
	:ensure t
	:config
	(add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package pdf-tools
	:ensure t)

(provide 'init-utils)
;;; init-utils.el ends here
