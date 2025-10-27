;;; init-utils.el --- summary -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Utility packages and miscellaneous configurations
;;
;;; Code:

(require 'envir)

(use-package makefile-executor
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

;; (use-package rime
;; 	:demand t
;; 	:bind  (:map rime-active-mode-map
;; 							 ("<escape>" . nil))
;; 	:hook (kill-emacs . rime-lib-finalize)
;; 	:custom
;; 	(default-input-method "rime")
;; 	(rime-show-candidate 'posframe)
;; 	:config
;; 	(add-to-list 'rime-disable-predicates 'meow-normal-mode-p)
;; 	(defun conia/enable-rime ()
;; 		(rime-activate nil)
;; 		(rime-lib-select-schema "luna_pinyin_simp"))
;; 	(add-hook 'after-init-hook #'conia/enable-rime))

(use-package keycast)

(use-package visual-replace
	:hook (after-init . visual-replace-global-mode))

(use-package dumb-jump
	:config
	(add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package pdf-tools)

(provide 'init-utils)
;;; init-utils.el ends here
