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

(use-package rime
	:demand t
	:hook ((kill-emacs . rime-lib-finalize)
				 (after-init . conia/enable-rime))
	:custom
	(default-input-method "rime")
	:init
	(defun conia/enable-rime ()
		(rime-activate nil)
		(rime-lib-select-schema "luna_pinyin_simp"))
	:config
	(when (posframe-workable-p)
		(setq rime-show-candidate 'posframe))
	(add-to-list 'rime-disable-predicates 'evil-normal-state-p))

(use-package keycast)

(use-package visual-replace
	:hook (after-init . visual-replace-global-mode))

(use-package dumb-jump
	:config
	(add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package pdf-tools)

(use-package eat
	:demand t
	:hook((after-init . conia/pretty-hydra-define-eat))
	:custom
	(eat-kill-buffer-on-exit t)
	:init
	(defun conia/pretty-hydra-define-eat ()
		(pretty-hydra-define+
			toggles-hydra ()
			("Project"
			 (("p t" eat "open terminal" :exit t)))))
	:config
	(add-to-list 'evil-emacs-state-modes 'eat-mode)
	(setq evil-insert-state-modes (remove 'eat-mode evil-insert-state-modes)))

(provide 'init-utils)
;;; init-utils.el ends here
