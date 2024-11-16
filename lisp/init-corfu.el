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
				 (meow-insert-exit . corfu-quit)
				 (eshell-mode . conia/switch-corfu-preselect-prompt)
				 (minibuffer-mode . conia/switch-corfu-preselect-prompt))
  :bind (:map corfu-map
              ("M-j" . corfu-next)
              ("M-k" . corfu-previous))
  :ensure t
  :init
  (setq corfu-auto t
        corfu-auto-delay 0.025
        corfu-auto-prefix 2
        corfu-count 10
        corfu-scroll-margin 5
        corfu-max-width 60
        corfu-cycle t
        corfu-preselect 'first
        corfu-on-exact-match nil
        corfu-quit-no-match  t
				corfu-quit-at-boundary 'separator)
  :config
  (defun conia/switch-corfu-preselect-prompt ()
    (setq-local corfu-preselect 'prompt)))

(use-package cape
  :defer t
  :ensure t
  :init
  (advice-add #'tempel-complete :around #'cape-wrap-nonexclusive)
  (advice-add #'tempel-expand :around #'cape-wrap-nonexclusive)
	(advice-add #'elisp-completion-at-point :around #'cape-wrap-nonexclusive)
	(advice-add #'codeium-completion-at-point :around #'cape-wrap-nonexclusive)

	(defvar-local conia/supered-capfs nil)

	(defun conia/super-capf ()
		(apply #'cape-wrap-super conia/supered-capfs))

	(defun conia/merge-capf ()
		(let ((tosuper '()))
			(dolist (mode-capf conia/capfs-to-merge)
				(let ((enmode (car mode-capf))
							(capf (cdr mode-capf)))
					(when (or
								 (and (boundp enmode) (symbol-value enmode))
								 (derived-mode-p enmode))
						(setq-local completion-at-point-functions
												(remove capf completion-at-point-functions))
						(when (not (member capf tosuper))
							(cl-pushnew capf tosuper)))))
			
			;; sort tosuper by conia/capfs-priority
			(setq-local conia/supered-capfs
									(sort tosuper
												(lambda (a b)
													(> (or (alist-get a conia/capfs-priority) 0)
														 (or (alist-get b conia/capfs-priority) 0)))))
			
			(add-to-list 'completion-at-point-functions
									 #'conia/super-capf))))

(use-package nerd-icons-corfu
  :defer t
  :after corfu
  :ensure t
  :init
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'init-corfu)
;;; init-corfu.el ends here
