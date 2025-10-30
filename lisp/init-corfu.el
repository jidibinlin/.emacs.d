;;; init-corfu.el --- Description -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Corfu completion framework configuration with CAPF integration
;;
;;; Code:

(use-package corfu
  :hook ((after-init . global-corfu-mode)
				 (eshell-mode . conia/switch-corfu-preselect-prompt)
				 (minibuffer-mode . conia/switch-corfu-preselect-prompt))
  :bind (:map corfu-map
              ("M-j" . corfu-next)
              ("M-k" . corfu-previous))
  :init
  (setq corfu-auto t
        corfu-auto-delay 0.05
        corfu-auto-prefix 0
        corfu-count 10
        corfu-scroll-margin 5
        corfu-max-width 60
        corfu-cycle t
        corfu-preselect 'first
        corfu-on-exact-match nil
        corfu-quit-no-match  t
				corfu-quit-at-boundary 'separator)
  (defun conia/switch-corfu-preselect-prompt ()
    (setq-local corfu-preselect 'prompt)))

(use-package corfu-terminal
	:demand t
	:hook ((after-init . my/corfu-terminal-mode-after-init))
	:init
	(defun my/corfu-terminal-mode-after-init ()
		(unless (posframe-workable-p)
			(corfu-terminal-mode +1))))

(use-package cape
  :demand t
  :init
	(advice-add #'elisp-completion-at-point :around #'cape-wrap-nonexclusive)

	(add-to-list 'conia/capfs-to-merge (cons 'emacs-lisp-mode #'elisp-completion-at-point))
	(add-to-list 'conia/capfs-priority '(elisp-completion-at-point . 0))

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
	:demand t
	:after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(provide 'init-corfu)
;;; init-corfu.el ends here
