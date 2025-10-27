;;; init-window.el --- config for emacs window -*- lexical-binding: t -*-

;;; Commentary:
;;
;; commentary
;;
;;; Code:

(use-package popper
  :demand t
  :hook ((after-init . popper-mode)
         ;(after-init . popper-echo-mode)
		 )
  :bind (("C-`"   . popper-toggle)
				 ("M-`"   . popper-cycle)
				 ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-group-function #'popper-group-by-project)
  (setq popper-reference-buffers
				'("\\*Messages\\*"
					"Output\\*$"
					"\\*Async Shell Command\\*"
					"\\*Warnings\\*"
					help-mode
					compilation-mode)))

(use-package winum
	:demand t
	:custom
	(winum-auto-setup-mode-line nil)
	:hook ((after-init . winum-mode))
	:custom-face
	(winum-face ((t (:inherit font-lock-keyword-face))))
	:config
	(defun conia/winum-icon (num)
		(if (or (string= "" num)
						(string= "0" num))
				""
			(let* ((format-str "nf-md-numeric_%s_circle_outline")
						 (icon-str (format format-str num))
						 (icon (nerd-icons-mdicon
										icon-str
										:face "winum-face" :v-adjust 0.16)))
				icon)))

	(defvar conia/winum--mode-line-segment
		'(:eval (format "%s"
										(thread-first (winum-get-number-string)
																	(conia/winum-icon)))))

	(defun conia/winum-append-header-line-format ()
		(setq-default header-line-format
									(add-to-list 'header-line-format conia/winum--mode-line-segment)))

	(add-hook 'after-init-hook #'conia/winum-append-header-line-format 100)

	(pretty-hydra-define
		toggles-window
		(:title (pretty-hydra-title "Windowd/Workspace" 'mdicon "nf-md-microsoft_windows")
						:color amaranth :quit-key ("q" "C-g" "ESC"))
		("split/delete"
		 (("v" split-window-right "split window vertico" :exit t)
			("h" split-window-below "split window horizontal" :exit t)
			("d" delete-window "close current window" :exit t)
			("o" other-window "other window")
			("i" delete-other-windows "del other window" :exit t)
			("=" balance-windows "balance windows" :exit t))
		 "jump"
		 (("1" winum-select-window-1 "jump to win1" :exit t)
			("2" winum-select-window-2 "jump to win2" :exit t)
			("3" winum-select-window-3 "jump to win3" :exit t)
			("4" winum-select-window-4 "jump to win4" :exit t)
			("5" winum-select-window-5 "jump to win5" :exit t)))))

(use-package persp-mode
	:hook((kill-emacs . persp-save-state)
				(after-init . persp-mode))
	:custom
	(persp-mode-prefix-key (kbd "C-c C-w"))
	:config
	(setq persp-autokill-buffer-on-remove 'kill-weak
				persp-reset-windows-on-nil-window-conf nil
				persp-nil-hidden t
				persp-auto-save-fname "autosave"
				persp-save-dir (concat user-emacs-directory "workspace/")
				persp-set-last-persp-for-new-frames t
				persp-switch-to-added-buffer nil
				persp-kill-foreign-buffer-behaviour 'kill
				persp-remove-buffers-from-nil-persp-behaviour nil
				persp-auto-resume-time -1)

	(defun conia/pretty-hydra-define-persp ()
		(pretty-hydra-define+
			toggles-window ()
			("persp"
			 (("w" persp-switch "switch workspace" :exit t)))))

	(add-hook 'persp-mode-hook #'conia/pretty-hydra-define-persp 100))

(use-package transient-posframe
	:diminish
	:custom-face
	(transient-posframe ((t (:inherit tooltip))))
	:hook (after-init . transient-posframe-mode)
	:after (posframe)
	:init
	(setq transient-posframe-border-width 1
				transient-posframe-poshandler 'posframe-poshandler-frame-near-top-center
				transient-posframe-parameters '((left-fringe . 8)
																				(right-fringe . 8)))
	:config
	(with-no-warnings
		;; FIXME:https://github.com/yanghaoxie/transient-posframe/issues/5#issuecomment-1974871665
		(defun conia/transient-posframe--show-buffer (buffer _alist)
			"Show BUFFER in posframe and we do not use _ALIST at this period."
			(when (posframe-workable-p)
				(let* ((posframe
								(posframe-show buffer
															 :height (with-current-buffer buffer
																				 (count-screen-lines (point-min) (point-max)))
															 :font transient-posframe-font
															 :position (point)
															 :poshandler transient-posframe-poshandler
															 :background-color (face-attribute 'transient-posframe
																																 :background nil t)
															 :foreground-color (face-attribute 'transient-posframe
																																 :foreground nil t)
															 :min-width 80
															 :internal-border-width transient-posframe-border-width
															 :internal-border-color (face-attribute 'transient-posframe-border
																																			:background nil t)
															 :override-parameters transient-posframe-parameters)))
					(frame-selected-window posframe))))
		(advice-add #'transient-posframe--show-buffer
								:override #'conia/transient-posframe--show-buffer)

		(defun conia/transient-posframe--hide ()
			"Hide transient posframe."
			(posframe-hide transient--buffer-name))
		(advice-add #'transient-posframe--delete
								:override #'conia/transient-posframe--hide)))

(provide 'init-window)
;;; init-window.el ends here
