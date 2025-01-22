;;; init-vc.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 jidibinlin
;;
;; Author: jidibinlin <2694273649@qq.com>
;; Maintainer: jidibinlin <2694273649@qq.com>
;; Created: November 01, 2024
;; Modified: November 01, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/qibin/init-vc
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package transient
  :ensure t)

(use-package magit
  :ensure t
	:hook ((magit-pre-refresh . diff-hl-magit-pre-refresh)
				 (magit-post-refresh . diff-hl-magit-post-refresh))
	:custom-face
	(magit-header-line ((t (:inherit header-line
																	 :foreground unspecified
																	 :background unspecified
																	 :box unspecified)))))

(use-package vc
  :defer t
  :ensure nil
  :custom-face
  (vc-up-to-date-state ((t (:inherit font-lock-keyword-face :bold t))))
  (vc-edited-state ((t (:inherit font-lock-warning-face :bold t))))
  (vc-conflict-state ((t (:inherit error :bold t)))))

(use-package emsg-blame
	:ensure (:host github :repo "ISouthRain/emsg-blame")
  :hook (elpaca-after-init . global-emsg-blame-mode)
  :config
  (defun my--emsg-blame-display ()
    "Display git blame message, right-aligned with Magit-style faces.
If another message is already being displayed, display both messages unless they
do not both fit in the echo area."
    (let* ((message-log-max nil) ; prevent messages from being logged to *Messages*
           (cur-msg (or (current-message) ""))
           (blm-msg (format "%s %s %s "
														emsg-blame--commit-summary
														(propertize emsg-blame--commit-author 'face 'magit-log-author)
														(propertize emsg-blame--commit-date 'face 'magit-log-date)))
           (available-width (max 0 (- (frame-width) (string-width cur-msg) 1)))
           (blm-msg-width (string-width blm-msg))
           (padding (max 0 (- available-width blm-msg-width)))
           (rev-blm-msg (concat (make-string padding ?\s) blm-msg)))
      (if (> blm-msg-width available-width)
					(message blm-msg)
        (message (concat cur-msg rev-blm-msg)))))

  (setq emsg-blame-display #'my--emsg-blame-display))

(use-package ediff
	:init
	(setq-default ediff-diff-options "-w"
								ediff-split-window-function #'split-window-horizontally
								ediff-window-setup-function #'ediff-setup-windows-plain))

(use-package diff-hl
	:ensure t
	:hook (elpaca-after-init . global-diff-hl-mode)
	:custom
	(diff-hl-draw-borders nil)
	:config
	;; shiped from doom emacs
	(defun conia/pretty-diff-hl-fringe (&rest _)
		(let* ((scale (if (and (boundp 'text-scale-mode-amount)
													 (numberp text-scale-mode-amount))
											(expt text-scale-mode-step text-scale-mode-amount)
										1))
					 (spacing (or (and (display-graphic-p) (default-value 'line-spacing)) 0))
					 (h (+ (ceiling (* (frame-char-height) scale))
								 (if (floatp spacing)
										 (truncate (* (frame-char-height) spacing))
									 spacing)))
					 (w (min (frame-parameter nil (intern (format "%s-fringe" diff-hl-side)))
									 16))
					 (_ (if (zerop w) (setq w 16))))

			(define-fringe-bitmap 'diff-hl-bmp-middle
				(make-vector
				 h (string-to-number (let ((half-w (1- (/ w 2))))
															 (concat (make-string half-w ?1)
																			 (make-string (- w half-w) ?0)))
														 2))
				nil nil 'center)))
	
	(advice-add #'diff-hl-define-bitmaps
							:after #'conia/pretty-diff-hl-fringe)
	
	(defun conia/diff-hl-type-at-pos-fn (type _pos)
		'diff-hl-bmp-middle)
	
	(setq diff-hl-fringe-bmp-function #'conia/diff-hl-type-at-pos-fn)
	(defun conia/diff-hl-fringe-pretty(_)
		(set-face-attribute 'diff-hl-insert nil :background 'unspecified :inherit nil)
		(set-face-attribute 'diff-hl-delete nil :background 'unspecified :inherit nil)
		(set-face-attribute 'diff-hl-change nil :background 'unspecified :inherit nil))
	(add-to-list 'after-make-frame-functions
							 #'conia/diff-hl-fringe-pretty)
	(add-to-list 'enable-theme-functions #'conia/diff-hl-fringe-pretty))

(provide 'init-vc)
;;; init-vc.el ends here
