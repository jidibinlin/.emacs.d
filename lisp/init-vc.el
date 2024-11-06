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
  :ensure (:version (lambda (_) "0.7.6")))

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
