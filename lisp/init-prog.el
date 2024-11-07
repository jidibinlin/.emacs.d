;;; init-prog.el --- for progmode -*- lexical-binding: t -*-

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

(use-package elec-pair
  :hook (prog-mode . electric-pair-mode)
  ;; {{ @see https://debbugs.gnu.org/cgi/bugreport.cgi?bug=55340
  ;; `new-line-indent` disables `electric-indent-mode'
  :config
  (defun my-electric-pair-open-newline-between-pairs-psif-hack (orig-func &rest args)
    (ignore orig-func args)
    (when (and (if (functionp electric-pair-open-newline-between-pairs)
									 (funcall electric-pair-open-newline-between-pairs)
                 electric-pair-open-newline-between-pairs)
							 (eq last-command-event ?\n)
							 (< (1+ (point-min)) (point) (point-max))
							 (eq (save-excursion
										 (skip-chars-backward "\t\s")
										 (char-before (1- (point))))
									 (matching-paren (char-after))))
      (save-excursion (newline-and-indent 1))))
  (advice-add 'electric-pair-open-newline-between-pairs-psif
							:around
							#'my-electric-pair-open-newline-between-pairs-psif-hack)
  ;; }}
  (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package eldoc-box
  :ensure (:host github :repo "jidibinlin/eldoc-box")
  :hook (eldoc-mode . eldoc-box-hover-at-point-mode)
  :diminish (eldoc-box-hover-at-point-mode eldoc-box-hover-mode)
  :init
  (setq-default eldoc-idle-delay 1)
  :custom-face
  (eldoc-box-border ((t (:inherit region :background unspecified))))
  (eldoc-box-body ((t (:inherit tooltip))))
  :config
  (setq eldoc-box-max-pixel-width 600)
  (setq eldoc-box-max-pixel-height 200)

  (defun my/eldoc-box-scroll-up ()
    "Scroll up in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-down 3))))

  (defun my/eldoc-box-scroll-down ()
    "Scroll down in `eldoc-box--frame'"
    (interactive)
    (with-current-buffer eldoc-box--buffer
      (with-selected-frame eldoc-box--frame
        (scroll-up 3))))

  (add-to-list 'eldoc-box-self-insert-command-list #'my/eldoc-box-scroll-down)
  (add-to-list 'eldoc-box-self-insert-command-list #'my/eldoc-box-scroll-up)

  (define-key prog-mode-map (kbd "M-n") #'my/eldoc-box-scroll-down)
  (define-key prog-mode-map (kbd "M-p") #'my/eldoc-box-scroll-down)
	(defun conia/eldoc-box-frame-pretty(_)
		(set-face-attribute 'eldoc-box-border nil :inherit 'region :background 'unspecified)
		(set-face-attribute 'eldoc-box-body nil :inherit 'tooltip))
	(add-to-list 'enable-theme-functions #'conia/eldoc-box-frame-pretty)
	(add-to-list 'after-make-frame-functions #'conia/eldoc-box-frame-pretty))

(use-package flymake
  :hook (prog-mode . flymake-mode)
  :init
  (defconst +flymake-fringe-bitmap-double-arrow-right
    [#b11011000
		 #b01101100
		 #b00110110
		 #b00011011
		 #b00110110
		 #b01101100
		 #b11011000])

  (defconst +flymake-fringe-bitmap-double-arrow-left
    [#b00011011
		 #b00110110
		 #b01101100
		 #b11011000
		 #b01101100
		 #b00110110
		 #b00011011])

  (define-fringe-bitmap
    '+flymake-fringe-bitmap-double-arrow-left
    +flymake-fringe-bitmap-double-arrow-left)

  (define-fringe-bitmap
    '+flymake-fringe-bitmap-double-arrow-right
    +flymake-fringe-bitmap-double-arrow-right)

  :hook (flymake-mode . +pretty-flymake-fringe-indicator)
  :config
  (setq flymake-fringe-indicator-position 'right-fringe)
  (defun +pretty-flymake-fringe-indicator(&rest _)
    (let ((bitmap (if (eq flymake-fringe-indicator-position 'right-fringe)
											'+flymake-fringe-bitmap-double-arrow-left
                    '+flymake-fringe-bitmap-double-arrow-right)))
      (setq flymake-error-bitmap `(,bitmap compilation-error))
      (setq flymake-warning-bitmap `(,bitmap compilation-warning))
      (setq flymake-note-bitmap `(,bitmap compilation-info))))
	(add-to-list 'enable-theme-functions #'+pretty-flymake-fringe-indicator))

(use-package apheleia
  :ensure t
	:demand t
  :hook (elpaca-after-init . apheleia-global-mode))

(use-package rainbow-delimiters
	:ensure t
	:hook (prog-mode . rainbow-delimiters-mode)
	:custom
	(rainbow-delimiters-max-face-count 4))

(use-package treesit-fold
	:ensure (:host github :repo "emacs-tree-sitter/treesit-fold")
	:after prog-mode
	:hook ((elpaca-after-init . global-treesit-fold-mode)))

(use-package protobuf-mode
	:ensure t)

(use-package markdown-mode
	:ensure t
	:custom-face
	(markdown-code-face ((t (:inherit default :background unspecified :foreground unspecified)))))

(provide 'init-prog)
;;; init-prog.el ends here
