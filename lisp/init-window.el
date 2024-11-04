;;; init-window.el --- config for emacs window -*- lexical-binding: t -*-

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

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
	(setq popper-group-function #'popper-group-by-project)
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1))

(use-package perspective
	:ensure t
  :bind
  ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c C-w"))  ; pick your own prefix key here
  :hook(elpaca-after-init . persp-mode))

(use-package transient-posframe
  :diminish
  :custom-face
  (transient-posframe ((t (:inherit tooltip))))
  :hook (elpaca-after-init . transient-posframe-mode)
	:ensure t
  :init
  (setq transient-posframe-border-width 1
				transient-posframe-min-height nil
				transient-posframe-min-width 80
				transient-posframe-poshandler 'posframe-poshandler-frame-center
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
																				 (1+ (count-screen-lines (point-min) (point-max))))
															 :font transient-posframe-font
															 :position (point)
															 :poshandler transient-posframe-poshandler
															 :background-color (face-attribute 'transient-posframe
																																 :background nil t)
															 :foreground-color (face-attribute 'transient-posframe
																																 :foreground nil t)
															 :min-width transient-posframe-min-width
															 :min-height transient-posframe-min-height
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
