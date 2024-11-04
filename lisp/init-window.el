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

(provide 'init-window)
;;; init-window.el ends here
