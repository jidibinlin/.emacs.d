;;; init-org.el --- org -*- lexical-binding: t -*-

;; Author: yangqibin
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

(use-package org-modern
	:ensure t
  :custom
  (org-pretty-entities t)
  (org-modern-table nil)
  (org-tags-column 0)
  (org-insert-heading-respect-content t)
  (org-catch-invisible-edits 'show-and-error)
  (org-modern-block-fringe 1)
	(org-modern-star 'Replace)
  :hook ((org-mode . org-modern-mode)
         (org-agenda-finalize . org-modern-agenda))
  :config
	(set-face-attribute 'org-modern-symbol nil :family "Iosevka Etoile")
  (defun disable-org-block (_)
    (set-face-attribute 'org-block nil :foreground 'unspecified
												:background 'unspecified :inherit 'unspecified)
    (set-face-attribute 'org-block-begin-line nil :foreground 'unspecified
												:background 'unspecified)
    (set-face-attribute 'org-block-end-line nil :foreground 'unspecified
												:background 'unspecified))

  (add-to-list 'enable-theme-functions #'disable-org-block)
  (add-hook 'org-modern-mode-hook (lambda () (disable-org-block nil))))

(provide 'init-org)
;;; init-org.el ends here
