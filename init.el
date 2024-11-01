;;; init.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 jidibinlin
;;
;; Author: jidibinlin <2694273649@qq.com>
;; Maintainer: jidibinlin <2694273649@qq.com>
;; Created: November 01, 2024
;; Modified: November 01, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/qibin/init
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(require 'envir)
(require 'init-font)
(require 'init-theme)
(require 'init-headerline)
(require 'init-treesit)
(require 'init-meow)
(require 'init-vertico)
(require 'init-corfu)
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ignored-local-variable-values '((git-commit-major-mode . git-commit-elisp-text-mode)))
 '(package-selected-packages
   '(breadcrumb consult corfu hide-mode-line marginalia meow orderless
		spacious-padding treesit-auto vertico)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t :background "#ffffff")))
 '(header-line ((t :box (:line-width 4 :color "#ffffff" :style nil))))
 '(header-line-highlight ((t :box (:color "#000000"))))
 '(keycast-key ((t)))
 '(line-number ((t :background "#ffffff")))
 '(mode-line ((t :box (:line-width 6 :color "#ffffff" :style nil))))
 '(mode-line-active ((t :box (:line-width 6 :color "#ffffff" :style nil))))
 '(mode-line-highlight ((t :box (:color "#000000"))))
 '(mode-line-inactive ((t :box (:line-width 6 :color "#ffffff" :style nil))))
 '(tab-bar-tab ((t :box (:line-width 4 :color "#ffffff" :style nil))))
 '(tab-bar-tab-inactive ((t :box (:line-width 4 :color "#c2c2c2" :style nil))))
 '(tab-line-tab ((t)))
 '(tab-line-tab-active ((t)))
 '(tab-line-tab-inactive ((t)))
 '(vertical-border ((t :background "#ffffff" :foreground "#ffffff")))
 '(window-divider ((t (:background "#ffffff" :foreground "#ffffff"))))
 '(window-divider-first-pixel ((t (:background "#ffffff" :foreground "#ffffff"))))
 '(window-divider-last-pixel ((t (:background "#ffffff" :foreground "#ffffff")))))
