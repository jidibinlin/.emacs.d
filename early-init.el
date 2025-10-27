;;; early-init.el --- emacs early init file -*- lexical-binding: t; -*-

;;; Commentary:
;;
;;  Description
;;
;;; Code:

;; (when (< emacs-major-version 31)
;; 	(setq-default comp-deferred-compilation nil)
;; 	(setq-default native-comp-deferred-compilation nil)
;; 	(setq-default native-comp-enable nil)
;; 	(setq-default native-comp-speed -1))

(setq package-enable-at-startup nil)

(setq native-comp-async-report-warnings-errors 'silent)

(add-to-list 'load-path (concat user-emacs-directory "envir/"))
(add-to-list 'load-path (concat user-emacs-directory "lisp/"))
(require 'envir)

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertico-scroll-bars) default-frame-alist)
(when (or conia-syis-wsl conia-sysis-windows)
	(push '(undecorated . t) default-frame-alist))

(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil)

(scroll-bar-mode -1)

(setq-default auto-save-default nil)
(prefer-coding-system 'utf-8)
(setq gc-cons-threshold (* 1024 1024 20))


(setq custom-file (make-temp-file "emacs-custom"))

;;; early-init.el ends here
