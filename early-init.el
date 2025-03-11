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

(setq package-archives
      (let ((proto (if (gnutls-available-p) "https" "http")))
        `(("gnu"    . ,(format "%s://elpa.gnu.org/packages/" proto))
          ("nongnu" . ,(format "%s://elpa.nongnu.org/nongnu/" proto))
          ("melpa"  . ,(format "%s://melpa.org/packages/" proto)))))

(defvar elpaca-installer-version 0.10)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
															:ref nil :depth 1
															:files (:defaults "elpaca-test.el" (:exclude "extensions"))
															:build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

(setq package-enable-at-startup nil)
(setq custom-file (make-temp-file "emacs-custom"))

(elpaca elpaca-use-package
  (elpaca-use-package-mode))

;;; early-init.el ends here
