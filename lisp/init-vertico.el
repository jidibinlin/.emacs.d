;;; init-vertico.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 jidibinlin
;;
;; Author: jidibinlin <2694273649@qq.com>
;; Maintainer: jidibinlin <2694273649@qq.com>
;; Created: November 01, 2024
;; Modified: November 01, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/qibin/init-vertico
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defvar conia-completion-styles '(basic partial-completion orderless))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :custom
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch))
  ;; (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-styles conia-completion-styles)
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

(use-package marginalia
  :hook (elpaca-after-init . marginalia-mode)
  :demand t
  :ensure t)

(use-package vertico
  :hook (elpaca-after-init . vertico-mode)
  :ensure t
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 20)
  (vertico-resize nil)
  (vertico-cycle t)
  (enable-recursive-minibuffers t)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :init
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
            (replace-regexp-in-string
              "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
              crm-separator)
            (car args))
      (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
    '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode))

(use-package vertico-directory
	:after vertico
	:ensure nil
	:bind (:map vertico-map
							("RET" . vertico-directory-enter)
							("DEL" . vertico-directory-delete-char)
							("M-DEL" . vertico-directory-delete-word))
	:hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line))
	:init
	(defvar +fly-commands
    '(consult-line
       consult-outline
       consult-imenu
       consult-ripgrep
       isearch-forward
       isearch-backward
       +vertico/project-search
       eglot-rename))
	
	(defvar +fly-back-commands
    '(self-insert-command))
	
	:config
	(setq xref-show-xrefs-function #'consult-xref)
	(setq xref-show-definitions-function #'consult-xref)
	
	(defun +fly-back-to-present ()
    "Self-explained."
    (remove-hook 'pre-command-hook '+fly-back-to-present t)
    (cond ((and (memq last-command +fly-commands)
             (equal (this-command-keys-vector) (kbd "M-p")))
            ;; repeat one time to get straight to the first history item
            (setq unread-command-events
              (append unread-command-events
                (listify-key-sequence (kbd "M-p")))))
      ((memq this-command +fly-back-commands)
        (delete-region
          (goto-char (minibuffer-prompt-end))
          (point-max)))))
	
	(defun +fly-time-travel ()
    "Insert `thing-at-point'."
    (when (memq this-command +fly-commands)
      (insert (propertize
                (save-excursion
                  (set-buffer (window-buffer (minibuffer-selected-window)))
                  (or (seq-some
                        (lambda (thing) (thing-at-point thing t))
                        '(symbol))
                    "No thing at point"))
                'face 'shadow))
      (add-hook 'pre-command-hook '+fly-back-to-present nil t)))
	(add-hook 'minibuffer-setup-hook #'+fly-time-travel))

(use-package consult-tramp
	:ensure (:host github :repo "Ladicle/consult-tramp"))

(use-package embark-consult
	:ensure t)

(use-package which-key
	:ensure t)

(use-package embark
	:ensure t
	:bind ("C-;" . #'embark-act)
	:config
	;; shiped from doom
	(defun +vertico-embark-which-key-indicator ()
		"An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
		(lambda (&optional keymap targets prefix)
			(if (null keymap)
				(which-key--hide-popup-ignore-command)
				(which-key--show-keymap
					(if (eq (plist-get (car targets) :type) 'embark-become)
						"Become"
						(format "Act on %s '%s'%s"
							(plist-get (car targets) :type)
							(embark--truncate-target (plist-get (car targets) :target))
							(if (cdr targets) "…" "")))
					(if prefix
						(pcase (lookup-key keymap prefix 'accept-default)
							((and (pred keymapp) km) km)
							(_ (key-binding prefix 'accept-default)))
						keymap)
					nil nil t (lambda (binding)
											(not (string-suffix-p "-argument" (cdr binding))))))))

	(with-eval-after-load 'which-key
		(defun +vertico--embark-which-key-prompt (fn &rest args)
			"Hide the which-key indicator immediately when using the completing-read prompter."
			(which-key--hide-popup-ignore-command)
			(let ((embark-indicators
							(remq #'embark-which-key-indicator embark-indicators)))
				(apply fn args)))

		(advice-add 'embark-completing-read-prompter
			:around #'+vertico--embark-which-key-prompt)
		(cl-nsubstitute #'+vertico-embark-which-key-indicator #'embark-mixed-indicator embark-indicators))
	(bind-key "C-;" #'embark-act minibuffer-local-map)
	(bind-key "C-c C-;" #'embark-export minibuffer-local-map)
	(bind-key "C-c C-l" #'embark-collect minibuffer-local-map)
	(bind-key "C-c C-e" #'+vertico/embark-export-write))

(use-package wgrep
	:commands wgrep-change-to-wgrep-mode
	:ensure t
	:config
	(setq wgrep-auto-save-buffer t)
	:init
	(defun +vertico/embark-export-write ()
		"Export the current vertico results to a writable buffer if possible.

Supports exporting consult-grep to wgrep, file to wdeired, and consult-location to occur-edit"
		(interactive)
		(require 'embark)
		(require 'wgrep)
		(let* ((edit-command
						 (pcase-let ((`(,type . ,candidates)
													 (run-hook-with-args-until-success 'embark-candidate-collectors)))
							 (pcase type
								 ('consult-grep #'wgrep-change-to-wgrep-mode)
								 ('file #'wdired-change-to-wdired-mode)
								 ('consult-location #'occur-edit-mode)
								 (x (user-error "embark category %S doesn't support writable export" x)))))
						(embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
			(embark-export))))

(provide 'init-vertico)
;;; init-vertico.el ends here
