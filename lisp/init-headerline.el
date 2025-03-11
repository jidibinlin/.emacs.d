;;; init-headerline.el --- Description -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Custom header line configuration with winum integration
;;
;;; Code:


(use-package hide-mode-line
  :hook (elpaca-after-init . global-hide-mode-line-mode)
  :ensure t
  :demand t)

(use-package spacious-padding
  :ensure t
  :demand t
  :hook (elpaca-after-init . spacious-padding-mode))

(defun subtle-modeline (&rest _)
  "`subtle-modeline' enable subtle mode line."
  (let* ((active-subtle (face-foreground 'font-lock-constant-face nil t))
				 (inactive-subtle (face-foreground 'shadow nil t))
				 (foreground (face-foreground 'default nil t))
				 (background (face-background 'default nil t))
				 (origin-box (face-attribute 'mode-line :box))
				 (new-box  (if (plistp origin-box)
                       (plist-put origin-box :color background)
										 (plist-put '() :color background))))

    (set-face-attribute 'mode-line nil
												:overline active-subtle
												:background background :box new-box)
    (set-face-attribute 'mode-line-active nil
												:overline active-subtle :background background :box new-box)
    (set-face-attribute 'mode-line-inactive nil
												:overline inactive-subtle  :background background :box new-box)
    (set-face-attribute 'header-line nil
												:overline active-subtle :foreground foreground
												:background background :box new-box)))

(add-hook 'elpaca-after-init-hook #'subtle-modeline)

(add-to-list 'enable-theme-functions #'subtle-modeline)

(use-package breadcrumb
  :init
  (setq breadcrumb-project-max-length 0.0)
	(setq breadcrumb-idle-time 1.5)
  :demand t
  :ensure t)

(defface conia/header-line-mode-indicator-face
	'((t :inherit (font-lock-keyword-face)))
	"Face for major mode indicator."
	:group 'conia)

(defvar-local conia/header-line-vc-indicator-cache nil)
(defun conia/header-line-vc-indicator ()
	(if (thread-first conia/header-line-vc-indicator-cache null not)
			conia/header-line-vc-indicator-cache
		(when (stringp vc-mode)
			(let ((icon (format "%s %s"
													(nerd-icons-devicon "nf-dev-git_branch"
																							:face
																							"font-lock-keyword-face"
																							:v-adjust 0.16)
													(string-trim-left vc-mode))))
				(setq-local conia/header-line-vc-indicator-cache icon)
				icon))))

(defvar-local conia/header-line-mode-icon-cache nil)
(defun conia/header-line-mode-indicator ()
	(if (thread-first conia/header-line-mode-icon-cache null not)
			conia/header-line-mode-icon-cache
		(let* ((icon (nerd-icons-icon-for-buffer))
					 (prop (text-properties-at 0 icon))
					 (face (plist-get prop 'face))
					 (face (copy-sequence face))
					 (_ (plist-put face :inherit 'conia/header-line-mode-indicator-face))
					 (icon (propertize icon 'face face)))
			(setq-local conia/header-line-mode-icon-cache icon)
			conia/header-line-mode-icon-cache)))

(defface conia/header-line-input-method-indicator-face
	'((t :inherit (font-lock-keyword-face) :bold t))
	"Face for input method indicator."
	:group 'conia)

(defun conia/header-line-input-method-indicator ()
	(when current-input-method
		(let* ((str (format "%s " current-input-method))
					 (str (capitalize str)))
			(propertize str 'face 'conia/header-line-input-method-indicator-face))))

(defun conia/set-base-header-line-format ()
	(setq-default header-line-format '((:eval (meow--render-indicator))
																		 (:eval (conia/header-line-input-method-indicator))
																		 (:eval (conia/header-line-vc-indicator))
																		 " "
																		 (:eval (conia/header-line-mode-indicator))
																		 " "
																		 (:eval (breadcrumb--header-line)))))

(add-hook 'elpaca-after-init-hook #'conia/set-base-header-line-format)

(defun conia/subtle-set-face-for-frame (frame)
	"Set face for frame."
	(with-selected-frame frame
		(subtle-modeline)))

(add-to-list 'after-make-frame-functions #'conia/subtle-set-face-for-frame)

(provide 'init-headerline)
;;; init-headerline.el ends here
