;;; init-headerline.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 jidibinlin
;;
;; Author: jidibinlin <2694273649@qq.com>
;; Maintainer: jidibinlin <2694273649@qq.com>
;; Created: November 01, 2024
;; Modified: November 01, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/qibin/init-headerline
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:


(use-package hide-mode-line
  :pin melpa
  :hook (after-init . global-hide-mode-line-mode)
  :ensure t
  :demand t)

(use-package spacious-padding
  :ensure t
  :demand t
  :hook ((after-init . spacious-padding-mode)))

(defun subtle-modeline ()
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

(defun enable-theme-func-subtlemodeline(_)
  "`enable-theme-func-subtlemodeline' `enable-theme' advice for `subtle-modeline'."
  (subtle-modeline))

(add-hook 'after-init-hook
          (lambda ()
            (subtle-modeline)
            (advice-add 'enable-theme :after #'enable-theme-func-subtlemodeline)
            (add-hook 'spacious-padding-mode-hook #'subtle-modeline)))

(use-package breadcrumb
  :init
  (setq breadcrumb-project-max-length 0.0)
  :demand t
  :ensure t)

(setq-default header-line-format '((:eval (meow--render-indicator))
                                   vc-mode
                                   "  "
                                   (:eval (breadcrumb--header-line))))

(provide 'init-headerline)
;;; init-headerline.el ends here
