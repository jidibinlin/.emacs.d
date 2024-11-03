;;; init-default.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 jidibinlin
;;
;; Author: jidibinlin <2694273649@qq.com>
;; Maintainer: jidibinlin <2694273649@qq.com>
;; Created: November 01, 2024
;; Modified: November 01, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/qibin/init-vc
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(setq jit-lock-chunk-size 8192)
(setq jit-lock-stealth-nice 1)

(use-package hungry-delete
  :ensure t
  :hook (elpaca-after-init . global-hungry-delete-mode))

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
  (defun +pretty-flymake-fringe-indicator()
    (let ((bitmap (if (eq flymake-fringe-indicator-position 'right-fringe)
                      '+flymake-fringe-bitmap-double-arrow-left
                    '+flymake-fringe-bitmap-double-arrow-right)))
      (setq flymake-error-bitmap `(,bitmap compilation-error))
      (setq flymake-warning-bitmap `(,bitmap compilation-warning))
      (setq flymake-note-bitmap `(,bitmap compilation-info)))))

(use-package eldoc
  :config
  (defun conia/eldoc-invoke-strategy-advice (_)
   (if (or (not (listp eldoc-documentation-functions))
           (equal (cl-first eldoc-documentation-functions) #'flymake-eldoc-function))
       nil
     (setq-local eldoc-documentation-functions
		 (let ((rest (remove #'flymake-eldoc-function eldoc-documentation-functions)))
		   (cons #'flymake-eldoc-function  rest)))))
  
  (advice-add #'eldoc--invoke-strategy :before #'conia/eldoc-invoke-strategy-advice))

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
  (define-key prog-mode-map (kbd "M-p") #'my/eldoc-box-scroll-down))

(provide 'init-default)
;;; init-default.el ends here
