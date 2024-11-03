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
(global-display-fill-column-indicator-mode 1)

(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")
(defun run-after-load-theme-hook (&rest _)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))
(advice-add #'load-theme :after #'run-after-load-theme-hook)

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

(use-package helpful
  :ensure t
  :bind (([remap describe-function] . helpful-callable)
	 ([remap describe-command] . helpful-command)
	 ([remap describe-variable] . helpful-variable)
	 ([remap describe-key] . helpful-key)
	 ([remap describe-symbol] . helpful-symbol)))

(use-package posframe
  :ensure t)

(use-package hydra
  :ensure t
  :init
  (setq hydra-hint-display-type 'posframe)
  (with-eval-after-load 'posframe
    (defun hydra-set-posframe-show-params ()
      "Set hydra-posframe style."
      (setq hydra-posframe-show-params
            `(:left-fringe 3
              :right-fringe 3
              :internal-border-width 2
	      :internal-border-color ,(face-background 'tooltip nil t)
              :background-color ,(face-background 'tooltip nil t)
              :foreground-color ,(face-foreground 'tooltip nil t)
              :lines-truncate t
              :poshandler posframe-poshandler-frame-center)))
    (hydra-set-posframe-show-params)
    (add-hook 'after-load-theme-hook 'hydra-set-posframe-show-params t)))

(use-package pretty-hydra
  :ensure t
  :custom (pretty-hydra-default-title-body-format-spec " %s%s")
  :bind (:map meow-normal-state-keymap
	      ("SPC" . toggles-hydra/body))
  :init
  (cl-defun pretty-hydra-title (title &optional icon-type icon-name
                                      &key face height v-adjust)
    "Add an icon in the hydra title."
    (let ((face (or face `(:inherit highlight :reverse-video t)))
          (height (or height 1.2))
          (v-adjust (or v-adjust 0.0)))
      (concat
       (when (and icon-type icon-name)
         (let ((f (intern (format "nerd-icons-%s" icon-type))))
           (when (fboundp f)
             (concat
              (apply f (list icon-name :face face :height height :v-adjust v-adjust))
              " "))))
       (propertize title 'face face))))
  (pretty-hydra-define toggles-hydra (:title (pretty-hydra-title "Main" 'faicon "nf-fa-toggle_on")
					     :color amaranth :quit-key ("q" "C-g" "ESC"))
    ("Project"
     (("p p" project-switch-project "switch project" :exit t)
      ("p f" project-find-file "find file" :exit t)
      ("p v" magit "magit" :exit t)
      ("p s" consult-ripgrep "search in project" :exit t))
     "Files"
     (("f s" save-buffer "save current file" :exit t))
     "Search"
     (("s d" xref-find-definitions "find definitions" :exit t)
      ("s r" xref-find-references "find references" :exit t))
     "Window/Workspace"
     (("w v" split-window-right "split window vertico" :exit t)
      ("w h" split-window-below "split window horizontal" :exit t)      
      ("w d" delete-window "close current window" :exit t)
      ("w o" other-window "other window"))))

(use-package elec-pair
  :hook (prog-mode . electric-pair-mode))


(provide 'init-default)
;;; init-default.el ends here
