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
(global-display-line-numbers-mode 1)

(setq-default scroll-conservatively most-positive-fixnum)
(setq-default scroll-margin 10)

(setq-default tab-width 2)

(use-package hungry-delete
  :ensure t
  :hook (elpaca-after-init . global-hungry-delete-mode))



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



(use-package helpful
  :ensure t
  :bind (([remap describe-function] . helpful-callable)
				 ([remap describe-command] . helpful-command)
				 ([remap describe-variable] . helpful-variable)
				 ([remap describe-key] . helpful-key)
				 ([remap describe-symbol] . helpful-symbol)))

(use-package display-fill-column-indicator
  :hook (after-init   . global-display-fill-column-indicator-mode)
  :config
  (defun my/disable-fille-column-background(&rest _)
    (set-face-attribute 'fill-column-indicator nil :background 'unspecified))
  (add-hook 'display-fill-column-indicator-mode-hook
            #'my/disable-fille-column-background)
  (add-to-list 'enable-theme-functions #'my/disable-fille-column-background))

(use-package posframe
  :ensure t)

(use-package hydra
  :ensure t
  :init
  (setq hydra-hint-display-type 'posframe)
  (with-eval-after-load 'posframe
    (defun hydra-set-posframe-show-params (&rest _)
      "Set hydra-posframe style."
      (setq hydra-posframe-show-params
            `(
							;; :left-fringe 3
              ;; :right-fringe 3
              :internal-border-width 10
							:internal-border-color ,(face-background 'tooltip nil t)
              :background-color ,(face-background 'tooltip nil t)
              :foreground-color ,(face-foreground 'tooltip nil t)
              :lines-truncate t
              :poshandler posframe-poshandler-frame-center)))
    (hydra-set-posframe-show-params)
    (add-to-list 'enable-theme-functions 'hydra-set-posframe-show-params t)))

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
  
  (pretty-hydra-define
    toggles-hydra
    (:title (pretty-hydra-title "Main" 'faicon "nf-fa-toggle_on")
						:color amaranth :quit-key ("q" "C-g" "ESC"))
    ("Project"
     (("p p" project-switch-project "switch project" :exit t)
      ("p f" consult-project-buffer "find file" :exit t)
      ("p v" magit "magit" :exit t)
      ("p s" consult-ripgrep "search in project" :exit t)
			("p c" project-kill-buffers "close project" :exit t))
     "Search"
     (("s d" xref-find-definitions "find definitions" :exit t)
      ("s r" xref-find-references "find references" :exit t))
     "Window/Workspace"
     (("w v" split-window-right "split window vertico" :exit t)
      ("w h" split-window-below "split window horizontal" :exit t)
      ("w d" delete-window "close current window" :exit t)
      ("w o" other-window "other window"))
     "Buffer"
     (("," consult-buffer "switch buffer" :exit t)
      ("b d" kill-current-buffer "kill buffer" :exit t)
			("f s" save-buffer "save current buffer" :exit t)))))

(provide 'init-default)
;;; init-default.el ends here
