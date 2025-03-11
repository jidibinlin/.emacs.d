;;; init-default.el --- Description -*- lexical-binding: t; -*-

(require 'envir)

(setq-default create-lockfiles nil)

;; font-jit
(setq jit-lock-chunk-size 8192)
(setq jit-lock-stealth-nice 1)
(setq jit-lock-defer-time 0.1)

;; linum
(setq-default display-line-numbers-width 3)
(setq-default display-line-numbers-widen t)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)

;; show diff of save some buffer
(add-to-list 'save-some-buffers-action-alist
             (list "d"
                   (lambda (buffer)
										 (diff-buffer-with-file (buffer-file-name buffer)))
                   "show diff between the buffer and its file"))

(setq make-backup-files nil)

(setq-default bidi-display-reordering nil)
(setq bidi-inhibit-bpa t
			long-line-threshold 1000
			large-hscroll-threshold 1000
			syntax-wholeline-max 1000)


(setq-default scroll-conservatively most-positive-fixnum)
(setq-default scroll-margin 10)

(setq-default ring-bell-function 'ignore)

(setq-default tab-width 2)

(setq-default lisp-indent-offset 2)

(setq-default fringes-outside-margins t)

(setq large-file-threshold (* 1024 1024))

(setq lisp-indent-offset nil)

(defun conia/large-file-control (orig-fun &rest args)
	"`conia/large-file-control' control large file.
`ORIG-FUN' is the original function `ARGS' is the rest param."
	(if (and buffer-file-name
					 (> (buffer-size) large-file-threshold))
			(progn (read-only-mode 1)
						 nil)
		(apply orig-fun args)))

(advice-add 'font-lock-mode :around #'conia/large-file-control)

(use-package gcmh
	:ensure t
	:config
	(gcmh-mode 1))

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
	:ensure t
	:custom
	(posframe-inhibit-double-buffering t)
	:init
	(defun posframe-poshandler-frame-near-top-center (info)
		"Posframe handler that smartly positions near top with dynamic offset."
		(cons 
		 ;; 水平居中
		 (/ (- (plist-get info :parent-frame-width)
					 (plist-get info :posframe-width))
				2)
		 ;; 让出顶部header-line
		 (+ (line-pixel-height)
				(plist-get spacious-padding-widths :internal-border-width)
				(plist-get spacious-padding-widths :header-line-width)
				(plist-get spacious-padding-widths :mode-line-width)
				5)))
	:config
	(defun conia/refresh-posframe (_)
		(posframe-delete-all))
	(add-to-list 'enable-theme-functions #'conia/refresh-posframe))

(use-package hydra
	:ensure t
	:bind (:map meow-motion-state-keymap
							("SPC" . toggles-hydra/body))
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
							:poshandler posframe-poshandler-frame-near-top-center)))
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
		(let ((face (or face `(:inherit highlight
																		:reverse-video t
																		:background unspecified)))
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
		(:title (pretty-hydra-title "Main" 'mdicon "nf-md-cat")
						:color amaranth :quit-key ("q" "C-g" "ESC"))
		("Project"
		 (("p p" project-switch-project "switch project" :exit t)
			("p f" project-find-file "find file" :exit t)
			("p b" consult-project-buffer "switch project buffer" :exit t)
			("p v" magit "magit" :exit t)
			("p s" consult-ripgrep "search in project" :exit t)
			("p c" project-kill-buffers "close project" :exit t))
		 "Search"
		 (("s d" xref-find-definitions "find definitions" :exit t)
			("s r" xref-find-references "find references" :exit t)
			("s i" consult-imenu-multi "imenu" :exit t))
		 "Window/Workspace"
		 (("w" toggles-window/body "windows/workspace" :exit t))
		 "Buffer"
		 (("," consult-buffer "switch buffer" :exit t)
			("b d" kill-current-buffer "kill buffer" :exit t)
			("f s" save-buffer "save current buffer" :exit t)))))

(use-package elisp-benchmarks
	:ensure t)

(use-package nano-read
	:ensure (:host github :repo "rougier/nano-read"))

(use-package text-mode
	:init
	(setopt text-mode-ispell-word-completion t))

(provide 'init-default)
;;; init-default.el ends here
