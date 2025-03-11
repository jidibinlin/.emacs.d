;;; init-go.el --- golang config -*- lexical-binding: t; -*-

(use-package go-ts-mode
	:init
	(setq-default go-ts-mode-indent-offset tab-width)
	:hook (go-ts-mode . eglot-ensure))

(provide 'init-go)
;;; init-go.el ends here
