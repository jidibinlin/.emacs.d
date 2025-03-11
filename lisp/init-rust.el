;;; init-rust.el --- summary -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Rust language configuration with tree-sitter support
;;
;;; Code:

(use-package rust-ts-mode
	:ensure nil
	:hook (rust-ts-mode . eglot-ensure))

(provide 'init-rust)
;;; init-rust.el ends here
