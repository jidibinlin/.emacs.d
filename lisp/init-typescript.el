;;; init-typescript.el --- summary -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; TypeScript configuration with tree-sitter support
;;
;;; Code:

(use-package typescript-ts-mode
  :hook (typescript-ts-mode . eglot-ensure))

(provide 'init-typescript)
;;; init-typescript.el ends here
