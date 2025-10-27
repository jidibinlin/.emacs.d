;;; init-treesit.el --- Description -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Tree-sitter configuration and automatic grammar installation
;;
;;; Code:


(use-package treesit-auto
  :demand t
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(provide 'init-treesit)
;;; init-treesit.el ends here
