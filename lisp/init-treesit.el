;;; init-treesit.el --- Description -*- lexical-binding: t; -*-


(use-package treesit-auto
  :ensure t
  :demand t
  :hook (after-init . global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all))

(provide 'init-treesit)
;;; init-treesit.el ends here
