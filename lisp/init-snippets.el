;;; init-snippets.el --- Description -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Snippet management with Tempel and LSP integration
;;
;;; Code:

(use-package tempel
  :bind (:map tempel-map
              ("TAB" . tempel-next)
              ("S-TAB" . tempel-previous))
  :init
	(add-to-list 'conia/capfs-to-merge
							 (cons 'prog-mode #'tempel-complete))
	(add-to-list 'conia/capfs-to-merge
							 (cons 'conf-mode #'tempel-complete))
	(add-to-list 'conia/capfs-to-merge
							 (cons 'text-mode #'tempel-complete))
  (add-hook 'conf-mode-hook 'conia/merge-capf)
  (add-hook 'prog-mode-hook 'conia/merge-capf)
  (add-hook 'text-mode-hook 'conia/merge-capf))

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection
  :after tempel
  :ensure t
	:demand t)

(use-package eglot-tempel
  :hook (after-init . eglot-tempel-mode)
  :ensure t
	:demand t)

(provide 'init-snippets)
;;; init-snippets.el ends here
