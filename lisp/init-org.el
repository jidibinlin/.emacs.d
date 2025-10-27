;;; init-org.el --- org -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Org-mode configuration with Roam integration
;;
;;; Code:

(use-package org-margin
	:hook (org-mode . org-margin-mode))


(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/Documents/MyBlogSite/RoamNote/"))
  :config
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
	(setq org-roam-main-templates '(("n" "Note" plain "%?"
																	 :target (file+head "${title}/${title}.org"
																											"
:PROPERTIES:
:ID:            ${id}
:TYPE:          ${TYPE}
:END:

#+startup: latexpreview
#+OPTIONS: author:nil ^:{}
#+HUGO_BASE_DIR: ~/Documents/MyBlogSite
#+HUGO_SECTION: /posts/%<%Y/%m>
#+HUGO_CUSTOM_FRONT_MATTER: :toc true :math true
#+HUGO_AUTO_SET_LASTMOD: t
#+HUGO_PAIRED_SHORTCODES: admonition
#+HUGO_DRAFT: true
#+DATE: %<[%Y-%m-%d %a %H:%M]>
#+TITLE: ${title}
#+HUGO_TAGS:
#+HUGO_CATEGORIES:
#+DESCRIPTION:
#+begin_export html
<!--more-->
#+end_export")
																	 :unnarrowed t)))

	(setq org-roam-sub-templates '(("n" "Note" plain "%?"
																	:target (file+head "${MAIN-TITLE}/${title}/${title}.org"
																										 "
:PROPERTIES:
:ID:            ${id}
:TYPE:          ${TYPE}
:END:

#+startup: latexpreview
#+OPTIONS: author:nil ^:{}
#+HUGO_BASE_DIR: ~/Documents/MyBlogSite
#+HUGO_SECTION: /posts/%<%Y/%m>
#+HUGO_CUSTOM_FRONT_MATTER: :toc true :math true :author golang-official
#+HUGO_AUTO_SET_LASTMOD: t
#+HUGO_PAIRED_SHORTCODES: admonition
#+HUGO_DRAFT: true
#+DATE: %<[%Y-%m-%d %a %H:%M]>
#+TITLE: ${title}
#+HUGO_TAGS:
#+HUGO_CATEGORIES:
#+DESCRIPTION:
#+begin_export html
<!--more-->
#+end_export

* main topic links :noexport:
[[id:${MAIN-ID}][${MAIN-TITLE}]]
")
																	:unnarrowed t)))

	(cl-defun org-roam-main-node-capture (&optional goto keys &key info)
		(interactive "P")
		(let ((filter-fn (lambda (org-roam-node)
											 (--> (assoc "TYPE" (org-roam-node-properties org-roam-node))
														(when it (cdr it))
														(string= it "main")
														))))
			(org-roam-capture goto  keys
												:filter-fn filter-fn
												:templates org-roam-main-templates
												:info '(:TYPE "main"))))

	(cl-defun org-roam-sub-node-capture (&optional goto keys &key info)
		(interactive "P")
		(let* ((filter-fn (lambda (node-type)
												(lambda (org-roam-node)
													(--> (assoc "TYPE" (org-roam-node-properties org-roam-node))
															 (when it (cdr it))
															 (string= it node-type)))))
					 (main-node (org-roam-node-read nil  (funcall filter-fn "main") nil t "Select main subject")))
			(if main-node
					(org-roam-capture goto  keys
														:filter-fn (funcall filter-fn "sub")
														:templates org-roam-sub-templates
														:info (list :TYPE "sub"
																				:MAIN-TITLE (org-roam-node-title main-node)
																				:MAIN-ID (org-roam-node-id main-node)))
				(user-error "selected main subject not exist")))))

(provide 'init-org)
;;; init-org.el ends here
