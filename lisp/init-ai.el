;;; init-ai.el --- ai -*- lexical-binding: t -*-

;; Author: 
;; Version: version
;; Package-Requires: dependencies
;; Homepage: homepage
;; Keywords: keywords

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; commentary

;;; Code:

(setq openrouter_token (getenv "OPENROUTER_API_KEY"))
(setq github_token (getenv "GITHUB_TOKEN"))

(use-package gptel
	:ensure t
	:demand t
  :config
	(setq gptel-openrouter  (gptel-make-openai
															"OpenRouter"
														:host "openrouter.ai"
														:endpoint "/api/v1/chat/completions"
														:stream t
														:key openrouter_token
														:models '(anthropic/claude-3.5-sonnet
																			anthropic/claude-3-5-haiku
																			openai/gpt-4o-mini
																			openai/gpt-4o)))
	(setq gptel-github (gptel-make-openai
												 "Github Models"
											 :host "models.inference.ai.azure.com"
											 :endpoint "/chat/completions"
											 :stream t
											 :key github_token
											 :models '(gpt-4o)))
	(setq gptel-model 'openai/gpt-4o-mini
				gptel-backend gptel-openrouter))

(use-package aidermacs
	:ensure t
  :config
	(setq aidermacs-default-model "openrouter/deepseek/deepseek-r1")
	(setq aidermacs-auto-commits t)
	(setq aidermacs-use-architect-mode t)
	(setq aidermacs-architect-model "openrouter/deepseek/deepseek-r1")
	(setq aidermacs-editor-model "openrouter/deepseek/deepseek-r1"))

(use-package copilot
	:ensure
	(:host github :repo "copilot-emacs/copilot.el")
  :hook ((prog-mode . (lambda ()
												(when (not (derived-mode-p 'read-only-mode))
													(copilot-mode))))
				 ;;(copilot-mode . conia/remove-copilot-post-command)
				 )
  :bind (:map  copilot-completion-map
							 ("<tab>" . 'copilot-accept-completion))
  :init
  (setq copilot-lisp-indent-offset 2)
	:custom
	(copilot-indent-offset-warning-disable t)
	(copilot-max-char most-positive-fixnum)
  :config
  (add-to-list 'copilot-indentation-alist '(go-mode go-ts-mode-indent-offset))
  (add-to-list 'copilot-indentation-alist '(go-ts-mode go-ts-mode-indent-offset))
  (add-to-list 'copilot-indentation-alist '(scheme-mode copilot-lisp-indent-offset))
  (add-to-list 'copilot-indentation-alist
							 '(clojure-ts-mode copilot-lisp-indent-offset))
  (add-to-list 'copilot-indentation-alist
							 '(clojure-mode copilot-lisp-indent-offset))

	(advice-add 'copilot-mode :around #'conia/large-file-control)
	(defun conia/remove-copilot-post-command()
		(remove-hook 'post-command-hook 'copilot--post-command t))

	(defvar-local copilot--cached-completion-returns '())

	(defun copilot--completion-doc-buf (completions key)
		(let ((completion (alist-get key completions)))
			(when completion
				(with-current-buffer (get-buffer-create "*copilot-doc*")
					(erase-buffer)
					(insert (plist-get completion :text))
					(current-buffer)))))

	(defun copilot-completion-at-point()
		"send completion request to copilot and return cached completions"
		;; send request each time triggered
		(copilot--get-completion
		 (jsonrpc-lambda (&key completions &allow-other-keys)
			 (when (not (seq-empty-p completions))
				 (add-to-list 'copilot--cached-completion-returns completions t)
				 (when (length> copilot--cached-completion-returns 3)
					 (setq copilot--cached-completion-returns
								 (cdr copilot--cached-completion-returns))))))
		(let* ((bounds (bounds-of-thing-at-point 'symbol))
					 (start (or (car bounds) (point)))
					 (end (or (cdr bounds) (point)))
					 (completions '()))
			;; preprocess cached completion data keep completion entry unique
			(cl-loop for completion-return in copilot--cached-completion-returns
							 do
							 (seq-doseq (completion completion-return)
								 (let ((key (plist-get completion :text)))
									 (setf (alist-get key completions) completion))))
			;; discard prefix from completion probably
			(let* ((discardpos (- start (line-beginning-position)))
						 (prefix-suber
							(lambda (foo)
								(let* ((key (car foo))
											 (subpose (min discardpos (length key))))
									(setf (car foo) (substring key subpose))))))
				(mapc prefix-suber completions))

			;; build completion data
			(list
			 start 
			 end
			 completions
			 :exclusive 'no
			 :company-kind (lambda (_) 'copilot)
			 :company-doc-buffer (apply-partially
														#'copilot--completion-doc-buf
														completions))))

	;; (add-to-list 'conia/capfs-to-merge (cons 'prog-mode 'copilot-completion-at-point))
	;; (add-to-list 'conia/capfs-priority '(copilot-completion-at-point . 50))
	)

(provide 'init-ai)

;;; init-ai.el ends here
