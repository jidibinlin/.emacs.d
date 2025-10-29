;;; init-ai.el --- ai -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Configuration for AI-related packages like GPTel, Copilot, etc.
;;
;;; Code:

(setq openrouter_token (getenv "OPENROUTER_API_KEY"))
(setq github_token (getenv "GITHUB_TOKEN"))

(use-package gptel
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
  :config
	(setq aidermacs-default-model "openrouter/deepseek/deepseek-r1:free")
	(setq aidermacs-auto-commits t)
	(setq aidermacs-use-architect-mode nil)
	(setq aidermacs-architect-model "openrouter/deepseek/deepseek-r1:free")
	(setq aidermacs-editor-model "openrouter/deepseek/deepseek-r1:free"))


;; 				 ;;(copilot-mode . conia/remove-copilot-post-command)

;;   :bind (:map  copilot-completion-map
;; 							 ("<tab>" . 'copilot-accept-completion))
;;   :init
;;   (setq copilot-lisp-indent-offset 2)
;; 	:custom
;; 	(copilot-indent-offset-warning-disable t)
;; 	(copilot-max-char most-positive-fixnum)
;;   :config
;;   (add-to-list 'copilot-indentation-alist '(go-mode go-ts-mode-indent-offset))
;;   (add-to-list 'copilot-indentation-alist '(go-ts-mode go-ts-mode-indent-offset))
;;   (add-to-list 'copilot-indentation-alist '(scheme-mode copilot-lisp-indent-offset))
;;   (add-to-list 'copilot-indentation-alist
;; 							 '(clojure-ts-mode copilot-lisp-indent-offset))
;;   (add-to-list 'copilot-indentation-alist
;; 							 '(clojure-mode copilot-lisp-indent-offset))

;; 	(advice-add 'copilot-mode :around #'conia/large-file-control)
;; 	(defun conia/remove-copilot-post-command()
;; 		(remove-hook 'post-command-hook 'copilot--post-command t))

;; 	(defvar-local copilot--cached-completion-returns '())

;; 	(defun copilot--completion-doc-buf (completions key)
;; 		(let ((completion (alist-get key completions)))
;; 			(when completion
;; 				(with-current-buffer (get-buffer-create "*copilot-doc*")
;; 					(erase-buffer)
;; 					(insert (plist-get completion :text))
;; 					(current-buffer)))))

;; 	(defun copilot-completion-at-point()
;; 		"send completion request to copilot and return cached completions"
;; 		;; send request each time triggered
;; 		(copilot--get-completion
;; 		 (jsonrpc-lambda (&key completions &allow-other-keys)
;; 			 (when (not (seq-empty-p completions))
;; 				 (add-to-list 'copilot--cached-completion-returns completions t)
;; 				 (when (length> copilot--cached-completion-returns 3)
;; 					 (setq copilot--cached-completion-returns
;; 								 (cdr copilot--cached-completion-returns))))))
;; 		(let* ((bounds (bounds-of-thing-at-point 'symbol))
;; 					 (start (or (car bounds) (point)))
;; 					 (end (or (cdr bounds) (point)))
;; 					 (completions '()))
;; 			;; preprocess cached completion data keep completion entry unique
;; 			(cl-loop for completion-return in copilot--cached-completion-returns
;; 							 do
;; 							 (seq-doseq (completion completion-return)
;; 								 (let ((key (plist-get completion :text)))
;; 									 (setf (alist-get key completions) completion))))
;; 			;; discard prefix from completion probably
;; 			(let* ((discardpos (- start (line-beginning-position)))
;; 						 (prefix-suber
;; 							(lambda (foo)
;; 								(let* ((key (car foo))
;; 											 (subpose (min discardpos (length key))))
;; 									(setf (car foo) (substring key subpose))))))
;; 				(mapc prefix-suber completions))

;; 			;; build completion data
;; 			(list
;; 			 start
;; 			 end
;; 			 completions
;; 			 :exclusive 'no
;; 			 :company-kind (lambda (_) 'copilot)
;; 			 :company-doc-buffer (apply-partially
;; 														#'copilot--completion-doc-buf
;; 														completions))))

;; 	;; (add-to-list 'conia/capfs-to-merge (cons 'prog-mode 'copilot-completion-at-point))
;; 	;; (add-to-list 'conia/capfs-priority '(copilot-completion-at-point . 50))
;; )

(use-package claude-code-ide
	:config
	(setq claude-code-ide-cli-path "codebuddy-code")
	(setq claude-code-ide-terminal-backend 'eat)
	(claude-code-ide-emacs-tools-setup))

(provide 'init-ai)

;;; init-ai.el ends here
