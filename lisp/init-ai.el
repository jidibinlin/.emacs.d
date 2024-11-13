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
	(setq gptel-claude  (gptel-make-openai
													"OpenRouter"
												:host "openrouter.ai"
												:endpoint "/api/v1/chat/completions"
												:stream t
												:key openrouter_token
												:models '("anthropic/claude-3.5-sonnet")))
	(setq gptel-github (gptel-make-openai
												 "Github Models"
											 :host "models.inference.ai.azure.com"
											 :endpoint "/chat/completions"
											 :stream t
											 :key github_token
											 :models '(gpt-4o)))
	(setq gptel-model 'gpt-4o
				gptel-backend gptel-github
				gptel-log-level 'debug))

(use-package aider
	:ensure
	(:host github :repo "tninja/aider.el")
  :config
  (setq aider-args '("--no-auto-commits"
										 "--model" "openrouter/anthropic/claude-3.5-sonnet")))

(use-package copilot
	:ensure
	(:host github :repo "copilot-emacs/copilot.el")
  :hook (prog-mode . (lambda ()
                       (when (not (derived-mode-p 'read-only-mode))
                         (copilot-mode))))
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
	
	(advice-add 'copilot-mode :around #'conia/large-file-control))

(provide 'init-ai)
;;; init-ai.el ends here
