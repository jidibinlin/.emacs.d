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

(use-package gptel
	:ensure t
	:init
  (setq gptel-model "anthropic/claude-3.5-sonnet")
  :config
  (setq gptel-backend
				(gptel-make-openai
						"OpenRouter"
					:host "openrouter.ai"
					:endpoint "/api/v1/chat/completions"
					:stream t
					:key "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"
					:models '("anthropic/claude-3.5-sonnet"))))

(use-package aider
	:ensure
	(:host github :repo "tninja/aider.el")
  :config
  (setq aider-args '("--no-auto-commits"
										 "--model" "openrouter/anthropic/claude-3.5-sonnet"))
  (setenv
	 "OPENROUTER_API_KEY"
	 "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"))

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

(use-package copilot-chat
  :after (request)
	:ensure
	(:host github :repo "chep/copilot-chat.el")
  :custom
  (copilot-chat-frontend 'org))


(provide 'init-ai)
;;; init-ai.el ends here
