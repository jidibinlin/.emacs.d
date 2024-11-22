;;; init-lsp.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 jidibinlin
;;
;; Author: jidibinlin <2694273649@qq.com>
;; Maintainer: jidibinlin <2694273649@qq.com>
;; Created: November 02, 2024
;; Modified: November 02, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/qibin/init-lsp
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(use-package eglot
  :init
  (setq eglot-autoshutdown t)
  (setq eglot-send-changes-idle-time 0.05)
  (setq-default eglot-events-buffer-size 0)
	(setq-default eglot-sync-connect 0)
	(add-to-list 'conia/capfs-to-merge (cons 'eglot--managed-mode
																					 'eglot-completion-at-point))
	(add-to-list 'conia/capfs-priority (cons 'eglot-completion-at-point 100))
	:demand t
	:config
	(add-to-list 'eglot-stay-out-of 'imenu)
	;; format on save
	(cl-defun conia/format--with-eglot (beg end &key buffer callback &allow-other-keys)
		(with-current-buffer buffer
			(or (with-demoted-errors "%s"
						(always (eglot-format beg end)))
					(ignore (funcall callback)))))
	(cl-defun conia/apheleia-formatter-eglot
			(&rest plist &key buffer callback &allow-other-keys)
		(conia/format--with-eglot nil nil :buffer buffer plist))

	(defun conia/enable-eglot-format-onsave ()
		(setq-local apheleia-formatter 'eglot))
	(add-hook 'eglot--managed-mode-hook #'conia/enable-eglot-format-onsave)

	(defun conia/eglot--register-apheleia-formatter()
		(add-to-list 'apheleia-formatters
								 '(eglot . conia/apheleia-formatter-eglot)))
	(add-hook 'elpaca-after-init-hook  #'conia/eglot--register-apheleia-formatter)
	(add-hook 'eglot--managed-mode-hook #'conia/merge-capf 100)

	;; eglot async
	(defvar-local eglot--cached-completion '())
	
	(cl-defmacro eglot--async-request
			(method params &rest args &key (success-fn) &allow-other-keys)
		`(progn
			 (when-let* ((server (eglot--current-server-or-lose))
									 (buf (current-buffer)))
				 (jsonrpc-async-request
					server
					,method ,params
					:success-fn (lambda (result)
												(if (buffer-live-p buf)
														(with-current-buffer buf
															(funcall ,success-fn result))))
					,@args))))
	
	(defvar-local member-trigger '("->" "::" "=>" ":" "."))

	(defun thing-at-point-member-trigger ()
		(cl-loop for (trigger regex len) in
						 (mapcar (lambda (trigger)
											 (list trigger
														 (regexp-quote trigger)
														 (length trigger)))
										 member-trigger)
						 when (save-excursion
										(backward-char len)
										(looking-at regex))
						 return trigger))
	
	(put 'member-trigger 'thing-at-point
			 'thing-at-point-member-trigger)

	;; (symbol . sy)
	;; (member . (char . parent))
	;; nil
	(defun eglot--completion-context()
		"Get current completion context"
		(let ((trigger (thing-at-point 'member-trigger t)))
			(if trigger
					(save-excursion
						(backward-char (length trigger))
						(cons 'member (cons trigger (thing-at-point 'symbol t))))
				(save-excursion
					(when-let* ((symbol (thing-at-point 'symbol t))
											(bounds (bounds-of-thing-at-point 'symbol)))
						(goto-char (car bounds))
						(if-let* ((trigger (thing-at-point 'member-trigger t)))
								(progn
									(backward-char (length trigger))
									(cons 'member (cons trigger (thing-at-point 'symbol t))))
							(cons 'symbol symbol)))))))

	;; (let ((context1 (cons 'member (cons "." "this")))
	;; 			(context2 (cons 'member (cons "." "physxDemo_Button"))))
	;; 	(message "is same? %s"(eglot--same-contextp context1 context2)))

	(defun eglot--same-contextp (cached-context cur-context)
		(let ((cached-t (when (consp cached-context)
											(car cached-context)))
					(cur-t (when (consp cur-context)
									 (car cur-context))))
			(when (equal cached-t cur-t)
				(pcase cached-t
					('symbol (equal (cdr cached-context) (cdr cur-context)))
					('member (let* ((cached-trigger-punc (cadr cached-context))
													(cached-trigger-parent (cddr cached-context))
													(cur-trigger-punc (cadr cur-context))
													(cur-trigger-parent (cddr cur-context)))
										 (and 
											(equal cached-trigger-punc cur-trigger-punc)
											(equal cached-trigger-parent cur-trigger-parent))))
					(t t)))))
	
	(defun eglot--get-on-completion-return-callback (context)
		(jsonrpc-lambda (&rest resp &allow-other-keys)
			(setq eglot--cached-completion
						(cons context (if (vectorp resp) resp
														(plist-get resp :items))))))
	
	(defun eglot-completion-at-point ()
		"Eglot's `completion-at-point' function."

		;; try notify lsp
		(when-let (completion-capability (eglot--server-capable :completionProvider))
																				;(eglot--signal-textDocument/didChange)
			(let ((context (eglot--completion-context))
						(cached-context (when (consp eglot--cached-completion)
															(car eglot--cached-completion))))
				(when (or (and (null context)
											 (null cached-context))
									(not (eglot--same-contextp cached-context context)))
					(eglot--async-request
					 'textDocument/completion
					 (eglot--CompletionParams)
					 :success-fn (eglot--get-on-completion-return-callback
												context))))
			
			;; Commit logs for this function help understand what's going on.
			(let* ((server (eglot--current-server-or-lose))
						 (sort-completions
							(lambda (completions)
								(cl-sort completions
												 #'string-lessp
												 :key (lambda (c)
																(plist-get
																 (get-text-property 0 'eglot--lsp-item c)
																 :sortText)))))
						 (metadata `(metadata (category . eglot)
																	(display-sort-function . ,sort-completions)))
						 (context (eglot--completion-context))
						 (cached-proxies :none)
						 (proxies
							(lambda ()
								(if (consp cached-proxies) (cdr cached-proxies)
									(progn
										(setq cached-proxies
													(cons context
																(mapcar
																 (jsonrpc-lambda
																		 (&rest item &key label insertText insertTextFormat
																						textEdit &allow-other-keys)
																	 (let ((proxy
																					;; Snippet or textEdit, it's safe to
																					;; display/insert the label since
																					;; it'll be adjusted.  If no usable
																					;; insertText at all, label is best,
																					;; too.
																					(cond ((or (eql insertTextFormat 2)
																										 textEdit
																										 (null insertText)
																										 (string-empty-p insertText)
																										 label)
																								 (string-trim-left label))
																								(t insertText))))
																		 (unless (zerop (length proxy))
																			 (put-text-property 0 1 'eglot--lsp-item item proxy))
																		 proxy))
																 ;; chick if cached completion still valid
																 (when
																		 (and (consp eglot--cached-completion)
																					(let* ((cached-context (car eglot--cached-completion))
																								 (equip? (eglot--same-contextp context cached-context)))
																						equip?))
																	 (cdr eglot--cached-completion)))))
										(message "new cached returned")
										(cdr cached-proxies)))))
						 (resolved (make-hash-table))
						 (resolve-maybe
							;; Maybe completion/resolve JSON object `lsp-comp' into
							;; another JSON object, if at all possible.  Otherwise,
							;; just return lsp-comp.
							(lambda (lsp-comp)
								(or (gethash lsp-comp resolved)
										(setf (gethash lsp-comp resolved)
													(if (and (eglot--server-capable :completionProvider
																													:resolveProvider)
																	 (plist-get lsp-comp :data))
															(jsonrpc-request server :completionItem/resolve
																							 lsp-comp :cancel-on-input t)
														lsp-comp)))))
						 (bounds (bounds-of-thing-at-point 'symbol)))
				(list
				 (or (car bounds) (point))
				 (or (cdr bounds) (point))
				 (lambda (probe pred action)
					 (cond
						((eq action 'metadata) metadata)               ; metadata
						((eq action 'lambda)                           ; test-completion
						 (test-completion probe (funcall proxies)))
						((eq (car-safe action) 'boundaries) nil)       ; boundaries
						((null action)                                 ; try-completion
						 (try-completion probe (funcall proxies)))
						((eq action t)                                 ; all-completions
						 (all-completions
							""
							(funcall proxies)
							(lambda (proxy)
								(let* ((item (get-text-property 0 'eglot--lsp-item proxy))
											 (filterText (plist-get item :filterText)))
									(and (or (null pred) (funcall pred proxy))
											 (string-prefix-p
												probe (or filterText proxy) completion-ignore-case))))))))
				 :annotation-function
				 (lambda (proxy)
					 (eglot--dbind ((CompletionItem) detail kind)
							 (get-text-property 0 'eglot--lsp-item proxy)
						 (let* ((detail (and (stringp detail)
																 (not (string= detail ""))
																 detail))
										(annotation
										 (or detail
												 (cdr (assoc kind eglot--kind-names)))))
							 (when annotation
								 (concat " "
												 (propertize annotation
																		 'face 'font-lock-function-name-face))))))
				 :company-kind
				 ;; Associate each lsp-item with a lsp-kind symbol.
				 (lambda (proxy)
					 (when-let* ((lsp-item (get-text-property 0 'eglot--lsp-item proxy))
											 (kind (alist-get (plist-get lsp-item :kind)
																				eglot--kind-names)))
						 (pcase kind
							 ("EnumMember" 'enum-member)
							 ("TypeParameter" 'type-parameter)
							 (_ (intern (downcase kind))))))
				 :company-deprecated
				 (lambda (proxy)
					 (when-let ((lsp-item (get-text-property 0 'eglot--lsp-item proxy)))
						 (or (seq-contains-p (plist-get lsp-item :tags)
																 1)
								 (eq t (plist-get lsp-item :deprecated)))))
				 :company-docsig
				 ;; FIXME: autoImportText is specific to the pyright language server
				 (lambda (proxy)
					 (when-let* ((lsp-comp (get-text-property 0 'eglot--lsp-item proxy))
											 (data (plist-get (funcall resolve-maybe lsp-comp) :data))
											 (import-text (plist-get data :autoImportText)))
						 import-text))
				 :company-doc-buffer
				 (lambda (proxy)
					 (let* ((documentation
									 (let ((lsp-comp (get-text-property 0 'eglot--lsp-item proxy)))
										 (plist-get (funcall resolve-maybe lsp-comp) :documentation)))
									(formatted (and documentation
																	(eglot--format-markup documentation))))
						 (when formatted
							 (with-current-buffer (get-buffer-create " *eglot doc*")
								 (erase-buffer)
								 (insert formatted)
								 (current-buffer)))))
				 :company-require-match 'never
				 :company-prefix-length
				 (save-excursion
					 (when (car bounds) (goto-char (car bounds)))
					 (when (listp completion-capability)
						 (looking-back
							(regexp-opt
							 (cl-coerce (cl-getf completion-capability :triggerCharacters) 'list))
							(eglot--bol))))
				 :exit-function
				 (lambda (proxy status)
					 (when (memq status '(finished exact))
						 ;; To assist in using this whole `completion-at-point'
						 ;; function inside `completion-in-region', ensure the exit
						 ;; function runs in the buffer where the completion was
						 ;; triggered from.  This should probably be in Emacs itself.
						 ;; (github#505)
						 (with-current-buffer (if (minibufferp)
																			(window-buffer (minibuffer-selected-window))
																		(current-buffer))
							 (eglot--dbind ((CompletionItem) insertTextFormat
															insertText textEdit additionalTextEdits label)
									 (funcall
										resolve-maybe
										(or (get-text-property 0 'eglot--lsp-item proxy)
												;; When selecting from the *Completions*
												;; buffer, `proxy' won't have any properties.
												;; A lookup should fix that (github#148)
												(get-text-property
												 0 'eglot--lsp-item
												 (cl-find proxy (funcall proxies) :test #'string=))))
								 (let ((snippet-fn (and (eql insertTextFormat 2)
																				(eglot--snippet-expansion-fn))))
									 (cond (textEdit
													;; Undo (yes, undo) the newly inserted completion.
													;; If before completion the buffer was "foo.b" and
													;; now is "foo.bar", `proxy' will be "bar".  We
													;; want to delete only "ar" (`proxy' minus the
													;; symbol whose bounds we've calculated before)
													;; (github#160).
													(delete-region (+ (- (point) (length proxy))
																						(if bounds
																								(- (cdr bounds) (car bounds))
																							0))
																				 (point))
													(eglot--dbind ((TextEdit) range newText) textEdit
														(pcase-let ((`(,beg . ,end)
																				 (eglot--range-region range)))
															(delete-region beg end)
															(goto-char beg)
															(funcall (or snippet-fn #'insert) newText))))
												 (snippet-fn
													;; A snippet should be inserted, but using plain
													;; `insertText'.  This requires us to delete the
													;; whole completion, since `insertText' is the full
													;; completion's text.
													(delete-region (- (point) (length proxy)) (point))
													(funcall snippet-fn (or insertText label))))
									 (when (cl-plusp (length additionalTextEdits))
										 (eglot--apply-text-edits additionalTextEdits)))
								 (eglot--signal-textDocument/didChange)))))))))
	)

(use-package consult-eglot
  :ensure t)

(when (< emacs-major-version 30)
	(use-package eglot-booster
		:ensure (:host github :repo "jdtsmith/eglot-booster")
		:after eglot
		:hook (elpaca-after-init . eglot-booster-mode)))

(provide 'init-lsp)
;;; init-lsp.el ends here
