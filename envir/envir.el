;;; envir.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024 jidibinlin
;;
;; Author: jidibinlin <2694273649@qq.com>
;; Maintainer: jidibinlin <2694273649@qq.com>
;; Created: November 01, 2024
;; Modified: November 01, 2024
;; Version: 0.0.1
;; Keywords: Symbol’s value as variable is void: finder-known-keywords
;; Homepage: https://github.com/qibin/envir
;; Package-Requires: ((emacs "24.3"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:

(defun is-wsl ()
  "`is-wsl' check if runing on windows-subsystem-linux."
  (when (eq system-type  'gnu/linux)
    (let ((kernel-release (with-temp-buffer
                            (insert-file-contents "/proc/sys/kernel/osrelease")
                            (buffer-string))))
      (string-match "microsoft" kernel-release))))


(defconst conia-system
  (cond
   ((is-wsl) 'wsl)
   ((equal system-type 'windows-nt) 'windows)
   ((equal system-type 'gnu/linux)  'linux)
   ((equal system-type 'darwin)  'macos)
   (t "unknown"))
  "`conia-system' current system type.")

(defconst conia-sysis-windows (equal conia-system 'windows))
(defconst conia-sysis-linux (equal conia-system 'linux))
(defconst conia-sysis-mac  (equal conia-system 'macos))
(defconst conia-syis-wsl (when (equal conia-system 'wsl) 'wsl))


(provide 'envir)
;;; envir.el ends here
