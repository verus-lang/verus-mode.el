;;; verus-mode --- Support for Verus programming -*- lexical-binding: t; indent-tabs-mode: nil; no-byte-compile: t; -*-

;; Copyright (C) 2023 Jay Bosamiya
;; Author: Jay Bosamiya <verus@jaybosamiya.com>
;; URL: https://github.com/jaybosamiya/verus-mode.el

;; Created: 13 Feb 2023
;; Version: 0.1
;; Package-Requires: ((emacs "28.2") (rustic "3.0"))
;; Keywords: convenience, languages

;; This file is not part of GNU Emacs.

;; This file is distributed under the BSD 3-Clause License (the "License");
;; you may not use this file except in compliance with the License.

;;; Commentary:

;; This file implements support for Verus programming in Emacs, including:
;;
;; * TODO Syntax highlighting
;; * TODO Unicode math (prettify-symbols-mode)
;; * TODO Relative indentation
;; * TODO Real-time verification (Flycheck)

;; Note: Byte-compilation is temporarily disabled on this package, to make it
;; easier to develop verus-mode. It will be re-enabled at some point.


;;; Code:

;;; Imports

(require 'auto-minor-mode)
(require 'rustic)

;;; Customization

(defgroup verus nil
  "Verus mode."
  :group 'languages)

(defcustom verus-home nil
  "Where to find Verus.
May be either nil (use $VERUS_HOME) or an absolute path."
  :group 'verus
  :type 'directory
  :risky t)

(defcustom verus-analyzer nil
  "Where to find Verus.
Must be an an absolute path."
  :group 'verus
  :type 'directory
  :risky t)

;;; Keymaps

(defvar verus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'verus-run)
    map))

;;; Mode definition

(defvar-local verus--old-lsp-server rustic-lsp-server)

(defun verus--setup ()
  "Setup Verus mode."
  ;; TODO FIXME
  (setq-local rustic-lsp-server 'rust-analyzer))

(defun verus--cleanup ()
  "Cleanup Verus mode."
  (setq-local rustic-lsp-server verus--old-lsp-server))

(define-minor-mode verus-mode
  "Toggle Verus mode."
        :lighter " verus"
        :keymap verus-mode-map
        :group 'verus
        (if verus-mode
            (verus--setup)
          (verus--cleanup)))

;;; Commands

(defun verus-run ()
  "Run Verus on the current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (not file)
        (message "Buffer is not visiting a file")
      (message "NOT YET IMPLEMENTED"))))

;;; Auto-minor-mode

(defun verus--is-verus-file ()
  "Return non-nil if the current buffer is a Verus file.
This is done by checking if the file contains a string that is
'verus!' followed by any number of spaces, and then an opening
curly brace"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^verus! *{" nil t)))

(add-to-list
 'auto-minor-mode-magic-alist
 '(verus--is-verus-file . verus-mode))

(provide 'verus-mode)
;;; verus-mode.el ends here
