;;; verus-mode --- Support for Verus programming -*- lexical-binding: t; indent-tabs-mode: nil; no-byte-compile: t; -*-

;; Copyright (C) 2023 Jay Bosamiya
;; Author: Jay Bosamiya <verus@jaybosamiya.com>
;; URL: https://github.com/jaybosamiya/verus-mode.el

;; Created: 13 Feb 2023
;; Version: 0.1
;; Package-Requires: ((emacs "28.2") (rustic "3.0") (f "0.20.0"))
;; Keywords: convenience, languages

;; This file is not part of GNU Emacs.

;; This file is distributed under the BSD 3-Clause License (the "License");
;; you may not use this file except in compliance with the License.

;;; Commentary:

;; This file implements support for Verus programming in Emacs, including:
;;
;; * Invoking Verus verification
;; * TODO Syntax highlighting
;; * TODO Unicode math (prettify-symbols-mode)
;; * TODO Relative indentation
;; * TODO Real-time verification (Flycheck)

;; Note: Byte-compilation is temporarily disabled on this package, to make it
;; easier to develop verus-mode. It will be re-enabled at some point.


;;; Code:

;;; Imports

(require 'rustic)
(require 'f)

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

(defcustom verus-verify-location "source/tools/rust-verify.sh"
  "Where to find the Verus verification script, relative to `verus-home'."
  :group 'verus
  :type 'file
  :risky t)

(defcustom verus-analyzer nil
  "Where to find Verus Analyzer.
Specifically, this must be an absolute path pointing to the
directory containing a checkout of
https://github.com/verus-lang/rust-analyzer/tree/verus that has
had `cargo build --release' run in it."
  :group 'verus
  :type 'directory
  :risky t)

;;; Keymaps

(defvar verus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c C-c") 'verus-run)
    map))

;;; Syntax highlighting

;; TODO FIXME: Actually do this properly, rather than by using highlights
(defun verus--syntax-highlight ()
  (highlight-regexp "\\_<assume\\_>" font-lock-warning-face)
  (highlight-regexp "\\_<assert\\_>" font-lock-keyword-face)
  (highlight-regexp "\\_<ensures\\_>" font-lock-keyword-face)
  (highlight-regexp "\\_<requires\\_>" font-lock-keyword-face)
  (highlight-regexp "\\_<invariant\\_>" font-lock-keyword-face)
  (highlight-regexp "\\_<spec\\_>" font-lock-keyword-face)
  (highlight-regexp "\\_<proof\\_>" font-lock-keyword-face)
  (highlight-regexp "\\_<exec\\_>" font-lock-keyword-face)
  (highlight-regexp "\\_<open\\_>" font-lock-keyword-face)
  (highlight-regexp "\\_<closed\\_>" font-lock-keyword-face)
  (highlight-regexp "\\_<decreases\\_>" font-lock-keyword-face)
  (highlight-regexp "\\_<int\\_>" font-lock-keyword-face)
  (highlight-regexp "\\_<nat\\_>" font-lock-keyword-face))

;;; Mode definition

;; TODO FIXME: Get rustic to actually use the right lsp server
(defvar-local verus--old-lsp-server rustic-lsp-server)
(defvar-local verus--old-rutic-analyzer-command rustic-analyzer-command)
(defvar-local verus--rust-verify nil)

(defun verus--setup ()
  "Setup Verus mode."
  (if (not verus-home)
      (setq verus-home (getenv "VERUS_HOME")))
  (if (or (not verus-home) (not (file-exists-p verus-home)))
      (error "Verus home directory %s does not exist." verus-home))
  (setq verus--rust-verify (f-join verus-home verus-verify-location))
  (if (not verus-analyzer)
      (message "The variable verus-analyzer must be set to properly use Verus mode.")
    (let ((analyzer (concat verus-analyzer "/target/release/rust-analyzer")))
      (if (not (file-exists-p analyzer))
          (message "The file %s does not exist.  Are you sure you ran 'cargo build --release' in the correct path?" analyzer)
        (setq-local rustic-lsp-server 'rust-analyzer)
        (setq-local rustic-analyzer-command analyzer))))
  ;; TEMPORARY FIXME: Disable format-on-save until we have verusfmt
  (setq-local rustic-format-on-save nil)
  ;; Enable syntax highlighting
  (verus--syntax-highlight))

(defun verus--cleanup ()
  "Cleanup Verus mode."
  (setq-local rustic-lsp-server verus--old-lsp-server)
  (setq-local rustic-analyzer-command verus--old-rutic-analyzer-command))

;;;###autoload
(define-derived-mode verus-mode rustic-mode "Verus"
  "Major mode for Verus code.

\\{rustic-mode-map}"
  :group 'verus
  :keymap verus-mode-map
  (verus--setup))
;; TODO: Handle cleanup?

(defun verus--is-verus-file ()
  "Return non-nil if the current buffer is a Verus file.
This is done by checking if the file contains a string that is
'verus!' followed by any number of spaces, and then an opening
curly brace"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^verus! *{$" nil t)))

;;;###autoload
(add-to-list 'magic-mode-alist (cons #'verus--is-verus-file #'verus-mode))

;;; Commands

(defun verus--crate-root-file ()
  "Find the root of the current crate. Usually either `main.rs' or `lib.rs'."
  (let ((root (locate-dominating-file default-directory "Cargo.toml")))
    (unless root
      (error "Not in a crate"))
    (let ((lib (f-join root "src/lib.rs"))
          (main (f-join root "src/main.rs")))
      (if (file-exists-p lib)
          lib
        (if (file-exists-p main)
            main
          (error "Could not find crate root file."))))))

(defun verus-run ()
  "Run Verus on the current file."
  (interactive)
  (let ((file (buffer-file-name)))
    (if (not file)
        (message "Buffer is not visiting a file. Cannot run Verus.")
      (let ((default-directory (f-dirname file)))
        (compile
         (concat (shell-quote-argument verus--rust-verify)
                 " "
                 (shell-quote-argument (verus--crate-root-file))))))))

(provide 'verus-mode)
;;; verus-mode.el ends here
