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
https://github.com/verus-lang/rust-analyzer/tree/november-08-2022
that has had `cargo xtask dist && gunzip
./dist/rust-analyzer-x86_64-apple-darwin.gz && chmod +x
./dist/rust-analyzer-x86_64-apple-darwin' run in it."
  :group 'verus
  :type 'directory
  :risky t)

;;; Keymaps

(defvar verus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c C-c") 'verus-run-on-file)
    (define-key map (kbd "C-c C-c C-S-c") 'verus-run-on-crate)
    map))

;;; Syntax highlighting

;; TODO FIXME: Actually do this properly, rather than by using highlights
(defun verus--syntax-highlight ()
  "Highlight Verus keywords."
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

;;; Compilation mode setup

(defun verus--compilation-mode-setup ()
  "Setup compilation mode for Verus."
  ;; Recognize Verus errors that look like:
  ;; --> [filename]:[line]:[column]
  (if (not (assoc 'verus compilation-error-regexp-alist-alist))
      (add-to-list 'compilation-error-regexp-alist-alist
                   '(verus
                     "^ *--> \\(.+\\):\\([0-9]+\\):\\([0-9]+\\)"
                     1 2 3)))
  (if (not (member 'verus compilation-error-regexp-alist))
      (add-to-list 'compilation-error-regexp-alist 'verus)))

;;; Mode definition

;; TODO FIXME: Get rustic to actually use the right lsp server
(defvar-local verus--old-lsp-server rustic-lsp-server)
(defvar-local verus--old-rutic-analyzer-command rustic-analyzer-command)
(defvar-local verus--old-lsp-rust-analyzer-server-command nil) ; Copied at start
(defvar-local verus--rust-verify nil)

(defun verus--setup ()
  "Setup Verus mode."
  (if (not verus-home)
      (setq verus-home (getenv "VERUS_HOME")))
  (if (or (not verus-home) (not (file-exists-p verus-home)))
      (error "Verus home directory %s does not exist" verus-home))
  (setq verus--rust-verify (f-join verus-home verus-verify-location))
  (if (not verus-analyzer)
      (message "The variable verus-analyzer must be set to properly use Verus mode.")
    ;; FIXME: Use the right LSP server based on machine, currently this is
    ;; hardcoded to macOS.
    (let ((analyzer (concat verus-analyzer "/dist/rust-analyzer-x86_64-apple-darwin")))
      (if (not (file-exists-p analyzer))
          (message "The file %s does not exist.  Are you sure you ran 'cargo xtask dist' etc. in the correct path?" analyzer)
        (setq-local rustic-lsp-server 'rust-analyzer)
        (setq-local verus--old-lsp-rust-analyzer-server-command lsp-rust-analyzer-server-command)
        (setq-local lsp-rust-analyzer-server-command (list analyzer))
        (setq-local rustic-analyzer-command (list analyzer)))))
  ;; TEMPORARY FIXME: Disable format-on-save until we have verusfmt
  (setq-local rustic-format-on-save nil)
  (verus--syntax-highlight)
  (verus--compilation-mode-setup))

(defun verus--cleanup ()
  "Cleanup Verus mode."
  (setq-local rustic-lsp-server verus--old-lsp-server)
  (setq-local rustic-analyzer-command verus--old-rutic-analyzer-command)
  (setq-local lsp-rust-analyzer-server-command verus--old-lsp-rust-analyzer-server-command))

;;;###autoload
(define-minor-mode verus-mode
  "Minor mode for verus code."
  :group 'verus
  :keymap verus-mode-map
  (if verus-mode
      (verus--setup)
    (verus--cleanup)))

(defun verus--is-verus-file ()
  "Return non-nil if the current buffer is a Verus file.
This is done by checking if the file contains a string that is
'verus!' followed by any number of spaces, and then an opening
curly brace"
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^verus! *{$" nil t)))

(defun verus--verus-mode-or-rust-mode ()
  "Decide whether to use Verus mode or Rust mode."
  (if (verus--is-verus-file)
      (verus-mode)
    (rust-mode)))

(defun verus--verus-mode-or-rustic-mode ()
  "Decide whether to use Verus mode or Rustic mode."
  (if (verus--is-verus-file)
      (verus-mode)
    (rustic-mode)))

;; ;;;###autoload
;; (progn
;;   ;; Note: This is a hack, we probably would like to do this with
;;   ;; `magic-mode-alist', but for now, this works.
;;   ;;
;;   ;; In the future, we might wish to just have Verus use a separate file
;;   ;; extension, such as `.vrs'.
;;   ;;
;;   ;; Any places that use `rust-mode' or `rustic-mode' in `auto-mode-alist' are
;;   ;; updated to instead invoke `verus--verus-mode-or-rust-mode' or
;;   ;; `verus--verus-mode-or-rustic-mode' respectively.
;;   (dolist (mode auto-mode-alist)
;;     (cond ((eq (cdr mode) 'rust-mode)
;;            (setcdr mode 'verus--verus-mode-or-rust-mode))
;;           ((eq (cdr mode) 'rustic-mode)
;;            (setcdr mode 'verus--verus-mode-or-rustic-mode)))))

;;;###autoload
(eval-after-load 'rustic-mode
  '(progn
     (add-hook 'rustic-mode-hook
               (lambda ()
                 (if (verus--is-verus-file)
                     (verus-mode))))))

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
          (error "Could not find crate root file"))))))

(defun verus-run-on-crate (prefix)
  "Run Verus on the current crate.

If PREFIX is non-nil, then run ask for the command to run."
  (interactive "p")
  (let ((file (buffer-file-name)))
    (if (not file)
        (message "Buffer is not visiting a file. Cannot run Verus.")
      (let ((default-directory (f-dirname file)))
        (let ((compilation-command
               (concat (shell-quote-argument verus--rust-verify)
                       " "
                       (shell-quote-argument (verus--crate-root-file)))))
          (compile (if (= prefix 1)
                       compilation-command
                     (read-shell-command "Run Verus: " compilation-command))))))))

(defun verus-run-on-file (prefix)
  "Run Verus on the current file.

If PREFIX is non-nil, then run ask for the command to run."
  (interactive "p")
  (let ((file (buffer-file-name)))
    (if (not file)
        (message "Buffer is not visiting a file. Cannot run Verus.")
      (let ((default-directory (f-dirname file)))
        (let ((compilation-command
               (concat (shell-quote-argument verus--rust-verify)
                       " "
                       (shell-quote-argument (verus--crate-root-file))
                       (if (string= (verus--crate-root-file) file)
                           " --verify-root"
                         (concat " --verify-module " (shell-quote-argument (f-base file)))))))
          (compile (if (= prefix 1)
                       compilation-command
                     (read-string "Run Verus: " compilation-command))))))))

;; ;; Temporary
;; (add-to-list 'lsp-language-id-configuration
;;              '(verus-mode . "verus"))

(provide 'verus-mode)
;;; verus-mode.el ends here
