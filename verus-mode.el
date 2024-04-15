;;; verus-mode.el --- Support for Verus programming -*- lexical-binding: t; indent-tabs-mode: nil; -*-

;; Copyright (C) 2023 Jay Bosamiya
;; Author: Jay Bosamiya <verus@jaybosamiya.com>
;; URL: https://github.com/verus-lang/verus-mode.el

;; Created: 13 Feb 2023
;; Version: 0.6.4
;; Package-Requires: ((emacs "28.2") (rustic "3.0") (f "0.20.0") (flycheck "30.0") (dumb-jump "0.5.4") (toml "0.0.1"))
;; Keywords: convenience, languages

;; This file is not part of GNU Emacs.

;; This file is distributed under the BSD 3-Clause License (the "License");
;; you may not use this file except in compliance with the License.

;;; Commentary:

;; This file implements support for Verus programming in Emacs, including:
;;
;; * Invoking Verus verification
;; * Syntax highlighting
;; * Jump-to-definition (dumb-jump)
;; * Unicode math (prettify-symbols-mode)
;; * TODO Relative indentation
;; * Real-time verification (Flycheck)


;;; Code:

;;; Imports

(require 'rustic)
(require 'f)
(require 'flycheck)
(require 'dumb-jump)
(require 'toml)

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

(defcustom verus-verify-locations
  '("source/target-verus/release/verus"
    "source/target-verus/debug/verus"
    "source/target-verus/release/verus.exe"
    "source/target-verus/debug/verus.exe"
    "source/tools/rust-verify.sh")
  "Locations to find the Verus verification script, relative to `verus-home'.

These are checked in-order to figure out how to run Verus."
  :group 'verus
  :type '(repeat string)
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

(defcustom verus-auto-check-version t
  "Automatic checks for new version of verus-mode.el.

If non-nil, automatically check for a new version of
verus-mode.el, once per Emacs session."
  :group 'verus
  :type 'boolean)

(defcustom verus-auto-check-version-interval (* 60 60 24)
  "How often to check for a new version of verus-mode.el, in seconds.

Ignored if `verus-auto-check-version' is nil. Defaults to once per day."
  :group 'verus
  :type 'integer)

(defcustom verus-enable-experimental-features nil
  "If non-nil, enable experimental features.

These features are not guaranteed to work, and may change or be
removed at any time."
  :group 'verus
  :type 'boolean)

;;; Keymaps

(defvar verus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c C-c") 'verus-run-on-file)
    (define-key map (kbd "C-c C-c C-p") 'verus-run-on-file-with-profiling)
    (define-key map (kbd "C-c C-c C-S-c") 'verus-run-on-crate)
    (define-key map (kbd "C-c C-n") 'flycheck-next-error)
    (define-key map (kbd "C-c C-p") 'flycheck-previous-error)
    map))

;;; Syntax highlighting

(defun verus--syntax-highlight ()
  "Setup syntax highlighting for Verus keywords."
  (font-lock-add-keywords
   nil
   `((,(regexp-opt '("assert"
                     "assume"
                     "closed"
                     "decreases"
                     "ensures"
                     "exec"
                     "ghost"
                     "has"
                     "invariant"
                     "is"
                     "matches"
                     "open"
                     "proof"
                     "recommends"
                     "requires"
                     "reveal"
                     "reveal_with_fuel"
                     "spec"
                     "tracked"
                     "via"
                     "when") 'symbols)
      . font-lock-keyword-face)))
  (font-lock-add-keywords
   nil
   `((,(regexp-opt '("int" "nat") 'symbols)
      . font-lock-type-face))))

;;; Unicode math (prettify-symbols-mode)

(defcustom verus-symbols-alist
  '(("exists" . ?∃) ("forall" . ?∀)
    ("nat" . ?ℕ) ("int" . ?ℤ)
    ("<==>" . ?⟺) ("==>" . ?⟹)
    ("===" . ?⩶)
    ("&&" . ?∧) ("||" . ?∨)
    ("&&&" . ?⨇) ("|||" . ?⨈)
    ("old" . ?⥀))
  "Verus symbols."
  :group 'verus
  :type 'alist)

(defun verus--prettify-symbols-setup ()
  "Setup prettify-symbols for use with Verus."
  (when (and (boundp 'prettify-symbols-alist)
             (fboundp 'prettify-symbols-mode))
    (setq-local prettify-symbols-alist (append verus-symbols-alist
                                               prettify-symbols-alist))
    (prettify-symbols-mode 1)))

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

;;; Dumb-jump setup

(defun verus--dumb-jump-setup ()
  "Setup dumb-jump for Verus."
  ;; (let ((ext '(:language "verus" :ext "rs" :agtype "rust" :rgtype "rust")))
  ;;   (if (not (member ext dumb-jump-language-file-exts))
  ;;           (add-to-list 'dumb-jump-language-file-exts ext)))
  (if (not (member #'dumb-jump-xref-activate xref-backend-functions))
      (let ((local-xref-backend-functions xref-backend-functions))
        (add-to-list 'local-xref-backend-functions #'dumb-jump-xref-activate)
        (setq-local xref-backend-functions local-xref-backend-functions)))
  (if (fboundp 'xref-show-definitions-completing-read)
      (setq-local xref-show-definitions-function #'xref-show-definitions-completing-read))
  (if (not (member "Cargo.toml" dumb-jump-project-denoters))
      (add-to-list 'dumb-jump-project-denoters "Cargo.toml")))

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
      (error "Verus home directory %s does not exist" verus-home))
  (setq verus--rust-verify
        (cl-find-if #'file-executable-p
                    (mapcar (lambda (loc) (f-join verus-home loc))
                            verus-verify-locations)))
  (when (not verus--rust-verify)
    (error "Could not find way to execute Verus in any of the following locations: %s"
           (mapcar (lambda (loc) (f-join verus-home loc))
                   verus-verify-locations)))
  (when (string-suffix-p "rust-verify.sh" verus--rust-verify)
    (message "WARNING: You are using an old version of Verus.  This may soon be unsupported.  Please update to the latest version."))
  (when (not verus-enable-experimental-features)
    ;; Disable rustic's lsp setup, since we don't yet have LSP support in
    ;; non-experimental mode; this way, we remove the annoying lsp pop up.
    (setq-local rustic-lsp-client nil))
  (when verus-enable-experimental-features
    ;; NOTE: This is marked as experimental because it doesn't work properly,
    ;; should be fixed in the future, and stabilized.
    (if (not verus-analyzer)
        (message "The variable verus-analyzer must be set to properly use Verus mode.")
      ;; FIXME: Use the right LSP server based on machine, currently this is
      ;; hardcoded to macOS.
      (let ((analyzer (concat verus-analyzer "/dist/rust-analyzer-x86_64-apple-darwin")))
        (if (not (file-exists-p analyzer))
            (message "The file %s does not exist.  Are you sure you ran 'cargo xtask dist' etc. in the correct path?" analyzer)
          (setq-local rustic-lsp-server 'rust-analyzer)
          (setq-local rustic-analyzer-command analyzer)))))
  ;; TEMPORARY FIXME: Disable format-on-save until we have verusfmt
  (setq-local rustic-format-on-save nil)
  (verus--syntax-highlight)
  (verus--compilation-mode-setup)
  (verus--prettify-symbols-setup)
  (verus--setup-version-check)
  (verus--flycheck-setup)
  (verus--dumb-jump-setup))

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

This is done by checking for a collection of things that tend to
show up in Verus files."
  (save-excursion
    (goto-char (point-min))
    (or
     ;; Check for a bunch of common Verus markers
     (re-search-forward "^[ \t]*verus![ \t]*{" nil t)
     (re-search-forward "^[ \t]*test_verify_one_file![ \t]*{" nil t)
     (re-search-forward "^[ \t]*use[ \t]+vstd::" nil t)
     ;; Or if none exist, then see if we have extra args specified in the
     ;; Cargo.toml, which indicates that the files in the directory should be
     ;; marked as Verus files.
     (verus--extra-args-from-cargo-toml))))

(defun verus--is-main-file ()
  "Return non-nil if the current buffer is a Verus main file.
This is done by checking if the file contains a `fn main` function."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "[ \t]*fn[ \t]+main[ \t]*(" nil t)))

(defun verus--is-a-verus-example-file ()
  "Return non-nil if the current buffer is in an example directory in Verus."
  (when-let ((root (locate-dominating-file default-directory "source/rust_verify/example")))
    (let ((example-dir (f-canonical (f-join root "source/rust_verify/example"))))
      (string-prefix-p example-dir (f-canonical (f-dirname (buffer-file-name)))))))

(defun verus--has-modules-in-file ()
  "Return non-nil if the current buffer has modules in it."
  (save-excursion
    (goto-char (point-min))
    (re-search-forward "^[ \t]*mod[ \t]+" nil t)))

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

;;;###autoload
(progn
  ;; Note: This is a hack, we probably would like to do this with
  ;; `magic-mode-alist', but for now, this works.
  ;;
  ;; In the future, we might wish to just have Verus use a separate file
  ;; extension, such as `.vrs'.
  ;;
  ;; Any places that use `rust-mode' or `rustic-mode' in `auto-mode-alist' are
  ;; updated to instead invoke `verus--verus-mode-or-rust-mode' or
  ;; `verus--verus-mode-or-rustic-mode' respectively.
  (dolist (mode auto-mode-alist)
    (cond ((eq (cdr mode) 'rust-mode)
           (setcdr mode 'verus--verus-mode-or-rust-mode))
          ((eq (cdr mode) 'rustic-mode)
           (setcdr mode 'verus--verus-mode-or-rustic-mode)))))

;;; Commands

(defvar-local verus--reported-non-crate-file nil
  "Whether we have already reported that the current file is not a crate.")

(defun verus--crate-root-file ()
  "Find the root of the current crate. Usually either `main.rs' or `lib.rs'."
  (let ((is-main (verus--is-main-file)))
    (if (or is-main (verus--is-a-verus-example-file))
        (buffer-file-name)
      (if-let ((vstd-root (locate-dominating-file default-directory "vstd.rs")))
          (progn (message "Found vstd.rs, assuming we are in vstd.")
                 (f-join vstd-root "vstd.rs"))
        (if-let ((root (locate-dominating-file default-directory "Cargo.toml")))
            (let ((lib (f-join root "src/lib.rs"))
                  (main (f-join root "src/main.rs")))
              (cond ((file-exists-p lib) lib)
                    ((file-exists-p main) main)
                    (t (error "Could not find crate root file"))))
          (progn
            (when (not verus--reported-non-crate-file)
              (when (not is-main)
                (message "Not in a crate, using current file as root"))
              (setq-local verus--reported-non-crate-file t))
            (buffer-file-name)))))))

(defun verus--extract-extra-args-from (toml-file)
  "Extract the `package.metadata.verus.ide.extra_args' string from the TOML-FILE."
  (when (and toml-file (file-exists-p toml-file))
    (let* ((toml (toml:read-from-file toml-file))
           (package (cdr (assoc "package" toml)))
           (metadata (cdr (assoc "metadata" package)))
           (verus (cdr (assoc "verus" metadata)))
           (ide (cdr (assoc "ide" verus)))
           (extra-args (cdr (assoc "extra_args" ide))))
      extra-args)))

(defun verus--path-shift-relative (path old new)
  "Shift relative PATH from OLD to NEW."
  (f-relative (f-join old path) new))

(defun verus--extra-args-from-cargo-toml ()
  "Return a list of extra arguments to pass to Verus.

This reads the `extra_args` key from the
`package.metadata.verus.ide` table in the Cargo.toml for the
current crate. It additionally updates any paths found to be
relative to the current working directory (while the original
ones are relative to the Cargo.toml)."
  (let* ((root (locate-dominating-file default-directory "Cargo.toml"))
         (extra-args-str (verus--extract-extra-args-from (f-join root "Cargo.toml")))
         (args (split-string-and-unquote (string-trim extra-args-str)))
         (cwd (f-full default-directory)))
    (when args
      (mapcar (lambda (arg)
                (let* ((xs (split-string arg "="))
                       (rxs (reverse xs))
                       (last (car rxs))
                       (rest (reverse (cdr rxs))))
                  (if (or (string-prefix-p "./" last)
                          (string-prefix-p "../" last))
                      (concat (string-join rest "=") "="
                              (verus--path-shift-relative last root cwd))
                    arg))) args))))

(defun verus--run-on-crate-command ()
  "Return the command to run Verus on the current crate.

Returns a list of command-line arguments. Expects to be run in a
buffer visiting the file, otherwise throws an error."
  (let ((file (buffer-file-name)))
    (when (not file)
      (error "Buffer is not visiting a file. Cannot run Verus"))
    (let ((default-directory (f-dirname file))
          (crate-root (verus--crate-root-file)))
      (append
       (list verus--rust-verify)
       (if (string-suffix-p "lib.rs" crate-root)
           (list "--crate-type=lib"))
       (if (string-suffix-p "vstd.rs" crate-root)
           (list "--crate-type=lib"
                 "--no-vstd"))
       (verus--extra-args-from-cargo-toml)
       (list crate-root)))))

(defun verus--current-module-name ()
  "Return the `::'-delimited name of current module.

Utilizes the current buffer's file name, but also accounts for
the path from the crate root file to the current buffer."
  (if (verus--has-modules-in-file)
      (error "UNREACHABLE. Should not invoke current-module-name on a file with submodules"))
  (let ((root (verus--crate-root-file))
        (buf (buffer-file-name)))
    (if (string= buf root)
        (error "UNREACHABLE. Should not invoke current-module-name on root of crate"))
    (let ((rel (f-no-ext (f-relative buf (f-dirname root)))))
      (replace-regexp-in-string
       "::mod$" ""
       (replace-regexp-in-string
        "/" "::" rel)))))

(defun verus--run-on-file-command ()
  "Return the command to run Verus on the current file.

Returns a list of command-line arguments. Expects to be run in a
buffer visiting the file, otherwise throws an error."
  (append
   (verus--run-on-crate-command)
   (cond
    ((verus--has-modules-in-file)
     (message
      (concat "File has modules, running on whole crate. "
              "Verus#385, once implemented, should support convenient submodules."))
     nil)
    ((string= (buffer-file-name) (verus--crate-root-file))
     (list "--verify-root"))
    (t
     (list "--verify-module" (verus--current-module-name))))))

(defun verus-run-on-crate (prefix)
  "Run Verus on the current crate.

If PREFIX is non-nil, then run ask for the command to run."
  (interactive "p")
  (let ((verus-command (with-demoted-errors "Verus error: %S"
                         (verus--run-on-crate-command))))
    (when verus-command
      (let ((compilation-command
             (mapconcat #'shell-quote-argument verus-command " ")))
        (compile (if (= prefix 1)
                     compilation-command
                   (read-shell-command "Run Verus: " compilation-command)))))))

(defun verus-run-on-file (prefix &optional extra-args)
  "Run Verus on the current file.

If PREFIX is non-nil, then run ask for the command to run.

If EXTRA-ARGS is non-nil, then add them to the command."
  (interactive "p")
  (let ((verus-command (with-demoted-errors "Verus error: %S"
                         (verus--run-on-file-command))))
    (when verus-command
      (let ((compilation-command
             (mapconcat #'shell-quote-argument
                        (append verus-command extra-args)
                        " ")))
        (compile (if (= prefix 1)
                     compilation-command
                   (read-shell-command "Run Verus: " compilation-command)))))))

(defun verus-run-on-file-with-profiling (prefix)
  "Run Verus on the current file, with profiling enabled.

If PREFIX is non-nil, then enable 'always profiling' mode."
  (interactive "p")
  (verus-run-on-file 1 (if (= prefix 1)
                           (list "--profile")
                         (list "--profile-all"))))

;;; Flycheck setup

(flycheck-define-checker verus
  "A Verus syntax checker using the Verus compiler."
  :command ("rust-verify.sh"
            (eval
             (let ((args (cdr (verus--run-on-file-command))))
               (seq-filter (lambda (x) (not (string= x "--expand-errors"))) args)))
            "--error-format=short"
            "--expand-errors")
  :error-patterns
  ((error (file-name) ":" line ":" column ": error[" (id (one-or-more (not (any "]")))) "]: " (message) line-end)
   (error (file-name) ":" line ":" column ": error: " (message) line-end)
   (warning (file-name) ":" line ":" column ": warning[" (id (one-or-more (not (any "]")))) "]: " (message) line-end)
   (warning (file-name) ":" line ":" column ": warning: " (message) line-end)
   (info (file-name) ":" line ":" column ": note: " (message) line-end))
  :predicate (lambda ()
               (and
                (flycheck-buffer-saved-p)
                (verus--is-verus-file)))
  :modes verus-mode)

(defun verus--flycheck-setup ()
  "Setup Flycheck for Verus."
  (setq flycheck-verus-executable verus--rust-verify)
  (add-to-list 'flycheck-checkers 'verus))

;;; Automatic version checking

(defun verus--verus-mode-el-current-version ()
  "Get the current version of verus-mode.el."
  (let ((version (with-temp-buffer
                   (insert-file-contents-literally (locate-library "verus-mode.el"))
                   (goto-char (point-min))
                   (re-search-forward ";; Version: \\([0-9.]+\\)")
                   (match-string 1))))
    (if (not version)
        (error "Could not find version in verus-mode.el")
      version)))

(defun verus--verus-mode-el-latest-available-version ()
  "Get the latest available version for verus-mode.el."
  (let ((version (with-temp-buffer
                   ;; Insert "https://raw.githubusercontent.com/verus-lang/verus-mode.el/main/verus-mode.el" into the buffer
                   ;; suppressing any messages.
                   (let ((url-show-status nil))
                     (url-insert-file-contents "https://raw.githubusercontent.com/verus-lang/verus-mode.el/main/verus-mode.el"))
                   (goto-char (point-min))
                   (re-search-forward ";; Version: \\([0-9.]+\\)")
                   (match-string 1))))
    (if (not version)
        (error "Could not find version in verus-mode.el")
      version)))

(defvar verus--verus-mode-el-last-version-check nil
  "The last time we checked for a new version of verus-mode.el.")

(defvar verus--verus-mode-el-last-version-check-file
  (f-join user-emacs-directory ".verus-mode.el-last-version-check")
  "The file where we store the last time we checked for a new version of verus-mode.el.")

(defun verus-check-version-now ()
  "Check for a new version of verus-mode.el, right now, even if we've already checked recently."
  (interactive)
  (let ((current (verus--verus-mode-el-current-version))
        (latest (verus--verus-mode-el-latest-available-version)))
    (if (version< current latest)
        (message "verus-mode.el: A new version is available: %s (you are using %s)" latest current)
      (message "verus-mode.el: You are using the latest version (%s)" current)))
  (setq verus--verus-mode-el-last-version-check (float-time))
  (with-temp-file verus--verus-mode-el-last-version-check-file
    (insert (number-to-string verus--verus-mode-el-last-version-check))))

(defun verus--maybe-verus-mode-el-check-version ()
  "Check for a new version of verus-mode.el. if we haven't checked recently."
  (when (or (not verus--verus-mode-el-last-version-check)
            (> (- (float-time) verus--verus-mode-el-last-version-check)
               verus-auto-check-version-interval))
    (if verus-auto-check-version
        (progn
          (message "verus-mode.el: Checking for a new version...")
          (verus-check-version-now))
      (message (concat
                "verus-mode.el: "
                "Automatic version checking is disabled, but strongly recommended. "
                "To enable it, set `verus-auto-check-version' to t.")))))

(defun verus--setup-version-check ()
  "Setup the version check, delayed until Emacs is idle for 5 seconds."
  (when (file-exists-p verus--verus-mode-el-last-version-check-file)
    (with-temp-buffer
      (insert-file-contents verus--verus-mode-el-last-version-check-file)
      (setq verus--verus-mode-el-last-version-check (read (current-buffer)))))
  (run-with-idle-timer 5 nil #'verus--maybe-verus-mode-el-check-version))

(provide 'verus-mode)
;;; verus-mode.el ends here
