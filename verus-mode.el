;;; verus-mode.el --- Support for Verus programming -*- lexical-binding: t; indent-tabs-mode: nil; -*-

;; Copyright (C) 2023 Jay Bosamiya
;; Author: Jay Bosamiya <verus@jaybosamiya.com>
;; URL: https://github.com/verus-lang/verus-mode.el

;; Created: 13 Feb 2023
;; Version: 0.9.1
;; Package-Requires: ((emacs "28.2") (rustic "3.0") (f "0.20.0") (flycheck "30.0") (dumb-jump "0.5.4") (tomlparse "1.0.0"))
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
(require 'tomlparse)

;;; TOML tree-sitter setup

(defun verus--ensure-toml-grammar ()
  "Ensure TOML tree-sitter grammar is installed for tomlparse.
Warns the user if tree-sitter is not available or if installation fails."
  (cond
   ;; Check if tree-sitter functions are available (Emacs 29+)
   ((not (and (fboundp 'treesit-available-p)
              (fboundp 'treesit-language-available-p)
              (fboundp 'treesit-install-language-grammar)))
    (display-warning
     'verus-mode
     (concat "Tree-sitter functions are not available. "
             "You may need Emacs 29 or later with tree-sitter support enabled. "
             "TOML parsing features may not work correctly. "
             "See https://www.masteringemacs.org/article/how-to-get-started-tree-sitter")
     :warning))
   ;; Check if tree-sitter is actually available on the system
   ((not (treesit-available-p))
    (display-warning
     'verus-mode
     (concat "Tree-sitter is not available on your system. "
             "You may need to install libtree-sitter or compile Emacs with tree-sitter support. "
             "TOML parsing features may not work correctly. "
             "See https://www.masteringemacs.org/article/how-to-get-started-tree-sitter")
     :warning))
   ;; Tree-sitter is available, check if TOML grammar is installed
   ((not (treesit-language-available-p 'toml))
    ;; Add TOML recipe to treesit-language-source-alist if not already present
    (unless (assoc 'toml treesit-language-source-alist)
      (add-to-list 'treesit-language-source-alist
                   '(toml "https://github.com/tree-sitter/tree-sitter-toml")))
    (condition-case err
        (progn
          (message "verus-mode.el: Installing TOML tree-sitter grammar...")
          (treesit-install-language-grammar 'toml)
          (message "verus-mode.el: TOML tree-sitter grammar installed successfully."))
      (error
       ;; Installation failed - check if it's due to missing C compiler
       (if (not (executable-find "cc"))
           ;; No C compiler available - provide manual installation instructions
           (display-warning
            'verus-mode
            (concat "Failed to install TOML tree-sitter grammar (no C compiler `cc` detected; you might be on Windows?).\n"
                    "To install the TOML grammar manually:\n"
                    "1. Download toml.dll from https://github.com/emacs-tree-sitter/tree-sitter-langs/releases\n"
                    "2. Create the directory: " (f-join user-emacs-directory "tree-sitter") "\n"
                    "3. Move toml.dll to: " (f-join user-emacs-directory "tree-sitter" "libtree-sitter-toml.dll") "\n"
                    "4. Restart Emacs\n"
                    "TOML parsing features may not work correctly until this is done.")
            :warning)
         ;; C compiler is available but installation still failed - rethrow
         (display-warning
          'verus-mode
          (format (concat "Failed to install TOML tree-sitter grammar: %s\n"
                          "TOML parsing features may not work correctly. "
                          "You may need to install it manually. "
                          "See https://github.com/johannes-mueller/tomlparse.el")
                  (error-message-string err))
          :warning)))))))

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

(defcustom verus-partial-verif-of-fn-is-starred nil
  "If non-nil, add stars `verus-run-function-at-point'.

The starred variant allows Verus to match against any function which has
the function name as a subset."
  :group 'verus
  :type 'boolean)

(defcustom verus-enable-experimental-features nil
  "If non-nil, enable experimental features.

These features are not guaranteed to work, and may change or be
removed at any time."
  :group 'verus
  :type 'boolean)

(defcustom verus-cargo-verus-arguments nil
  "Extra arguments to pass to `cargo verus verify', as a list of strings.

When non-nil, this list is appended directly to the `cargo verus verify'
invocation instead of the default `--' separator.  The list MUST contain
`--' at the correct position to separate cargo-verus flags from Verus
flags; an error is signalled if it does not.

Example `.dir-locals.el' usage:

  ((verus-mode . ((verus-cargo-verus-arguments
                   . (\"--features\" \"foo\" \"--\" \"--expand-errors\")))))

The `--features foo' part is interpreted by cargo-verus, while
`--expand-errors' is forwarded to the Verus binary."
  :group 'verus
  :type '(repeat string)
  :safe #'listp)

;;; Keymaps

(defvar verus-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c C-c") 'verus-run-on-file)
    (define-key map (kbd "C-c C-c C-p") 'verus-run-on-file-with-profiling)
    (define-key map (kbd "C-c C-c C-f") 'verus-run-on-function-at-point)
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
        (push #'dumb-jump-xref-activate xref-backend-functions)
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
  (verus--ensure-toml-grammar)
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
     (verus--extra-args-from-cargo-toml)
     ;; Or check if this is a cargo-verus project
     (when-let ((root (locate-dominating-file default-directory "Cargo.toml")))
       (verus--is-cargo-verus-project-p (f-join root "Cargo.toml"))))))

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

(defun verus--is-cargo-verus-project-p (toml-file)
  "Check if TOML-FILE indicates a cargo-verus project.
A project is cargo-verus-supported if its Cargo.toml contains
package.metadata.verus.verify as true."
  (when (and toml-file (file-exists-p toml-file))
    (let* ((toml (tomlparse-file toml-file :object-type 'alist))
           (package (cdr (assoc 'package toml)))
           (metadata (cdr (assoc 'metadata package)))
           (verus (cdr (assoc 'verus metadata)))
           (verify (cdr (assoc 'verify verus))))
      (and verify (eq verify t)))))

(defun verus--is-workspace-root-p (toml-file)
  "Check if TOML-FILE is a Cargo workspace root.
A Cargo.toml is a workspace root if it contains a [workspace] section."
  (when (and toml-file (file-exists-p toml-file))
    (let* ((toml (tomlparse-file toml-file :object-type 'alist))
           (workspace (assoc 'workspace toml)))
      (not (null workspace)))))

(defun verus--find-workspace-root (start-dir)
  "Find the workspace root starting from START-DIR.
Walks up the directory tree looking for a Cargo.toml with [workspace] section.
Returns the directory containing the workspace root Cargo.toml, or nil if not found."
  (when-let ((cargo-toml (locate-dominating-file start-dir "Cargo.toml")))
    (let ((toml-path (f-join cargo-toml "Cargo.toml")))
      (if (verus--is-workspace-root-p toml-path)
          cargo-toml
        ;; Not a workspace root, check parent directories
        (let ((parent (f-parent cargo-toml)))
          (when (and parent (not (string= parent cargo-toml)))
            (verus--find-workspace-root parent)))))))

(defun verus--get-cargo-verus-root-directory ()
  "Get the root directory for running cargo verus commands.
For workspace members, returns the workspace root directory.
For standalone packages, returns the package root directory.
Returns nil if not in a cargo-verus project."
  (when-let* ((file (or (buffer-file-name) default-directory))
              (cargo-toml-root (locate-dominating-file (if (file-directory-p file) file (f-dirname file)) "Cargo.toml"))
              (cargo-toml (f-join cargo-toml-root "Cargo.toml")))
    (when (verus--is-cargo-verus-project-p cargo-toml)
      ;; This is a cargo-verus project, check if it's in a workspace
      (or (verus--find-workspace-root cargo-toml-root)
          cargo-toml-root))))

(defun verus--is-in-cargo-verus-workspace ()
  "Check if the current buffer is in a cargo-verus workspace.
A workspace is different from a single crate - it has a workspace root
that is different from the package root.
Returns non-nil if in a workspace, nil otherwise.

This exists entirely due to Verus#1938, and will be removed once that is resolved."
  (when-let* ((file (or (buffer-file-name) default-directory))
              (cargo-toml-root (locate-dominating-file (if (file-directory-p file) file (f-dirname file)) "Cargo.toml"))
              (cargo-toml (f-join cargo-toml-root "Cargo.toml")))
    (when (verus--is-cargo-verus-project-p cargo-toml)
      ;; Check if there's a workspace root different from this crate
      (when-let ((workspace-root (verus--find-workspace-root cargo-toml-root)))
        (not (string= (f-canonical workspace-root) (f-canonical cargo-toml-root)))))))

(defun verus--extract-extra-args-from (toml-file)
  "Extract the `package.metadata.verus.ide.extra_args' string from the TOML-FILE."
  (when (and
         toml-file
         (file-exists-p toml-file)
         (with-temp-buffer
           (insert-file-contents toml-file)
           (goto-char (point-min))
           (search-forward "verus" nil t)))
    (let* ((toml (tomlparse-file toml-file :object-type 'alist))
           (package (cdr (assoc 'package toml)))
           (metadata (cdr (assoc 'metadata package)))
           (verus (cdr (assoc 'verus metadata)))
           (ide (cdr (assoc 'ide verus)))
           (extra-args (cdr (assoc 'extra_args ide))))
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
  (when-let ((root (locate-dominating-file default-directory "Cargo.toml"))
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

(defun verus--get-package-name (toml-file)
  "Extract the package name from TOML-FILE (Cargo.toml).
Returns the package name as a string, or nil if not found."
  (when (and toml-file (file-exists-p toml-file))
    (let* ((toml (tomlparse-file toml-file :object-type 'alist))
           (package (cdr (assoc 'package toml)))
           (name (cdr (assoc 'name package))))
      name)))

(defun verus--cargo-verus-command (&optional package)
  "Build cargo-verus command.
If PACKAGE is non-nil, adds -p PACKAGE to target a specific workspace member.
Returns a list of command-line arguments for cargo verus verify.

When `verus-cargo-verus-arguments' is non-nil it is appended in place of the
default `--' separator.  The list must contain `--' at the appropriate
position; an error is signalled otherwise."
  (if verus-cargo-verus-arguments
      (progn
        (unless (member "--" verus-cargo-verus-arguments)
          (error "verus-cargo-verus-arguments must contain \"--\" to separate \
cargo-verus flags from Verus flags (e.g. (\"--features\" \"foo\" \"--\" \"--expand-errors\"))"))
        (append (list "cargo" "verus" "verify")
                (when package (list "-p" package))
                verus-cargo-verus-arguments))
    (append (list "cargo" "verus" "verify")
            (when package (list "-p" package))
            (list "--"))))

(defun verus--run-on-crate-command ()
  "Return the command to run Verus on the current crate.

Returns a list of command-line arguments. Expects to be run in a
buffer visiting the file, otherwise throws an error."
  (let ((file (buffer-file-name)))
    (when (not file)
      (error "Buffer is not visiting a file. Cannot run Verus"))
    (let* ((default-directory (f-dirname file))
           (crate-root (verus--crate-root-file))
           (cargo-toml-root (locate-dominating-file default-directory "Cargo.toml"))
           (cargo-toml (when cargo-toml-root (f-join cargo-toml-root "Cargo.toml")))
           (use-cargo-verus (and cargo-toml (verus--is-cargo-verus-project-p cargo-toml))))
      (if use-cargo-verus
          (let* ((default-directory cargo-toml-root)
                 (workspace-root (verus--find-workspace-root cargo-toml-root))
                 ;; If in a workspace, get the package name from the current crate's Cargo.toml
                 (package-name (when workspace-root
                                 (verus--get-package-name cargo-toml))))
            (verus--cargo-verus-command package-name))
        (append
         (list verus--rust-verify)
         (if (string-suffix-p "lib.rs" crate-root)
             (list "--crate-type=lib"))
         (if (string-suffix-p "vstd.rs" crate-root)
             (list "--crate-type=lib"
                   "--no-vstd"))
         (verus--extra-args-from-cargo-toml)
         (list crate-root))))))

(defun verus--current-module-name ()
  "Return the `::'-delimited name of current module.

Utilizes the current buffer's file name, but also accounts for
the path from the crate root file to the current buffer."
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
  (let ((file (buffer-file-name)))
    (when (not file)
      (error "Buffer is not visiting a file. Cannot run Verus"))
    ;; Use the file's directory as default-directory for finding crate root,
    ;; since the caller may have set default-directory to the workspace root
    (let ((default-directory (f-dirname file)))
      (append
       (verus--run-on-crate-command)
       (cond
        ;; In workspace: skip subsetting arguments due to issue verus#1938
        ((verus--is-in-cargo-verus-workspace) nil)
        ((string= file (verus--crate-root-file))
         (list "--verify-root"))
        (t
         (list "--verify-module" (verus--current-module-name))))))))

(defun verus-run-on-crate (prefix)
  "Run Verus on the current crate.

If PREFIX is non-nil, then run ask for the command to run."
  (interactive "p")
  (let* ((cargo-verus-root (verus--get-cargo-verus-root-directory))
         (default-directory (or cargo-verus-root default-directory))
         (verus-command (with-demoted-errors "Verus error: %S"
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
  (let* ((cargo-verus-root (verus--get-cargo-verus-root-directory))
         (default-directory (or cargo-verus-root default-directory))
         (verus-command (with-demoted-errors "Verus error: %S"
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

If PREFIX is non-nil, then enable `always profiling' mode."
  (interactive "p")
  (verus-run-on-file 1 (if (= prefix 1)
                           (list "--profile")
                         (list "--profile-all"))))

(defun verus--run-on-function-command (function-name)
  "Return the command to run Verus on a specific function.

Returns a list of command-line arguments. If FUNCTION-NAME is nil,
returns base command for manual function specification."
  (let ((base-command (verus--run-on-crate-command)))
    (if (verus--is-in-cargo-verus-workspace)
        (progn
          (message "Function/module subsetting is unsupported on workspace crates until https://github.com/verus-lang/verus/issues/1938 is resolved. Verifying entire workspace instead.")
          base-command)
      (let ((verify-args (cond
                          ((string= (buffer-file-name) (verus--crate-root-file))
                           (list "--verify-root"))
                          ((verus--has-modules-in-file)
                           (message "Warning: module detection for functions may not be fully accurate in a file that also has submodules.  If you get a failure in module detection, please file an issue at https://github.com/verus-lang/verus-mode.el/issues.")
                           (list "--verify-only-module" (verus--current-module-name)))
                          (t
                           (list "--verify-only-module" (verus--current-module-name)))))
            (function-args (if function-name
                               (list "--verify-function"
                                     (if verus-partial-verif-of-fn-is-starred
                                         (concat "*" function-name "*")
                                       function-name))
                             (list "--verify-function"))))
        (append base-command verify-args function-args)))))

(defun verus-run-on-function-at-point (prefix)
  "Run Verus on the function at point.

If PREFIX is non-nil, then confirm command to run before running it."
  (interactive "p")
  (let* ((cargo-verus-root (verus--get-cargo-verus-root-directory))
         (default-directory (or cargo-verus-root default-directory))
         (function-name
          (save-excursion
            (when (re-search-backward "\\_<fn\\_>\\s-+\\([a-zA-Z0-9_]+\\)[<(]" nil t)
              (match-string 1)))))
    (if function-name
        (let ((verus-command (with-demoted-errors "Verus error: %S"
                               (verus--run-on-function-command function-name))))
          (when verus-command
            (let ((compilation-command
                   (mapconcat #'shell-quote-argument verus-command " ")))
              (compile (if (= prefix 1)
                           compilation-command
                         (read-shell-command "Run Verus: " compilation-command))))))
      (message "Could not auto-detect function to verify. Try using C-u C-c C-c C-f.")
      (unless (= prefix 1)
        (let ((verus-command (with-demoted-errors "Verus error: %S"
                               (verus--run-on-function-command nil))))
          (when verus-command
            (let ((compilation-command
                   (mapconcat #'shell-quote-argument verus-command " ")))
              (compile (read-shell-command "Run Verus: " compilation-command)))))))))

;;; Flycheck setup

(flycheck-define-checker verus-cargo
  "A Verus syntax checker using cargo verus verify."
  :command ("cargo"
            "verus"
            "verify"
            (eval
             ;; Extract -p flag and package name if present
             (let* ((full-cmd (verus--run-on-file-command))
                    (dash-dash-pos (cl-position "--" full-cmd :test #'string=))
                    ;; Get everything between "verify" and "--"
                    (cargo-flags (when dash-dash-pos
                                   (cl-subseq full-cmd 3 dash-dash-pos))))
               cargo-flags))
            "--message-format=json"
            "--"
            (eval
             ;; Get verus arguments (everything after "--")
             (let* ((full-cmd (verus--run-on-file-command))
                    (dash-dash-pos (cl-position "--" full-cmd :test #'string=))
                    (args (when dash-dash-pos
                            (nthcdr (1+ dash-dash-pos) full-cmd))))
               (seq-filter (lambda (x) (not (string= x "--expand-errors"))) args)))
            "--expand-errors")
  :error-parser flycheck-parse-cargo-rustc
  :error-filter flycheck-rust-error-filter
  :working-directory (lambda (checker)
                       (or (verus--get-cargo-verus-root-directory)
                           default-directory))
  :predicate (lambda ()
               (and
                (flycheck-buffer-saved-p)
                (verus--is-verus-file)
                (verus--get-cargo-verus-root-directory)))
  :modes verus-mode)

(flycheck-define-checker verus
  "A Verus syntax checker using the Verus compiler."
  :command ("rust-verify.sh"
            (eval
             (let ((args (cdr (verus--run-on-file-command))))
               (seq-filter (lambda (x) (not (string= x "--expand-errors"))) args)))
            "--error-format=json"
            "--expand-errors")
  :error-parser flycheck-parse-cargo-rustc
  :error-filter flycheck-rust-error-filter
  :predicate (lambda ()
               (and
                (flycheck-buffer-saved-p)
                (verus--is-verus-file)
                (not (when-let ((root (locate-dominating-file default-directory "Cargo.toml")))
                       (verus--is-cargo-verus-project-p (f-join root "Cargo.toml"))))))
  :modes verus-mode)

(defun verus--flycheck-setup ()
  "Setup Flycheck for Verus."
  (add-to-list 'flycheck-checkers 'verus-cargo)
  (add-to-list 'flycheck-checkers 'verus)
  (setq flycheck-verus-executable verus--rust-verify))

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
  "The file storing the last time we checked for a new version of verus-mode.el.")

(defun verus-check-version-now ()
  "Check for a new version of verus-mode.el, even if already checked recently."
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
