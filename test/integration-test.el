;;; integration-test.el --- Integration tests for verus-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Jay Bosamiya

;;; Commentary:

;; Full integration tests that exercise verus-mode with real Verus files,
;; testing verification, flycheck, navigation, and error detection.

;;; Code:

(require 'ert)
(require 'verus-mode)
(require 'flycheck)

;;; Test Configuration

(defvar verus-test-timeout 30
  "Timeout in seconds for verification commands.")

(defvar verus-test-examples-dir
  (expand-file-name "verus-examples" default-directory)
  "Directory containing Verus example files.")

;;; Enable ansi-color in compilation buffer for better readability

(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;;; Suppress unwanted messages during batch tests

(when noninteractive
  ;; Suppress "Compilation finished" and similar messages
  (advice-add 'compilation-handle-exit :around
              (lambda (orig-fun process-status exit-status msg)
                (let ((inhibit-message t))
                  (funcall orig-fun process-status exit-status msg))))
  ;; Suppress flycheck-next-error messages
  (advice-add 'flycheck-next-error :around
              (lambda (orig-fun &rest args)
                (let ((inhibit-message t))
                  (apply orig-fun args))))
  ;; Suppress next-error messages
  (advice-add 'next-error :around
              (lambda (orig-fun &rest args)
                (let ((inhibit-message t))
                  (apply orig-fun args))))
  ;; For read-file-name, throw an error, it should not be invoked in batch mode.
  (advice-add 'read-file-name :around
              (lambda (orig-fun &rest args)
                (error (concat "read-file-name should not be called in batch mode. "
                               "it means that the default-directory is not set correctly. "
                               "thus verus-mode could not find the file path")))))

;;; Test Helpers

(defun verus-test-wait-for-compilation (&optional timeout)
  "Wait for compilation to finish, with optional TIMEOUT in seconds."
  (let ((timeout (or timeout verus-test-timeout))
        (start (current-time)))
    (while (and (get-buffer "*compilation*")
                (with-current-buffer "*compilation*"
                  (get-buffer-process (current-buffer)))
                (< (float-time (time-subtract (current-time) start)) timeout))
      (accept-process-output nil 0.1))
    (when (and (get-buffer "*compilation*")
               (with-current-buffer "*compilation*"
                 (get-buffer-process (current-buffer))))
      (error "Compilation timeout after %d seconds" timeout))))

(defun verus-test-wait-for-flycheck (&optional timeout)
  "Wait for flycheck to finish checking, with optional TIMEOUT in seconds."
  (let ((timeout (or timeout verus-test-timeout))
        (start (current-time)))
    (while (and (flycheck-running-p)
                (< (float-time (time-subtract (current-time) start)) timeout))
      (accept-process-output nil 0.1))
    (when (flycheck-running-p)
      (error "Flycheck timeout after %d seconds" timeout))))

(defun verus-test-get-compilation-result ()
  "Get the result of the last compilation (success or failure).
Uses the compilation exit status which works for both single crates and workspaces."
  (when (get-buffer "*compilation*")
    (with-current-buffer "*compilation*"
      (goto-char (point-max))
      ;; Look for compilation exit status at the end of the buffer
      (cond
       ((re-search-backward "Compilation finished" nil t)
        'success)
       ((re-search-backward "Compilation exited abnormally with code" nil t)
        'failure)
       (t 'unknown)))))

(defun verus-test-compilation-has-error-at (file line)
  "Check if compilation buffer has an error at FILE:LINE."
  (when (get-buffer "*compilation*")
    (with-current-buffer "*compilation*"
      (goto-char (point-min))
      (let ((pattern (format "%s:%d:" (file-name-nondirectory file) line)))
        (search-forward pattern nil t)))))

(defmacro with-verus-file (file &rest body)
  "Visit FILE in verus-mode and execute BODY, then clean up."
  (declare (indent 1))
  `(let ((buf (find-file-noselect ,file)))
     (unwind-protect
         (with-current-buffer buf
           (revert-buffer t t t)  ; Force fresh reload from disk
           (unless (eq major-mode 'verus-mode)
             (verus-mode))
           ,@body)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (set-buffer-modified-p nil))
         (kill-buffer buf)))))

;;; Basic File Loading Tests

(ert-deftest verus-integration-test-load-syntax-example ()
  "Test loading and verifying syntax.rs example."
  :tags '(integration slow)
  (skip-unless (and (getenv "VERUS_HOME")
                    (file-exists-p (expand-file-name "syntax.rs" verus-test-examples-dir))))
  (let ((file (expand-file-name "syntax.rs" verus-test-examples-dir)))
    (with-verus-file file
                     (should (eq major-mode 'verus-mode))
                     (should (verus--is-verus-file)))))

(ert-deftest verus-integration-test-load-crate-example ()
  "Test loading crate example files."
  :tags '(integration)
  (skip-unless (file-exists-p (expand-file-name "crate/src/lib.rs" verus-test-examples-dir)))
  (let ((file (expand-file-name "crate/src/lib.rs" verus-test-examples-dir)))
    (with-verus-file file
                     (should (eq major-mode 'verus-mode))
                     (should (verus--is-verus-file)))))

(ert-deftest verus-integration-test-load-cv-crate-example ()
  "Test loading cargo-verus crate example."
  :tags '(integration)
  (skip-unless (file-exists-p (expand-file-name "cv-crate/src/lib.rs" verus-test-examples-dir)))
  (let ((file (expand-file-name "cv-crate/src/lib.rs" verus-test-examples-dir))
        (cargo-toml (expand-file-name "cv-crate/Cargo.toml" verus-test-examples-dir)))
    (with-verus-file file
                     (should (eq major-mode 'verus-mode))
                     (should (verus--is-verus-file))
                     (should (verus--is-cargo-verus-project-p cargo-toml)))))

;;; Verification Tests - C-c C-c C-c (verus-run-on-file)

(ert-deftest verus-integration-test-verify-file-success ()
  "Test C-c C-c C-c on a file that should verify successfully."
  :tags '(integration slow verification)
  (skip-unless (and (getenv "VERUS_HOME")
                    (file-exists-p (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir))))
  (let ((file (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir)))
    (with-verus-file file
                     ;; Ensure file has correct content (in case previous test left it in bad state)
                     (goto-char (point-min))
                     (when (search-forward "assert(1 == 2)" nil t)
                       (replace-match "assert(1 == 1)")
                       (save-buffer))

                     ;; Wait for any existing compilation to finish, then kill buffer
                     (when (get-buffer "*compilation*")
                       (verus-test-wait-for-compilation)
                       (kill-buffer "*compilation*"))

                     ;; Run verification
                     (verus-run-on-file 1)
                     (verus-test-wait-for-compilation)

                     ;; Check result
                     (should (eq (verus-test-get-compilation-result) 'success))
                     (should (get-buffer "*compilation*")))))

;;; Verification Tests - C-c C-c C-f (verus-run-on-function-at-point)

(ert-deftest verus-integration-test-verify-function-success ()
  "Test C-c C-c C-f on a function that should verify successfully."
  :tags '(integration slow verification function)
  (skip-unless (and (getenv "VERUS_HOME")
                    (file-exists-p (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir))))
  (let ((file (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir)))
    (with-verus-file file
                     ;; Ensure file has correct content
                     (goto-char (point-min))
                     (when (search-forward "assert(1 == 2)" nil t)
                       (replace-match "assert(1 == 1)")
                       (save-buffer))

                     ;; Wait for any existing compilation to finish, then kill buffer
                     (when (get-buffer "*compilation*")
                       (verus-test-wait-for-compilation)
                       (kill-buffer "*compilation*"))

                     ;; Navigate to a function
                     (goto-char (point-min))
                     (search-forward "fn test()")

                     ;; Run verification on function
                     (verus-run-on-function-at-point 1)
                     (verus-test-wait-for-compilation)

                     ;; Check result
                     (should (eq (verus-test-get-compilation-result) 'success))
                     (should (get-buffer "*compilation*")))))

(ert-deftest verus-integration-test-verify-function-with-error ()
  "Test C-c C-c C-f on a function with an error."
  :tags '(integration slow verification function)
  (skip-unless (and (getenv "VERUS_HOME")
                    (file-exists-p (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir))))
  (let ((file (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir)))
    (with-verus-file file
                     ;; Save original content
                     (let ((original-content (buffer-string)))
                       ;; Introduce an error
                       (goto-char (point-min))
                       (search-forward "assert(1 == 1)")
                       (replace-match "assert(1 == 2)")
                       (save-buffer)

                       ;; Wait for any existing compilation to finish, then kill buffer
                       (when (get-buffer "*compilation*")
                         (verus-test-wait-for-compilation)
                         (kill-buffer "*compilation*"))

                       (unwind-protect
                           (progn
                             ;; Navigate to the function
                             (goto-char (point-min))
                             (search-forward "fn test()")

                             ;; Run verification on function
                             (verus-run-on-function-at-point 1)
                             (verus-test-wait-for-compilation)

                             ;; Check that verification failed
                             (should (eq (verus-test-get-compilation-result) 'failure))

                             ;; Check that error is reported in compilation buffer
                             (should (get-buffer "*compilation*"))
                             (with-current-buffer "*compilation*"
                               (goto-char (point-min))
                               (should (search-forward "error" nil t))))

                         ;; Restore file
                         (erase-buffer)
                         (insert original-content)
                         (save-buffer))))))

(ert-deftest verus-integration-test-verify-function-correct-directory ()
  "Test that C-c C-c C-f uses the correct default-directory (cargo root, not src/)."
  :tags '(integration slow verification function)
  (skip-unless (and (getenv "VERUS_HOME")
                    (file-exists-p (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir))))
  (let ((file (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir))
        (expected-root (expand-file-name "cv-crate" verus-test-examples-dir))
        (captured-directory nil))
    (with-verus-file file
                     ;; Ensure file has correct content
                     (goto-char (point-min))
                     (when (search-forward "assert(1 == 2)" nil t)
                       (replace-match "assert(1 == 1)")
                       (save-buffer))

                     ;; Wait for any existing compilation to finish, then kill buffer
                     (when (get-buffer "*compilation*")
                       (verus-test-wait-for-compilation)
                       (kill-buffer "*compilation*"))

                     ;; Navigate to a function
                     (goto-char (point-min))
                     (search-forward "fn test()")

                     ;; Define advice function to capture default-directory
                     (cl-flet ((capture-directory (&rest _)
                                 (setq captured-directory default-directory)))
                       (unwind-protect
                           (progn
                             (advice-add 'compile :before #'capture-directory)

                             ;; Run verification on function
                             (verus-run-on-function-at-point 1)
                             (verus-test-wait-for-compilation)

                             ;; Check that the directory used was the cargo root, not src/
                             ;; Normalize paths for comparison (expand and remove trailing slash)
                             (let ((normalized-captured (file-name-as-directory
                                                         (expand-file-name captured-directory)))
                                   (normalized-expected (file-name-as-directory expected-root)))
                               (should (string= normalized-captured normalized-expected)))

                             ;; Also verify it's not the src/ directory
                             (let ((normalized-captured (file-name-as-directory
                                                         (expand-file-name captured-directory)))
                                   (src-dir (file-name-as-directory
                                             (expand-file-name "cv-crate/src" verus-test-examples-dir)))
                                   (foo-dir (file-name-as-directory
                                             (expand-file-name "cv-crate/src/foo" verus-test-examples-dir))))
                               (should-not (string= normalized-captured src-dir))
                               (should-not (string= normalized-captured foo-dir)))

                             ;; Check that compilation succeeded
                             (should (eq (verus-test-get-compilation-result) 'success)))

                         ;; Remove advice
                         (advice-remove 'compile #'capture-directory))))))

(ert-deftest verus-integration-test-verify-file-with-error ()
  "Test C-c C-c C-c on a file with an intentional error."
  :tags '(integration slow verification)
  (skip-unless (and (getenv "VERUS_HOME")
                    (file-exists-p (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir))))
  (let ((file (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir)))
    (with-verus-file file
                     ;; Save original content
                     (let ((original-content (buffer-string)))
                       ;; Introduce an error
                       (goto-char (point-min))
                       (search-forward "assert(1 == 1)")
                       (replace-match "assert(1 == 2)")
                       (save-buffer)

                       ;; Wait for any existing compilation to finish, then kill buffer
                       (when (get-buffer "*compilation*")
                         (verus-test-wait-for-compilation)
                         (kill-buffer "*compilation*"))

                       (unwind-protect
                           (progn
                             ;; Run verification
                             (verus-run-on-file 1)
                             (verus-test-wait-for-compilation)

                             ;; Check that verification failed
                             (should (eq (verus-test-get-compilation-result) 'failure))

                             ;; Check that error is reported in compilation buffer
                             (should (get-buffer "*compilation*"))
                             (with-current-buffer "*compilation*"
                               (goto-char (point-min))
                               (should (search-forward "error" nil t))))

                         ;; Restore file
                         (erase-buffer)
                         (insert original-content)
                         (save-buffer))))))

;;; Flycheck Integration Tests

(ert-deftest verus-integration-test-flycheck-success ()
  "Test that flycheck reports no errors on valid file."
  :tags '(integration slow flycheck)
  (skip-unless (and (getenv "VERUS_HOME")
                    (file-exists-p (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir))))
  (let ((file (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir)))
    (with-verus-file file
                     ;; Ensure flycheck is active
                     (flycheck-mode 1)

                     ;; Trigger flycheck
                     (flycheck-buffer)
                     (verus-test-wait-for-flycheck)

                     ;; Check for no errors
                     (should (memq flycheck-last-status-change '(finished no-checker))))))

(ert-deftest verus-integration-test-flycheck-error-detection ()
  "Test that flycheck detects verification errors."
  :tags '(integration slow flycheck)
  (skip-unless (and (getenv "VERUS_HOME")
                    (file-exists-p (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir))))
  (let ((file (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir)))
    (with-verus-file file
                     ;; Save original content
                     (let ((original-content (buffer-string)))
                       ;; Introduce an error
                       (goto-char (point-min))
                       (search-forward "assert(1 == 1)")
                       (replace-match "assert(1 == 2)")
                       (save-buffer)

                       ;; Ensure flycheck is active
                       (flycheck-mode 1)

                       (unwind-protect
                           (progn
                             ;; Trigger flycheck
                             (flycheck-buffer)
                             (verus-test-wait-for-flycheck)

                             ;; Check that errors were found - flycheck may report 'finished' even with errors
                             (should (memq flycheck-last-status-change '(finished errored)))
                             (should (> (length (flycheck-overlay-errors-in (point-min) (point-max))) 0)))

                         ;; Restore file
                         (erase-buffer)
                         (insert original-content)
                         (save-buffer))))))

;;; Navigation Tests - M-g n / next-error

(ert-deftest verus-integration-test-next-error-navigation ()
  "Test that M-g n (next-error) navigates to verification errors."
  :tags '(integration slow navigation)
  (skip-unless (and (getenv "VERUS_HOME")
                    (file-exists-p (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir))))
  (let ((file (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir)))
    (with-verus-file file
                     ;; Save original content
                     (let ((original-content (buffer-string)))
                       ;; Introduce an error at a known line
                       (goto-char (point-min))
                       (search-forward "assert(1 == 1)")
                       (let ((error-line (line-number-at-pos)))
                         (replace-match "assert(1 == 2)")
                         (save-buffer)

                         ;; Wait for any existing compilation to finish, then kill buffer
                         (when (get-buffer "*compilation*")
                           (verus-test-wait-for-compilation)
                           (kill-buffer "*compilation*"))

                         (unwind-protect
                             (progn
                               ;; Run verification
                               (verus-run-on-file 1)
                               (verus-test-wait-for-compilation)

                               ;; Try to navigate to error
                               (next-error)

                               ;; Should be at the error location
                               (should (= (line-number-at-pos) error-line)))

                           ;; Restore file
                           (erase-buffer)
                           (insert original-content)
                           (save-buffer)))))))

;;; C-c C-n / C-c C-p (flycheck navigation) Tests

(ert-deftest verus-integration-test-flycheck-next-error ()
  "Test that C-c C-n navigates to next flycheck error."
  :tags '(integration slow flycheck navigation)
  (skip-unless (and (getenv "VERUS_HOME")
                    (file-exists-p (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir))))
  (let ((file (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir)))
    (with-verus-file file
                     ;; Save original content
                     (let ((original-content (buffer-string)))
                       ;; Introduce an error
                       (goto-char (point-min))
                       (search-forward "assert(1 == 1)")
                       (let ((error-line (line-number-at-pos)))
                         (replace-match "assert(1 == 2)")
                         (save-buffer)

                         ;; Ensure flycheck is active
                         (flycheck-mode 1)

                         (unwind-protect
                             (progn
                               ;; Trigger flycheck
                               (flycheck-buffer)
                               (verus-test-wait-for-flycheck)

                               ;; Go to beginning of buffer
                               (goto-char (point-min))

                               ;; Navigate to error using C-c C-n
                               (flycheck-next-error)

                               ;; Should be near the error
                               (should (<= (abs (- (line-number-at-pos) error-line)) 1)))

                           ;; Restore file
                           (erase-buffer)
                           (insert original-content)
                           (save-buffer)))))))

;;; Crate Root Detection Tests

(ert-deftest verus-integration-test-crate-root-detection ()
  "Test that crate root file is correctly detected."
  :tags '(integration)
  (skip-unless (file-exists-p (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir)))
  (let ((file (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir))
        (expected-root (expand-file-name "cv-crate/src/lib.rs" verus-test-examples-dir)))
    (with-verus-file file
                     (let ((root (verus--crate-root-file)))
                       (should (string= root expected-root))))))

;;; Module Name Detection Tests

(ert-deftest verus-integration-test-module-name-detection ()
  "Test that module names are correctly detected."
  :tags '(integration)
  (skip-unless (file-exists-p (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir)))
  (let ((file (expand-file-name "cv-crate/src/foo/bar.rs" verus-test-examples-dir)))
    (with-verus-file file
                     (let ((module-name (verus--current-module-name)))
                       (should (string= module-name "foo::bar"))))))

;;; Workspace Tests

(ert-deftest verus-integration-test-workspace-verify-file-success ()
  "Test C-c C-c C-c on a file in a workspace that should verify successfully."
  :tags '(integration slow verification workspace)
  (skip-unless (and (getenv "VERUS_HOME")
                    (file-exists-p (expand-file-name "cv-workspace/member1/src/submodule.rs" verus-test-examples-dir))))
  (let ((file (expand-file-name "cv-workspace/member1/src/submodule.rs" verus-test-examples-dir)))
    (with-verus-file file
                     ;; Ensure file has correct content
                     (goto-char (point-min))
                     (when (search-forward "assert(2 == 3)" nil t)
                       (replace-match "assert(2 == 1 + 1)")
                       (save-buffer))

                     ;; Wait for any existing compilation to finish, then kill buffer
                     (when (get-buffer "*compilation*")
                       (verus-test-wait-for-compilation)
                       (kill-buffer "*compilation*"))

                     ;; Run verification
                     (verus-run-on-file 1)
                     (verus-test-wait-for-compilation)

                     ;; Check result
                     (should (eq (verus-test-get-compilation-result) 'success))
                     (should (get-buffer "*compilation*")))))

(ert-deftest verus-integration-test-workspace-verify-with-error ()
  "Test C-c C-c C-c on a workspace file with an error and verify navigation works."
  :tags '(integration slow verification workspace navigation)
  (skip-unless (and (getenv "VERUS_HOME")
                    (file-exists-p (expand-file-name "cv-workspace/member1/src/submodule.rs" verus-test-examples-dir))))
  (let ((file (expand-file-name "cv-workspace/member1/src/submodule.rs" verus-test-examples-dir)))
    (with-verus-file file
                     ;; Save original content
                     (let ((original-content (buffer-string)))
                       ;; Introduce an error at a known line
                       (goto-char (point-min))
                       (search-forward "assert(2 == 1 + 1)")
                       (let ((error-line (line-number-at-pos)))
                         (replace-match "assert(2 == 3)")
                         (save-buffer)

                         ;; Wait for any existing compilation to finish, then kill buffer
                         (when (get-buffer "*compilation*")
                           (verus-test-wait-for-compilation)
                           (kill-buffer "*compilation*"))

                         (unwind-protect
                             (progn
                               ;; Run verification
                               (verus-run-on-file 1)
                               (verus-test-wait-for-compilation)

                               ;; Check that verification failed
                               (should (eq (verus-test-get-compilation-result) 'failure))

                               ;; Try to navigate to error
                               (next-error)

                               ;; Should be at the error location
                               (should (= (line-number-at-pos) error-line)))

                           ;; Restore file
                           (erase-buffer)
                           (insert original-content)
                           (save-buffer)))))))

(ert-deftest verus-integration-test-workspace-flycheck-error ()
  "Test that flycheck works correctly in workspace files and C-c C-n navigates to errors."
  :tags '(integration slow flycheck workspace navigation)
  (skip-unless (and (getenv "VERUS_HOME")
                    (file-exists-p (expand-file-name "cv-workspace/member1/src/submodule.rs" verus-test-examples-dir))))
  (let ((file (expand-file-name "cv-workspace/member1/src/submodule.rs" verus-test-examples-dir)))
    (with-verus-file file
                     ;; Save original content
                     (let ((original-content (buffer-string)))
                       ;; Introduce an error
                       (goto-char (point-min))
                       (search-forward "assert(2 == 1 + 1)")
                       (let ((error-line (line-number-at-pos)))
                         (replace-match "assert(2 == 3)")
                         (save-buffer)

                         ;; Ensure flycheck is active
                         (flycheck-mode 1)

                         (unwind-protect
                             (progn
                               ;; Trigger flycheck
                               (flycheck-buffer)
                               (verus-test-wait-for-flycheck)

                               ;; Check that errors were found
                               (should (memq flycheck-last-status-change '(finished errored)))
                               (should (> (length (flycheck-overlay-errors-in (point-min) (point-max))) 0))

                               ;; Go to beginning of buffer
                               (goto-char (point-min))

                               ;; Navigate to error using C-c C-n
                               (flycheck-next-error)

                               ;; Should be near the error
                               (should (<= (abs (- (line-number-at-pos) error-line)) 1)))

                           ;; Restore file
                           (erase-buffer)
                           (insert original-content)
                           (save-buffer)))))))

;;; verus-cargo-verus-arguments Tests

(ert-deftest verus-integration-test-cargo-verus-arguments ()
  "Test that verus-cargo-verus-arguments are passed through to the compile command."
  :tags '(integration verification)
  (skip-unless (file-exists-p (expand-file-name "cv-crate/src/lib.rs" verus-test-examples-dir)))
  (let ((file (expand-file-name "cv-crate/src/lib.rs" verus-test-examples-dir))
        (captured-command nil))
    (with-verus-file file
      (let ((verus-cargo-verus-arguments '("--" "--expand-errors")))
        (cl-flet ((capture-command (command &rest _)
                    (setq captured-command command)))
          (unwind-protect
              (progn
                (advice-add 'compile :before #'capture-command)
                (verus-run-on-file 1)
                (verus-test-wait-for-compilation))
            (advice-remove 'compile #'capture-command))))
      (should (stringp captured-command))
      (should (string-match-p "^cargo verus verify -- --expand-errors" captured-command)))))

;;; Run All Integration Tests

(defun verus-run-integration-tests ()
  "Run all integration tests for verus-mode."
  (interactive)
  (ert-run-tests-interactively "^verus-integration-test"))

(defun verus-run-integration-tests-batch ()
  "Run all integration tests in batch mode."
  (ert-run-tests-batch-and-exit "^verus-integration-test"))

(provide 'integration-test)
;;; integration-test.el ends here
