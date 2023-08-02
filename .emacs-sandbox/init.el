;; Set up package management
(require 'package)
(setq package-archives '(("melpa" . "http://melpa.org/packages/")
                         ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(or (require 'use-package nil t)
    (progn
      (package-refresh-contents)
      (package-install 'use-package)
      (message "On a new system. Just installed use-package!")))

;; Install verus-mode.el's dependencies automatically
(use-package flycheck :ensure t)
(use-package rustic :ensure t)
(use-package dumb-jump :ensure t)

;; Let Emacs know where verus-mode.el even is.
(add-to-list 'load-path "PATH_TO_VERUS_MODE_DIR")

;; Let Emacs know where Verus itself is.
(setq verus-home "PATH_TO_VERUS_DIR")

;; Some sanity checks, that'll automatically detect if you have
;; forgotten to actually update important constants.
(if (string-match-p "\\.emacs-sandbox$" user-emacs-directory)
    ;; If `user-emacs-directory' ends with `.emacs-sandbox', then we
    ;; are part of the testing sandbox, so we don't need to complain,
    ;; and instead automatically do a fixup since we know exactly
    ;; where things should be.
    (progn
      (setq load-path (seq-filter (lambda (path) (not (string-prefix-p "PATH_TO_" path))) load-path))
      (add-to-list 'load-path (file-name-directory (directory-file-name user-emacs-directory)))
      ;; Set `verus-home' to the value read from the file `.verus-home' in the
      ;; `user-emacs-directory', stripping any surrounding whitespace.
      (setq verus-home (with-temp-buffer
                         (insert-file-contents (locate-user-emacs-file "verus-home"))
                         (s-trim (buffer-string)))))
  ;; Otherwise, we are not in the sandbox, and are running on a real
  ;; user's machine, and thus should make sure that the constants are
  ;; actually sane. If the constants haven't been updated, then
  ;; complain loudly.
  (if (seq-find (lambda (path) (string-prefix-p "PATH_TO_" path)) load-path)
      (error "Please replace the string 'PATH_TO_VERUS_MODE_DIR' in your .emacs with the path to the directory containing the verus-mode.el file"))
  (if (string-prefix-p verus-home "PATH_TO_")
      (error "Please replace the string 'PATH_TO_VERUS_DIR' in your .emacs with the path to Verus")))

;; Actually load verus-mode.el
(require 'verus-mode)

;; Autogenerated stuff, can be safely removed or kept or
;; ignored. Don't worry about this.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(use-package rustic flycheck dumb-jump)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
