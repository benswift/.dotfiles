;;; packages.el --- extempore layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2017 Sylvain Benner & Contributors
;;
;; Author: Ben Swift <ben@benswift.me>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `extempore-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `extempore/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `extempore/pre-init-PACKAGE' and/or
;;   `extempore/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst extempore-packages
  '(extempore-mode
    company
    yasnippet))

(defun extempore/init-extempore-mode ()
  "Initialize extempore mode"
  (use-package extempore-mode
    :mode
    (("\\.xtm$" . extempore-mode))
    ;; :init
    ;; (progn
    ;;   (spacemacs/register-repl 'extempore 'extempore-repl "extempore REPL"))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'extempore-mode "ms" "repl")

      (spacemacs/set-leader-keys-for-major-mode 'extempore-mode
        "'"  'extempore-repl
        ","  'lisp-state-toggle-lisp-state

        "cc" 'extempore-send-buffer-or-region
        "cj" 'extempore-connect-or-disconnect

        "ee" 'extempore-send-last-sexp
        "ef" 'extempore-send-definition
        "el" 'lisp-state-eval-sexp-end-of-line
        "er" 'extempore-send-region

        "mf" 'extempore-expand-definition
        "mx" 'extempore-expand-region))))

;;; packages.el ends here
