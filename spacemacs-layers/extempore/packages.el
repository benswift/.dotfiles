;;; packages.el --- Extempore layer packages file for Spacemacs.
;;
;; Copyright (c) 2017-2020 Ben Swift
;;
;; Author: Ben Swift <ben@benswift.me>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

;;; Commentary:

;;; Code:

(defconst extempore-packages
  '(extempore-mode))

(defun extempore/init-extempore-mode ()
  "Initialize extempore mode"
  (use-package extempore-mode
    :mode
    (("\\.xtm$" . extempore-mode))
    :config
    (progn
      (spacemacs/declare-prefix-for-mode 'extempore-mode "mc" "process")
      (spacemacs/declare-prefix-for-mode 'extempore-mode "me" "eval")

      (spacemacs/set-leader-keys-for-major-mode 'extempore-mode
        "'"  'extempore-repl
        ","  'lisp-state-toggle-lisp-state

        "cc" 'switch-to-extempore
        "cj" 'extempore-connect

        "ee" 'extempore-send-last-sexp
        "ef" 'extempore-send-definition
        "er" 'extempore-send-region
        "eb" 'extempore-send-buffer-or-region
        (setq extempore-tab-completion nil)

        (setq eldoc-documentation-function
              'extempore-eldoc-documentation-function)

        (set-face-attribute 'extempore-blink-face nil :foreground "#272822" :background "#FD971F")
        (set-face-attribute 'extempore-sb-blink-face nil :foreground "#272822" :background "#39FF14")

		;; stop the ' (quote) character being paired by smartparens
		(with-eval-after-load 'smartparens
		  (sp-local-pair 'extempore-mode "'" nil :actions nil)
		  (sp-local-pair 'extempore-mode "`" nil :actions nil))

        ))))

;;; packages.el ends here
