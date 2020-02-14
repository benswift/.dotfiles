;;; packages.el --- Extempore layer packages file for Spacemacs.
;;
;; Copyright (c) 2017-2018 Ben Swift
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

        ;; used in extempore-mode's print-line-debug snippet
        (defun extempore-yas-println-debug-expander (pl-str format-str)
          (if (not (string= pl-str ""))
              (mapconcat (lambda (name) (format format-str name name))
                         (cl-remove-if (lambda (x) (or (string-match "^'.*:$" x)
                                                       (string-match "^\".*:\"$" x)))
                                       (split-string pl-str " "))
                         " ")
            pl-str))

        (defvar extempore-yas-oscillator-list '("osc" "square" "triangle" "rect" "saw" "pulse" "fade" "delay" "delay_t" "comb" "flanger" "chorus" "tap_delay" "allpass" "reverb" "reverb2" "hold" "svf" "lpf" "lpf2" "bpf" "hpf" "notch" "peak" "lshelf" "hshelf" "skf" "lpfbq" "hpfbq" "bpfbq" "notchbq" "vcf" "hann" "hann_t" "linear"))

        (defun extempore-yas-get-sample-map-list ()
          (if (boundp 'user-extempore-lib-directory)
              (with-temp-buffer (insert-file-contents (concat user-extempore-lib-directory "sampler-maps.xtm"))
                                (goto-char (point-min))
                                (cl-labels ((sm-parse-fn (sm-list)
                                                         (if (re-search-forward "(define \\(*sm-[^ \n]*\\)" nil :no-error)
                                                             (funcall #'sm-parse-fn (cons (match-string-no-properties 1) sm-list))
                                                           sm-list)))
                                  (sm-parse-fn nil)))
            '("")))

        (defun extempore-yas-get-chord-sym (maj-min)
          ;; symbol lists from libs/core/pc_ivl.xtm
          (mapcar #'symbol-name
                  (case maj-min
                    ('^ 5 '(i i6 i64 i7 i- i-7 n n6 ii ii6 ii7 ii9 ii^ ii^7 iii iii6 iii7 iii^ iii^7 iv iv6 iv7 iv- iv-7 v v6 v7 v- v-7 vi vi6 vi7 vi^ vi^7 viio viio7 vii vii7))
                    ('- '(i i6 i64 i7 i^ i^6 i^64 i^7 n n6 ii ii6 ii7 ii- ii-6 ii-7 ii^ ii^7 iii iii6 iii7 iii- iii-6 iii-7 iv iv6 iv7 iv^ iv^6 iv^7 v v^ v6 v7 v- v-6 v-6 v-7 vi vi6 vi7 vi- vi-6 vi-7 vii vii6 vii7 viio viio6 viio7))
                    (t nil))))

        ;; help displaying LLVM IR
        (defun extempore-show-ir-in-temp-buffer (beg end)
          (interactive "r")
          (save-excursion
            (let ((ir-str (buffer-substring-no-properties beg end)))
              (with-current-buffer (get-buffer-create "*extempore LLVM IR*")
                (if (not (equal major-mode 'llvm-mode))
                    (llvm-mode))
                (delete-region (point-min) (point-max))
                (insert (replace-regexp-in-string "\\\\n" "\n" ir-str))
                (display-buffer "*extempore LLVM IR*" #'display-buffer-pop-up-window)))))

        ;; AOT-compilation help
        (defun extempore-AOT-compile-lib (&optional initial-input)
          (interactive)
          (ivy-read "Library: " 'read-file-name-internal
                    :matcher #'counsel--find-file-matcher
                    :initial-input initial-input
                    :action
                    (lambda (x)
                      (with-ivy-window
                        (let ((default-directory extempore-path))
                          (async-shell-command (format "extempore --port=17199 --eval \"(impc:aot:compile-xtm-file \\\"%s\\\" #t #t)\"" x)))))
                    :require-match 'confirm-after-completion
                    :keymap counsel-find-file-map))
        ))))

;;; packages.el ends here
