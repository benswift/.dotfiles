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
        "cj" 'extempore-connect-or-disconnect

        "ee" 'extempore-send-last-sexp
        "ef" 'extempore-send-definition
        "er" 'extempore-send-region
        "eb" 'extempore-send-buffer-or-region
        (setq extempore-tab-completion nil)

        ;; device-specific Extempore config
        (cond
         ((string= (system-name) "Lonyx")
          (setq extempore-share-directory "/home/ben/Code/extempore/")
          (setq user-extempore-lib-directory "/home/ben/Code/xtm/lib/"))
         ((string= (system-name) "WINYX")
          (setq extempore-program-args nil)
          (setq extempore-share-directory "c:/Users/ben/Code/extempore/"))
         ((string= (system-name) "debian-vm")
          (setq extempore-program-args "--device 1 --frames 1024")
          (setq extempore-share-directory "/home/ben/Code/extempore/")
          (setq user-extempore-lib-directory "/home/ben/Code/xtm/lib/"))
         ((or (string= (system-name) "hodgey.local")
              (string= (system-name) "hodgey.lan")
              t) ;; probably running on hodgey
          (setq extempore-program-args nil)
          (setq extempore-share-directory "/Users/ben/Code/extempore/")
          (setq user-extempore-lib-directory "/Users/ben/Code/xtm/lib/")))

        (setq eldoc-documentation-function
              'extempore-eldoc-documentation-function)

        (set-face-attribute 'extempore-blink-face nil :foreground "#272822" :background "#FD971F")
        (set-face-attribute 'extempore-sb-blink-face nil :foreground "#272822" :background "#39FF14")

        ;; more extempore-related goodies

        (autoload #'llvm-mode (concat extempore-share-directory "extras/llvm-mode.el")
          "Major mode for editing LLVM IR files" t)

        ;; to pull down the lldb-aware gud.el
        ;; (async-shell-command (format "curl -o %sextras/gud-lldb.el http://www.opensource.apple.com/source/lldb/lldb-69/utils/emacs/gud.el?txt" extempore-share-directory))

        (autoload #'lldb (concat extempore-share-directory "extras/gud-lldb.el")
          "A version of gud.el which supports debugging in LLDB." t)

        (add-to-list 'auto-mode-alist '("\\.ir$" . llvm-mode))
        (add-to-list 'auto-mode-alist '("\\.ll$" . llvm-mode))

        ;; lldb-GUD integration

        (defun ben-lldb-mode-hook ()
          (if (string-match "^.*-extempore.*\\*$" (buffer-name))
              (setq extempore-buffer (buffer-name))))

        (add-hook 'lldb-mode-hook 'ben-lldb-mode-hook)

        (defun extempore-create-template-file (base-path filename &optional header)
          (let ((full-path (format "%s/%s" base-path filename)))
            (unless (file-exists-p full-path)
              (progn
                (find-file full-path)
                (if header (insert header))
                (save-buffer)
                (kill-buffer)))))

        (defun extempore-create-template (name)
          "Set up the directory structure and files for a new extempore session/gig."
          (interactive "sSession name: ")
          (let* ((xtm-dir (expand-file-name "~/Code/xtm/"))
                 (base-path (concat xtm-dir "sessions/" name))
                 (setup-header
                  (concat ";;; setup.xtm --- setup file for " name "\n"
                          "(sys:load \"" xtm-dir "lib/benlib-scm.xtm\")\n"
                          "(ipc:load \"utility\" \"" xtm-dir "lib/benlib-scm.xtm\")\n\n"
                          "dspmt")))
            (if (file-exists-p base-path)
                (error "Cannot create xtm session: directory already exists."))
            (make-directory base-path)
            ;; practice files
            (extempore-create-template-file
             base-path "prac-utility.xtm" "headeru")
            (extempore-create-template-file
             base-path "prac-primary.xtm" "headerp")
            ;; gig files
            (extempore-create-template-file
             base-path "gig-utility.xtm" "headeru")
            (extempore-create-template-file
             base-path "gig-primary.xtm" "headerp")
            ;; setup file
            (extempore-create-template-file
             base-path "setup.xtm" setup-header)
            (dired base-path)))

        (defun extempore-run-integration-test ()
          (interactive)
          (let ((default-directory (concat extempore-share-directory "extras/")))
            (async-shell-command "./integration-test.sh"
                                 (get-buffer-create "*extempore-integration-test*"))))

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
                        (let ((default-directory extempore-share-directory))
                          (async-shell-command (format "extempore --port=17199 --eval \"(impc:aot:compile-xtm-file \\\"%s\\\" #t #t)\"" x)))))
                    :require-match 'confirm-after-completion
                    :keymap counsel-find-file-map))

        (autoload 'extempore-debovinate-file "~/.dotfiles/extempore-debovinator.el" "debovinate all the things!" :interactive)

        ))))

;;; packages.el ends here
