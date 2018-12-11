;;; ben-utils.el --- Ben's utility functions -*- lexical-binding: t -*-

;; Author: Ben Swift
;; Maintainer: Ben Swift
;; Version: 1.0
;; Package-Requires: ()
;; Homepage: https://github.com/benswift/.dotfiles

;; Copyright 2018 Ben Swift

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; Ben's helper functions. Probably not useful for anyone else, but if you wanna
;; pinch stuff then knock yourself out.

;; commentary

;;; Code:

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

;; add human-readable filesize switch for dired
(setq dired-listing-switches "-alh")

;; time
(setq display-time-format "%H:%M")
(display-time-mode 1)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; indentation
(setq standard-indent 2)

;; warnings to suppress
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

;; keybindings
(spacemacs/declare-prefix "o" "user-prefix")
(spacemacs/set-leader-keys "op" 'comp1720-print-student)
(spacemacs/set-leader-keys "of" 'fais-visit-student)
(spacemacs/set-leader-keys "os" 'comp1720-visit-major-project)
(spacemacs/set-leader-keys "ob" 'comp1720-visit-major-project-feedback-file)
(spacemacs/set-leader-keys "ov" 'comp1720-start-major-project-server)
(spacemacs/set-leader-keys "og" 'comp1720-visit-gitlab)
(spacemacs/set-leader-keys "om" 'jekyll-move-download-and-mogrify)
(spacemacs/set-leader-keys "oc" '(lambda () (interactive) (switch-to-buffer "*compilation*")))

;;;;;;;;;;;;;;;;;;;
;; Jekyll config ;;
;;;;;;;;;;;;;;;;;;;

;; and some other teaching stuff

(defconst anu-cs-jekyll-website-directory  (expand-file-name "~/Documents/teaching/comp1720-2018/website"))
(defconst anu-cs-lucy-directory  (expand-file-name "~/Documents/teaching/comp1720-2018/lucy"))
(load-file (expand-file-name "~/Documents/teaching/comp1720-2018/admin/anu-cs-utils.el"))

(require 'url-util) ; needed for url-unerserved-chars

(defun jekyll-sanitise-post-name (post-name)
  (apply #'string (reverse (cl-reduce (lambda (processed char)
                                        (if (member char url-unreserved-chars)
                                            (cons char processed)
                                          (if (and processed
                                                   (= (first processed) ?-))
                                              processed
                                            (cons ?- processed))))
                                      (string-to-list post-name)
                                      :initial-value '()))))

(defun jekyll-new-post (post-name)
  (interactive "sPost title: ")
  (let ((post-url-basename
         (format "%s-%s.md"
                 (format-time-string "%Y-%m-%d")
                 (downcase (jekyll-sanitise-post-name post-name)))))
    (find-file (f-join (projectile-project-root) "_posts" post-url-basename))
    (insert (format
             "---
title: %s
date: \"%s\"
tags:
---
"
             post-name
             (format-time-string "%F %T %z")))))


;;;;;;;;;;
;; mu4e ;;
;;;;;;;;;;

(require 'mu4e-contrib) ;; for mu4e-shr2text
(require 'smtpmail)

(setq user-full-name "Ben Swift")
(setq mu4e-user-mail-address-list
      '("ben@benswift.me"
        "benswift@fastmail.com"
        "ben.swift@anu.edu.au"
        "benjamin.j.swift@gmail.com"
        "ben.swift@simeonnetwork.org"))

;; headers
(setq mu4e-headers-include-related nil)

;; receive

(setq mu4e-maildir (expand-file-name "~/Maildir")
      mu4e-sent-folder "/Sent Items"
      mu4e-refile-folder "/Archive"
      mu4e-drafts-folder "/Drafts"
      mu4e-trash-folder "/Trash"
      mu4e-attachment-dir (expand-file-name "~/Downloads"))

(setq mu4e-get-mail-command "mbsync fastmail"
      mu4e-update-interval 300
      mu4e-headers-auto-update t
      mu4e-change-filenames-when-moving t
      mu4e-view-show-addresses t)

(defun mu4e-pretty-mbsync-process-filter (proc msg)
  (ignore-errors
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))
        (insert (car (reverse (split-string msg "\r"))))
        (when (re-search-backward "\\(C:\\).*\\(B:\\).*\\(M:\\).*\\(S:\\)")
          (add-face-text-property
           (match-beginning 1) (match-end 1) 'font-lock-keyword-face)
          (add-face-text-property
           (match-beginning 2) (match-end 2) 'font-lock-function-name-face)
          (add-face-text-property
           (match-beginning 3) (match-end 3) 'font-lock-variable-name-face)
          (add-face-text-property
           (match-beginning 4) (match-end 4) 'font-lock-type-face))))))

(advice-add
 'mu4e~get-mail-process-filter
 :override #'mu4e-pretty-mbsync-process-filter)

;; compose

(setq mu4e-compose-dont-reply-to-self t)
(setq mu4e-compose-signature-auto-include nil)
(add-hook 'mu4e-compose-mode-hook #'spacemacs/toggle-yasnippet-on)

(defun ben-find-to-firstname ()
  "search the current buffer for a To: field, and grab the first recipient's name from there"
  (interactive)
  (let ((str (buffer-substring-no-properties (point-min) (point-max))))
    (if (string-match
         "^To: \"?\\([^ ,<\n]+\\)"
         str)
        (match-string 1 str)
      nil)))

(require 'gnus-dired)

;; make the `gnus-dired-mail-buffers' function also work on
;; message-mode derived modes, such as mu4e-compose-mode
(defun gnus-dired-mail-buffers ()
  "Return a list of active message buffers."
  (let (buffers)
    (save-current-buffer
      (dolist (buffer (buffer-list t))
        (set-buffer buffer)
        (when (and (derived-mode-p 'message-mode)
                   (null message-sent-message-via))
          (push (buffer-name buffer) buffers))))
    (nreverse buffers)))

(setq gnus-dired-mail-mode 'mu4e-user-agent)
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(setq mu4e-maildir-shortcuts
      '(("/INBOX" . ?i)
        ("/Sent Items" . ?s)
        ("/Archive" . ?a)
        ("/Drafts" . ?d)
        ("/Trash" . ?t)
        ("/Junk Mail" . ?j)))

(setq mu4e-headers-date-format "%e %b %y"
      mu4e-headers-fields '((:human-date . 12)
                            (:flags . 6)
                            (:maildir . 10)
                            (:from . 22)
                            (:subject)))

(setq mu4e-view-show-images t
      mu4e-html2text-command #'mu4e-shr2text
      ;; make sure fg-bg contrast is high enough
      shr-color-visible-luminance-min 80
      ;; don't use variable pitch fonts
      shr-use-fonts nil)

(-each
    '(("from:Henry Gardner" "Hballs" ?h)
      ("to:benjamin.j.swift@gmail.com" "to gmail" ?g)
      ("list:extemporelang.googlegroups.com" "Extempore list" ?e))
  (lambda (b) (add-to-list 'mu4e-bookmarks b t)))

(setq mu4e-user-mailing-lists
      '(("extemporelang.googlegroups.com" . "Extempore")
        ("livecode.group.lurk.org" . "TOPLAP")
        ("acma-l.list.waikato.ac.nz" . "ACMA")
        ("llvm-dev@lists.llvm.org" . "LLVM")
        ("mu-discuss@googlegroups.com" . "mu-discuss")
        ("nanomsg@freelists.org" . "nanomsg")))

;; send

(require 'smtpmail-async)

(setq send-mail-function 'async-smtpmail-send-it
      message-send-mail-function 'async-smtpmail-send-it
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

;; contexts

(setq mu4e-contexts
      (list
       (make-mu4e-context
        :name "personal"
        :enter-func (lambda () (mu4e-message "switching to personal context"))
        ;; leave-func not defined
        :match-func (lambda (msg)
                      (when msg
                        (or (mu4e-message-contact-field-matches msg :to "ben@benswift.me")
                            (mu4e-message-contact-field-matches msg :to "extemporelang@googlegroups.com"))))
        :vars '((user-mail-address . "ben@benswift.me")
                (smtpmail-starttls-credentials '(("mail.messagingengine.com" 587 nil nil)))
                (smtpmail-smtp-server . "mail.messagingengine.com")))
       (make-mu4e-context
        :name "anu"
        :enter-func (lambda () (mu4e-message "switching to ANU context"))
        ;; leave-fun not defined
        :match-func (lambda (msg)
                      (when msg
                        (mu4e-message-contact-field-matches msg :to "ben.swift@anu.edu.au")))
        :vars '((user-mail-address . "ben.swift@anu.edu.au")
                (smtpmail-starttls-credentials '(("smtp.office365.com" 587 nil nil)))
                (smtpmail-smtp-server . "smtp.office365.com")))))


(defun ben-send-anu-email (email-address subject body &optional async cc-string)
  (with-temp-buffer
    (mu4e-context-switch nil "anu")
    (insert (format "From: Ben Swift <ben.swift@anu.edu.au>\nTo: %s\n%sSubject: %s\n--text follows this line--\n%s"
                    email-address
                    (if cc-string (format "Cc: %s\n" cc-string) "")
                    subject
                    body))
    (if async
        (async-smtpmail-send-it)
      (smtpmail-send-it))))

;;;;;;;;;;;;;;;
;; Extempore ;;
;;;;;;;;;;;;;;;

(cond
 ((string= (system-name) "Lonyx")
  (setq extempore-path "/home/ben/Code/extempore/")
  (setq user-extempore-lib-directory "/home/ben/Code/xtm/lib/"))
 ((string= (system-name) "WINYX")
  (setq extempore-program-args nil)
  (setq extempore-path "c:/Users/ben/Code/extempore/"))
 ((string= (system-name) "debian-vm")
  (setq extempore-program-args "--device 1 --frames 1024")
  (setq extempore-path "/home/ben/Code/extempore/")
  (setq user-extempore-lib-directory "/home/ben/Code/xtm/lib/"))
 (t ;; probably running on roughy
  (setq extempore-program-args nil)
  (setq extempore-path "~/Documents/research/extemporelang/extempore/")
  (setq user-extempore-lib-directory "~/Documents/research/extemporelang/xtm/")))

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
  (let* ((xtm-dir (expand-file-name "~/Documents/research/extemporelang/xtm/"))
         (base-path (concat xtm-dir "sessions/" name))
         (setup-header
          (format ";;; setup.xtm --- setup file for %s

(sys:load \"%slib/benlib-scm.xtm\")

dspmt" name xtm-dir)))
    (if (file-exists-p base-path)
        (error "Cannot create xtm session: directory already exists."))
    (make-directory base-path)
    (extempore-create-template-file
     base-path "practise.xtm" "headerp")
    (extempore-create-template-file
     base-path "gig.xtm" "headerp")
    (extempore-create-template-file
     base-path "setup.xtm" setup-header)
    (dired base-path)))

;;;;;;;;;;;;;;
;; org-mode ;;
;;;;;;;;;;;;;;

(setq org-directory (expand-file-name "~/Dropbox/org")
      org-default-notes-file (concat org-directory "/unfiled.org")
      org-agenda-files (list org-directory)
      org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))
      org-outline-path-complete-in-steps nil         ; Refile in a single go
      org-refile-use-outline-path t)

;;;;;;;;;;
;; misc ;;
;;;;;;;;;;

(defun slurp (f)
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(defun devdocs-lookup (language name)
  (interactive "slanguage: \nsname: ")
  (shell-command
   (format "open devdocs://search/%s"
           (url-hexify-string (concat language " " name)))))

(defun osx-screencapture (filename)
  (interactive "sfilename: ")
  (shell-command (format "screencapture -i \"%s.png\"" filename)))

(defun osx-screencapture-fullscreen (filename)
  (interactive "sfilename: ")
  (shell-command (format "screencapture -m \"%s.png\"" filename)))

;; this used to be called as part of the compose message hook
(defun ben-asciify-buffer-or-region (beg end)
  (interactive "r")
  (let ((asciify-alist '(("’" . "'")
                         ("‘" . "'")
                         ("“" . "\"")
                         ("”" . "\"")
                         ("—" . "---")
                         ("…" . "..."))))
    (unless (region-active-p)
      (setq beg (point-min))
      (setq end (point-max)))
    (save-excursion
      (-each asciify-alist
        (lambda (nonascii-char-pair)
          (goto-char beg)
          (while (search-forward (car nonascii-char-pair) end :noerror)
            (replace-match (cdr nonascii-char-pair) nil :literal)))))))

;;;;;;;;;;;;
;; church ;;
;;;;;;;;;;;;

(defun date-of-next-Sunday ()
  "return's next Sunday's date, as a string"
  (let ((next-sun (calendar-gregorian-from-absolute
                   (+ (calendar-absolute-from-gregorian (calendar-current-date))
                      (% (- 7 (string-to-number (format-time-string "%u"))) 7)))))
    (format "%04d-%02d-%02d"
            (nth 2 next-sun)
            (nth 0 next-sun)
            (nth 1 next-sun))))

(defun compile-church-chord-chart-pdf (num-songs)
  (interactive "nNumber of songs: ")
  (let* ((church-music-dir "/Users/ben/Documents/Church/Music/")
         (chord-charts-dir (concat church-music-dir "chord-charts/"))
         (lead-sheets-dir (concat church-music-dir "lead-sheets/"))
         (candidates (append (mapcar (lambda (f) (concat "chord-charts/" f)) (directory-files (concat church-music-dir "chord-charts/") nil "\\.pdf"))
                             (mapcar (lambda (f) (concat "lead-sheets/" f)) (directory-files (concat church-music-dir "lead-sheets/") nil "\\.pdf"))))
         (output-filename (format "/tmp/%s.pdf" (date-of-next-Sunday)))
         (charts (loop repeat num-songs collect (ivy-completing-read "chart: " candidates nil :require-match))))
    (let ((default-directory church-music-dir))
      (shell-command (format "pdfjam %s -o %s && open %s"
                             (mapconcat #'identity charts " ")
                             output-filename
                             output-filename)))))

(provide 'ben-utils)

;;; ben-utils.el ends here
