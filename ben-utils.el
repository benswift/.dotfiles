;;; ben-utils.el --- Ben's elisp utilities; the source of his powers

;; Copyright (c) 2017-2021 Ben Swift
;;
;; Author: Ben Swift <ben@benswift.me>
;; URL: https://github.com/benswift/.dotfiles/blob/master/ben-utils.el
;;
;; This file is not part of GNU Emacs.
;;
;;; License: MIT

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
(setq display-time-format "%H:%M"
      display-time-default-load-average nil)
(display-time-mode 1)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; indentation
(setq standard-indent 2)

;; use spaces, not tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; warnings to suppress
(add-to-list 'warning-suppress-types '(yasnippet backquote-change))

;;;;;;;;;;;;;;;;
;; projectile ;;
;;;;;;;;;;;;;;;;

(require 'projectile)

(setq compilation-buffer-name-function
      #'projectile-compilation-buffer-name
      compilation-save-buffers-predicate
      #'projectile-current-project-buffer-p)

;;;;;;;;;;;;
;; Jekyll ;;
;;;;;;;;;;;;

;; I do a *lot* of work with Jekyll static websites (including all my ANU course
;; websites) so these helpers save me heaps of time

(defun jekyll-list-asset-filenames ()
  (->> (projectile-current-project-files)
       (--filter (s-starts-with? "assets/" it))
       ;; in case there's heaps of js guff that you *probably* don't want
       (--remove (s-starts-with? "assets/js/" it))
       (--map (s-chop-prefix "assets/" it))))

(defun kramdown-slugify (text)
  "slugify text (as kramdown would) based on the regexps in
  basic_generate_id() available at:

https://github.com/gettalong/kramdown/blob/e9714d87e842831504503c7ed67f280873d98908/lib/kramdown/converter/base.rb#L232"
  (->> text
       (s-downcase)
       (s-replace " " "-")
       (s-replace-regexp "[^a-z0-9-]" "")))

(defun kramdown-list-slugified-headers ()
  (-map
   #'kramdown-slugify
   (progn
     (imenu--make-index-alist :noerror)
     imenu--index-alist)))

(defun kramdown-list-anchors (filename)
  "return a list of the (explicit) ids from a kramdown md file"
  (let* ((md-text (slurp filename))
         (matches (s-match-strings-all "{#\\([^}]*\\)}" md-text)))
    (--map (nth 1 it) matches)))

(defun yaml-list-top-level-keys (filename)
  "return a list of the top-level keys in a yaml file"
  (let* ((yaml-text (slurp filename))
         (matches (s-match-strings-all "^\\([a-zA-Z0-9_-]+\\):" yaml-text)))
    (--map (nth 1 it) matches)))

(defun jekyll-link-with-anchor (filename)
  (interactive
   (list (completing-read "file: "
                          (--filter (s-ends-with? ".md" it)
                                    (projectile-current-project-files))
                          nil
                          :require-match)))
  (format "{%% link %s %%}#%s"
          filename
          (completing-read "anchor: "
                           (kramdown-list-anchors (concat (projectile-project-root) filename))
                           nil
                           :require-match)))

(defun kramdown-id-for-current-line ()
  (interactive)
  (->> (buffer-substring-no-properties
        (line-beginning-position)
        (line-end-position))
       (replace-regexp-in-string "#+" "")
       (s-collapse-whitespace)
       (s-trim)
       (kramdown-slugify)
       (format "{#%s}")))

(defun image-dimensions (image-filename)
  "get image dimensions WxH (in pixels)

requires `identify' CLI program"
  (->> image-filename
       (format "identify -format \"%%wx%%h\" \"%s\"")
       shell-command-to-string
       (s-split "x")
       (-map #'string-to-number)))

(defun image-width (image-filename)
  "get image width (in pixels)

requires `identify' CLI program"
  (car (image-dimensions image-filename)))

(defun image-height (image-filename)
  "get image height (in pixels)

requires `identify' CLI program"
  (cadr (image-dimensions image-filename)))

(defun mogrify-width (image-filename width)
  "resize image (in-place) to `width'

requires `mogrify' CLI program"
  (let ((iw (image-width image-filename))
        (command-string (format "mogrify -resize \"%d\" \"%s\"" width image-filename)))
    (if (>= width iw)
        (progn (message "image is already %dpx wide---so I'll just leave it as-is" iw) 0)
      (shell-command command-string))))

(defun image-file? (filename)
  "filthy file-ext-based hack to tell if something's an image file

if you're using this with squoosh-file you can probably just
throw the file at squoosh and let it complain if it can't handle
it"
  (member (s-downcase (f-ext filename)) '("jpg" "jpeg" "png")))

(defun squoosh-file (filename)
  "squoosh an image file into a mozJPG-encoded jpg

requires the squoosh-cli from https://www.npmjs.com/package/@squoosh/cli"
  (interactive "f")
  (shell-command (format "squoosh-cli --mozjpeg auto %s" filename)))

(defun squoosh-and-resize-file (filename width)
  "squoosh an image file into a mozJPG-encoded jpg and resize to `WIDTH'

requires the squoosh-cli from https://www.npmjs.com/package/@squoosh/cli"
  (interactive "ffilename: \nnwidth: ")
  (shell-command (format "squoosh-cli --resize '{\"enabled\":true,\"width\":%s}' --mozjpeg auto %s" width filename)))

;; TODO defun probably should be interactive itself
(defun jekyll-completing-read-asset-subdirectory ()
  "return a list of (full paths to) subdirectories of the assets/ folder"
  (let* ((asset-root (f-join (projectile-project-root) "assets"))
         (candidates (cons "." (--map (f-relative it asset-root)
                                     (f-directories asset-root
                                                    (lambda (fname) (not (s-contains? ".git" fname)))
                                                    :recursive)))))
    (f-join asset-root (completing-read "assets/" candidates))))

(defun jekyll-move-download-and-mogrify (filename desired-width)
  "move file by default into the appropriate subfolder of assets/"
  (interactive
   (let* ((image-filename (completing-read "filename: "
                                           (f-files (expand-file-name "~/Downloads")
                                                    (lambda (fname)
                                                      (--any (s-ends-with? it fname)
                                                             '(".jpg" ".jpeg" ".png"))))
                                           nil
                                           :require-match))
          (default-width 1920)
          (original-width (image-width image-filename)))
     (list
      image-filename
      (when current-prefix-arg
        (read-number (format "desired image width (current %spx): " original-width) default-width)))))

  ;; downsize image if necessary
  (if (and current-prefix-arg (> (image-width filename) desired-width))
      (unless (= (mogrify-width filename desired-width) 0)
        (error "error mogrifying %s" filename)))

  ;; move the now processed image file into place
  (let* ((dest-path (f-join (jekyll-completing-read-asset-subdirectory)
                            (f-filename filename))))
    (f-move filename dest-path)
    ;; for convenience, copy the relevant "background image" Jekyll include
    (kill-new (f-relative dest-path (f-join (projectile-project-root) "assets")))))

(defun mogrify-image-file (filename desired-width)
  "note: this will never make the file wider

if DESIRED-WIDTH is greater than the file width, it'll just do
nothing"
  (interactive
   (list (read-file-name "file: ")
         (read-number "desired width (px): " 1920)))
  (if (> (image-width filename) desired-width)
      (shell-command (format "mogrify -resize \"%d\" %s" desired-width filename))
    (message "%s is already narrower than %dpx, skipping..." filename desired-width)))

(defun jekyll-save-screenshot (filename)
  (interactive "sfilename (sans extension): ")
  (let* ((width 1024)
         (dest-path (f-join (jekyll-completing-read-asset-subdirectory) filename))
         (project-relative-path (f-relative dest-path (f-join (projectile-project-root) "assets"))))
    (shell-command (format "screencapture -t jpg -i \"%s.jpg\"" dest-path))
    (mogrify-image-file dest-path width)
    (kill-new (format "![%s](%s.jpg)" (s-replace "-" " " filename) project-relative-path))))

;; TODO it'd be nice if this worked on the currently selected files in a dired buffer
(defun mogrify-image-files-recursively (dir max-width)
  (interactive
   (list (read-directory-name "directory: ")
         (read-number "max-width: " 1920)))
  (--each
      (directory-files-recursively dir "\.\\(jpg\\|jpeg\\|png\\)$")
    (mogrify-image-file it max-width)))

;; helpful keybindings
(spacemacs/declare-prefix "o" "user-prefix")
(spacemacs/set-leader-keys "om" 'jekyll-move-download-and-mogrify)
(spacemacs/set-leader-keys "oc"
  '(lambda ()
     (interactive)
     (switch-to-buffer (projectile-compilation-buffer-name "compilation"))))

;; Jekyll ANU theme helpers

(defvar anu-jekyll-theme-site-dirs
  '(
    "~/Documents/teaching/tools/cecs-jekyll/neo/cybernetics-website/"
    "~/Documents/teaching/tools/cecs-jekyll/neo/computing-website/"
    "~/Documents/teaching/tools/cecs-jekyll/neo/engineering-website/"
    "~/Documents/research/ccc-studio/website/"
    "~/Documents/teaching/extn1019/website/"
    "~/Documents/teaching/comp2710-lens-2021/website/"
    "~/Documents/teaching/tools/cecs-jekyll/docs/"
    "~/Documents/teaching/cs-outreach-hub/website/"
    "~/Documents/research/ccc-studio/website/"
    ))

(defun anu-jekyll-theme-update-all ()
  (interactive)
  (--each
      anu-jekyll-theme-site-dirs
    (let ((default-directory it))
      (async-shell-command "git pull --rebase && bundle update && bundle exec jekyll build && git add \"Gemfile.lock\" && git commit -m \"bundle update\" && git push" (get-buffer-create (projectile-compilation-buffer-name "jekyll-update"))))))

;;;;;;;;;;
;; mu4e ;;
;;;;;;;;;;

;; to init, try something like
;; mu init --my-address=ben@benswift.me --my-address=benswift@fastmail.com --my-address=ben.swift@anu.edu.au --my-address=benjamin.j.swift@gmail.com --my-address=ben.swift@simeonnetwork.org

;; these should be set before calling mu4e, and they shouldn't break anything if
;; we're on a machine where (require 'mu4e) fails
(setq mu4e-get-mail-command "mbsync fastmail"
      mu4e-attachment-dir (expand-file-name "~/Downloads"))

(when (spacemacs/system-is-linux)
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e"))

(when (require 'mu4e nil :noerror)
  (require 'mu4e-contrib) ;; for mu4e-shr2text
  (require 'smtpmail)

  (setq user-full-name "Ben Swift")

  ;; headers
  (setq mu4e-headers-include-related nil)

  ;; receive

  (setq mu4e-update-interval 300
        mu4e-headers-auto-update t
        mu4e-change-filenames-when-moving t
        mu4e-view-show-addresses t)

  (defun mu4e-pretty-mbsync-process-filter (proc msg)
    (ignore-errors
      (with-current-buffer (process-buffer proc)
        (let ((inhibit-read-only t))
          (delete-region (point-min) (point-max))
          (insert (car (reverse (split-string msg "\r"))))
          (when (re-search-backward "\\(C:\\).*\\(B:\\).*\\(F:\\).*\\(N:\\)")
            (add-face-text-property
             (match-beginning 1) (match-end 1) 'font-lock-keyword-face)
            (add-face-text-property
             (match-beginning 2) (match-end 2) 'font-lock-function-name-face)
            (add-face-text-property
             (match-beginning 3) (match-end 3) 'font-lock-builtin-face)
            (add-face-text-property
             (match-beginning 4) (match-end 4) 'font-lock-type-face))))))

  ;; compose

  (setq mu4e-compose-dont-reply-to-self t
        mu4e-compose-signature-auto-include nil
        mu4e-compose-format-flowed t
        fill-flowed-encode-column fill-column
        message-citation-line-format "On %Y-%m-%d at %R %Z, %f wrote...")

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
        '(("/personal/Archive" . ?a)
          ("/personal/Junk Mail" . ?j)
          ("/anu/Archive" . ?s)
          ("/anu/Junk E-Mail" . ?k)))

  (setq mu4e-bookmarks
        '((:name "Unread" :query "flag:unread AND NOT flag:trashed" :key ?u)
          (:name "Inbox" :query "date:20210101..now AND maildir:/anu/INBOX OR maildir:/personal/INBOX" :key ?i)
          (:name "Today's messages" :query "date:today..now" :key ?t)
          (:name "Archive" :query "maildir:/personal/Archive OR maildir:/anu/Archive" :key ?a)
          (:name "Drafts" :query "maildir:/personal/Drafts OR maildir:/anu/Drafts" :key ?d)
          (:name "Sent items" :query "\"maildir:/personal/Sent Items\" OR \"maildir:/anu/Sent Items\"" :key ?s)
          (:name "Trash" :query "maildir:/personal/Trash OR \"maildir:/anu/Deleted Items\"" :key ?T)))

  (setq mu4e-headers-date-format "%e %b %y"
        mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:maildir . 10)
                              (:from . 22)
                              (:subject)))

  ;; send

  (require 'smtpmail-async)

  (setq send-mail-function 'smtpmail-send-it
        message-send-mail-function 'smtpmail-send-it
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
                          (s-starts-with? "/personal" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "ben@benswift.me")
                  (mu4e-sent-folder . "/personal/Sent Items")
                  (mu4e-refile-folder . "/personal/Archive")
                  (mu4e-drafts-folder . "/personal/Drafts")
                  (mu4e-trash-folder . "/personal/Trash")
                  (smtpmail-starttls-credentials '(("mail.messagingengine.com" 587 nil nil)))
                  (smtpmail-smtp-server . "mail.messagingengine.com")))
         (make-mu4e-context
          :name "anu"
          :enter-func (lambda () (mu4e-message "switching to ANU context"))
          ;; leave-fun not defined
          :match-func (lambda (msg)
                        (when msg
                          (s-starts-with? "/anu" (mu4e-message-field msg :maildir))))
          :vars '((user-mail-address . "ben.swift@anu.edu.au")
                  (mu4e-sent-folder . "/anu/Sent Items")
                  (mu4e-refile-folder . "/anu/Archive")
                  (mu4e-drafts-folder . "/anu/Drafts")
                  (mu4e-trash-folder . "/anu/Deleted Items")
                  (smtpmail-starttls-credentials '(("smtp.office365.com" 587 nil nil)))
                  (smtpmail-smtp-server . "smtp.office365.com")))))

  (defun ben-send-anu-email (email-address subject body dry-run &optional cc-string)
    (let ((email-text (format "From: Ben Swift <ben.swift@anu.edu.au>\nTo: %s\n%sSubject: %s\n--text follows this line--\n%s"
                              email-address
                              (if cc-string (format "Cc: %s\n" cc-string) "")
                              subject
                              body)))
      (with-current-buffer (find-file (f-join (temporary-file-directory) "anu-email-text"))
        (delete-region (point-min) (point-max))
        (insert email-text)
        (message-mode)
        (mu4e-context-switch nil "anu")
        (if dry-run
            (pop-to-buffer (current-buffer))
          (smtpmail-send-it)))))

  ;; iCal integration

  (when (spacemacs/system-is-mac)
    (require 'mu4e-icalendar)
    (mu4e-icalendar-setup))

  ;; mimetype-specific handlers (this could be super-cool)

  ;; ...but this one doesn't work
  ;; (mailcap-add "text/calendar" "open -a /Applications/Calendar.app %s")

  ;; (when (require 'mu4e... ) ends here
  )

;; because they seemed to remove the old `SPC A m' keybinding
(spacemacs/set-leader-keys "oe" 'mu4e)

;;;;;;;;;;;;;;;
;; Extempore ;;
;;;;;;;;;;;;;;;

(cond
 ((string= (system-name) "paranoid-android")
  (setq extempore-path (expand-file-name "~/Documents/research/extemporelang/extempore/")))
 ((string= "smithy" (system-name))
  ;; (setq extempore-program-args "--frames 128")
  (setq extempore-path (expand-file-name "~/Documents/research/extemporelang/extempore/"))
  (setq user-extempore-lib-directory (expand-file-name "~/Documents/research/extemporelang/xtm/")))
 (t
  (message "unrecognised machine (%s), skipping Extempore mode var setup" (system-name))))

;; handy for tapping out rhythms

(defvar extempore-pattern-hydra-hit-value "1" "value for the 'hit'")
(defvar extempore-pattern-hydra-rest-value "_" "value for the 'rest'")

;; NOTE: this doesn't work if there's not at least one more character on the
;; line e.g. a close paren)
(defun extempore-pattern-hydra-insert (value)
  (if (looking-back "(" (- (point) 1))
      ;; we're in an empty list
      (insert value)
    (insert (format " %s" value))))

(spacemacs|define-transient-state extempore-pattern-hydra-tap-rhythm
  :title "Tap out an extempore pattern using the keyboard"
  :doc
  "\n[_j_] hit [_f_] rest [_J_] set hit value [_F_] set rest value [_q_] quit"
  :bindings
  ("j" (extempore-pattern-hydra-insert extempore-pattern-hydra-hit-value))
  ("f" (extempore-pattern-hydra-insert extempore-pattern-hydra-rest-value))
  ("J" (setq-local extempore-pattern-hydra-hit-value (read-string "hit value: ")))
  ("F" (setq-local extempore-pattern-hydra-rest-value (read-string "rest value: ")))
  ("q" nil :exit t))

(spacemacs/set-leader-keys-for-major-mode 'extempore-mode
  "rr" 'spacemacs/extempore-pattern-hydra-tap-rhythm-transient-state/body)

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

;; llvm-emacs utils

(setq load-path
      (cons (expand-file-name "~/.dotfiles/llvm-emacs") load-path))

(require 'llvm-mode)
(require 'tablegen-mode)

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

;; Ben's livecoding snippet helpers

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
      (with-temp-buffer
        (insert-file-contents (concat user-extempore-lib-directory "sampler-maps.xtm"))
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
          (cl-case maj-min
            ('^ 5 '(i i6 i64 i7 i- i-7 n n6 ii ii6 ii7 ii9 ii^ ii^7 iii iii6 iii7 iii^ iii^7 iv iv6 iv7 iv- iv-7 v v6 v7 v- v-7 vi vi6 vi7 vi^ vi^7 viio viio7 vii vii7))
            ('- '(i i6 i64 i7 i^ i^6 i^64 i^7 n n6 ii ii6 ii7 ii- ii-6 ii-7 ii^ ii^7 iii iii6 iii7 iii- iii-6 iii-7 iv iv6 iv7 iv^ iv^6 iv^7 v v^ v6 v7 v- v-6 v-6 v-7 vi vi6 vi7 vi- vi-6 vi-7 vii vii6 vii7 viio viio6 viio7))
            (t nil))))

;;;;;;;;;;;;;;;;;
;; tidalcycles ;;
;;;;;;;;;;;;;;;;;

(setq tidal-interpreter (expand-file-name "~/.ghcup/bin/ghci"))

;;;;;;;;;;;;;;
;; org-mode ;;
;;;;;;;;;;;;;;

(use-package org-roam-bibtex
  :after org-roam
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :custom
  (orb-preformat-keywords '("citekey" "title" "url" "author-or-editor" "keywords" "file"))
  (orb-file-field-extensions '("pdf" "epub" "html")))

(use-package org-pdftools
  :hook (org-load . org-pdftools-setup-link))

(use-package org-noter
  :after (:any org pdf-view)
  :custom
  (org-noter-always-create-frame nil)
  (org-noter-separate-notes-from-heading t)
  (org-noter-default-notes-file-names '("notes.org"))
  (org-noter-notes-search-path (list org-roam-directory)))

(use-package org-noter-pdftools
  :after org-noter
  :config
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

;; (with-eval-after-load 'pdf-view
;;   (evil-define-key 'visual pdf-view-mode-map "<prior>" 'pdf-view-scroll-down-or-previous-page)
;;   (evil-define-key 'visual pdf-view-mode-map "<next>" 'pdf-view-scroll-up-or-next-page))

(spacemacs/set-leader-keys "od" 'org-roam-dailies-goto-today)

(defun ben-sync-org-directory-to-github ()
  (interactive)
  (let ((default-directory org-directory))
    (async-shell-command
     (format "git add *.org roam/*.org roam/daily/*.org && git commit -m 'org directory auto-commit script @ %s' && git pull --rebase origin master && git push origin master"
             (format-time-string "%FT%T%z")))))

(defun ben-update-spacemacs ()
  (interactive)
  (let ((default-directory (expand-file-name "~/.emacs.d")))
    (shell-command "git pull origin develop")
    (configuration-layer/update-packages :no-confirmation)))

;;;;;;;;;;;;;
;; devdocs ;;
;;;;;;;;;;;;;

(require 'devdocs)

;; this will override `devdocs-do-search' from the official lib to instead use
;; the "local" URL (only works on macOS I think)
;; this is brittle---if devdocs.el ever changes the implementation of
;; `devdocs-search' then this may well stop working

(defun devdocs-do-search (pattern)
  (shell-command
   (format "open devdocs://search/%s" (url-hexify-string pattern))))

;; not really using this atm, so let's remove the precious "SPC o" keybinding
;; (spacemacs/set-leader-keys "os" 'devdocs-search)

;;;;;;;;;;;;;;;
;; spaceline ;;
;;;;;;;;;;;;;;;

;; this should be done somewhere else, I probably need to set up my own custom spaceline theme
(spaceline-toggle-buffer-encoding-abbrev-off)

;;;;;;;;;;
;; misc ;;
;;;;;;;;;;

;; from https://stackoverflow.com/a/49505968
(defun shuffle (sequence)
  (cl-loop for i from (length sequence) downto 2
        do (cl-rotatef (elt sequence (random i))
                    (elt sequence (1- i))))
  sequence)

(defun slurp (f)
  (with-temp-buffer
    (insert-file-contents f)
    (buffer-substring-no-properties
     (point-min)
     (point-max))))

(require 'csv)

(defun read-csv (filename headerp)
  (with-temp-buffer
    (insert-file-contents filename)
    (csv-parse-buffer headerp)))

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
                         (" " . " ") ;; sometimes necessary with copy-pasted stuff
                         (" " . " ") ;; sometimes necessary with copy-pasted stuff
                         ("“" . "\"")
                         ("”" . "\"")
                         ("–" . "-")
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

(defun xah-title-case-region-or-line (@begin @end)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, *begin *end are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2017-01-11"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           $p1
           $p2
           ($skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward $skipChars (line-beginning-position))
         (setq $p1 (point))
         (skip-chars-forward $skipChars (line-end-position))
         (setq $p2 (point)))
       (list $p1 $p2))))
  (let* (
         ($strPairs [
                     [" A " " a "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ["'T " "'t "]
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region @begin @end)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda ($x)
             (goto-char (point-min))
             (while
                 (search-forward (aref $x 0) nil t)
               (replace-match (aref $x 1) "FIXEDCASE" "LITERAL")))
           $strPairs))))))

(defun xah-upcase-sentence ()
  "Upcase first letters of sentences of current text block or selection.

Modified by me - I don't like the overlays, so I got rid of them.

URL `http://ergoemacs.org/emacs/emacs_upcase_sentence.html'
Version 2019-06-21"
  (interactive)
  (let ($p1 $p2)
    (if (region-active-p)
        (setq $p1 (region-beginning) $p2 (region-end))
      (save-excursion
        (if (re-search-backward "\n[ \t]*\n" nil "move")
            (progn
              (setq $p1 (point))
              (re-search-forward "\n[ \t]*\n"))
          (setq $p1 (point)))
        (progn
          (re-search-forward "\n[ \t]*\n" nil "move")
          (setq $p2 (point)))))
    (save-excursion
      (save-restriction
        (narrow-to-region $p1 $p2)
        (downcase-region $p1 $p2)
        (let ((case-fold-search nil))
          (goto-char (point-min))
          (while (re-search-forward "\\. \\{1,2\\}\\([a-z]\\)" nil "move") ; after period
            (upcase-region (match-beginning 1) (match-end 1)))

          ;;  new line after period
          (goto-char (point-min))
          (while (re-search-forward "\\. ?\n *\\([a-z]\\)" nil "move")
            (upcase-region (match-beginning 1) (match-end 1)))

          ;; after a blank line, after a bullet, or beginning of buffer
          (goto-char (point-min))
          (while (re-search-forward "\\(\\`\\|• \\|\n\n\\)\\([a-z]\\)" nil "move")
            (upcase-region (match-beginning 2) (match-end 2)))

          ;; for HTML. first letter after tag
          (goto-char (point-min))
          (while (re-search-forward "\\(<p>\n?\\|<li>\\|<td>\n?\\|<figcaption>\n?\\)\\([a-z]\\)" nil "move")
            (upcase-region (match-beginning 2) (match-end 2)))

          (goto-char (point-min)))))))

;; this overrides an eyebrowse keybinding, but I don't use that
(evil-global-set-key 'motion "gt" #'xah-upcase-sentence)

;;; Stefan Monnier <foo at acm.org>. It is the opposite of fill-paragraph
(defun unfill-paragraph (&optional region)
  "Takes a multi-line paragraph and makes it into a single line of text."
  (interactive (progn (barf-if-buffer-read-only) '(t)))
  (let ((fill-column (point-max))
        ;; This would override `fill-column' if it's an integer.
        (emacs-lisp-docstring-fill-column t))
    (fill-paragraph nil region)))

(require 'sgml-mode)

(defun ben-reformat-xml ()
  (interactive)
  (save-excursion
    (sgml-pretty-print (point-min) (point-max))
    (indent-region (point-min) (point-max))))

(require 'htmlize)

(defun ben-htmlize-region-save-screenshot (beg end)
  (interactive "r")
  (deactivate-mark) ;; so that the screenshot doesn't have the highlighting
  (htmlize-region-save-screenshot beg end))

(defun ben-symlink-dotfiles ()
  (interactive)
  (cl-flet ((linker (lambda (target linkname)
                      (make-symbolic-link (expand-file-name (format "~/.dotfiles/%s" target))
                                          (expand-file-name (format "~/%s" linkname))
                                          :ok-if-it-already-exists))))
    (linker "profile" ".zprofile") ;; assumes zsh
    (linker "spacemacs" ".spacemacs")
    (linker "gitconfig" ".gitconfig")
    (linker "gitignore" ".gitignore")
    (linker "mbsyncrc" ".mbsyncrc")
    (linker "ssh_config" ".ssh/config")
    (linker "RProfile" ".RProfile")))

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
         (charts (cl-loop repeat num-songs collect (ivy-completing-read "chart: " candidates nil :require-match))))
    (let ((default-directory church-music-dir))
      (shell-command (format "pdfjam %s -o %s && open %s"
                             (mapconcat #'identity charts " ")
                             output-filename
                             output-filename)))))

(provide 'ben-utils)

;;; ben-utils.el ends here
