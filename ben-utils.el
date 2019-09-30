;;; ben-utils.el --- Ben's utility functions -*- lexical-binding: t -*-

;; Author: Ben Swift
;; Maintainer: Ben Swift
;; Version: 1.0
;; Package-Requires: (csv url-util mu4e-contrib smtpmail smtpmail-async gnus-dired)
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

;;;;;;;;;;;;
;; Jekyll ;;
;;;;;;;;;;;;

;; I do a *lot* of work with Jekyll static websites (including all my ANU course
;; websites) so these helpers save me heaps of time

(require 'projectile)

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

(defun image-width (image-filename)
  "get image width (in pixels)

requires `identify' CLI program"
  (string-to-number (shell-command-to-string (format "identify -format \"%%w\" \"%s\"" image-filename))))

(defun image-height (image-filename)
  "get image height (in pixels)

requires `identify' CLI program"
  (string-to-number (shell-command-to-string (format "identify -format \"%%h\" \"%s\"" image-filename))))

(defun mogrify-width (image-filename width)
  "resize image (in-place) to `width'

requires `mogrify' CLI program"
  (let ((iw (image-width image-filename))
		(command-string (format "mogrify -resize \"%d\" \"%s\"" width image-filename)))
	(if (>= width iw)
		(progn (message "image is already %dpx wide---so I'll just leave it as-is" iw) 0)
	  (shell-command command-string))))

(defun imageoptim-file (filename-or-glob)
  "also apply some standard, useful optimisations"
  (interactive "sfilename-or-glob: ")
  (call-process "imageoptim" nil nil nil "--jpegmini" filename-or-glob))

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
	  (read-number (format "desired image width (current %spx): " original-width) default-width))))
  (when (or (< (image-width filename) desired-width) ;; don't enlarge it, but...
			(= (mogrify-width filename desired-width) 0)) ;; ...downsize if necessary
	;; run imageoptim as well---assuming JPEGmini is playing nice with Emacs
	;; (= (imageoptim-file filename) 0)
	(let ((asset-root (f-join (projectile-project-root) "assets")))
	  (unless (f-directory? asset-root)
		(error "no assets/ folder in projectile root - are you sure you're in the right project?"))
	  (f-move filename
			  (f-join asset-root
					  (completing-read "assets/" (cons "." (--map (f-relative it asset-root)
																  (f-directories asset-root
																				 (lambda (fname) (not (s-contains? ".git" fname)))
																				 :recursive))))
					  (f-filename filename))))))

(defun mogrify-image-file (filename max-width)
  (interactive
   (list (read-file-name "file: ")
		 (read-number "max-width: " 1920)))
  (shell-command (format "mogrify -resize \"%d\" %s" max-width filename)))

;; TODO it'd be nice if this worked on the currently selected files in a dired buffer
(defun mogrify-image-files-recursively (dir max-width)
  (interactive
   (list (read-directory-name "directory: ")
		 (read-number "max-width: " 1920)))
  (-each
	  (directory-files-recursively dir "\.\\(jpg\\|jpeg\\|png\\)$")
	(lambda (fname)
	  (when (> (image-width fname) max-width)
		(shell-command (format "mogrify -resize \"%d\" %s" max-width fname))))))

;; helpful keybindings
(spacemacs/declare-prefix "o" "user-prefix")
(spacemacs/set-leader-keys "om" 'jekyll-move-download-and-mogrify)
(spacemacs/set-leader-keys "oc" '(lambda () (interactive) (switch-to-buffer "*compilation*")))

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
	  org-default-notes-file (concat org-directory "/inbox.org")
	  org-agenda-files (list org-directory)
	  org-refile-targets '((nil :maxlevel . 9) (org-agenda-files :maxlevel . 9))
	  org-outline-path-complete-in-steps nil         ; Refile in a single go
	  org-refile-use-outline-path t)

;; org-ref & helm-bibtex

(setq org-ref-bibliography-notes (concat org-directory "/zotero-notes.org")
	  bibtex-completion-bibliography (expand-file-name "~/Documents/Zotero/export.bib"))

;;;;;;;;;;
;; misc ;;
;;;;;;;;;;

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

(defun devdocs-lookup (language name)
  (interactive "slanguage: \nsname: ")
  (shell-command
   (format "open devdocs://search/%s"
		   (url-hexify-string (concat language " " name)))))

(defun ben-send-iMessage (to-number message-text)
  (do-applescript
   (format
	"tell application \"Messages\"
		  send %s to buddy \"+61%s\" of service \"E:benswift@me.com\"
end tell"
	(prin1-to-string message-text)
	to-number)))

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

(defun ben-symlink-dotfiles ()
  (interactive)
  (cl-flet ((linker (lambda (target linkname)
					  (make-symbolic-link (expand-file-name (format "~/.dotfiles/%s" target))
										  (expand-file-name (format "~/%s" linkname))
										  :ok-if-it-already-exists))))
	(linker "bash_profile.osx" ".bash_profile")
	(linker "spacemacs" ".spacemacs")
	(linker "gitconfig" ".gitconfig")
	(linker "gitignore" ".gitignore")
	(linker "mbsyncrc" ".mbsyncrc")
	(linker "spacemacs-layers/extempore" ".emacs.d/private/extempore")
	(linker "ssh_config" ".ssh/config")
	(linker "scripts" "bin")
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
		 (charts (loop repeat num-songs collect (ivy-completing-read "chart: " candidates nil :require-match))))
	(let ((default-directory church-music-dir))
	  (shell-command (format "pdfjam %s -o %s && open %s"
							 (mapconcat #'identity charts " ")
							 output-filename
							 output-filename)))))

(provide 'ben-utils)

;;; ben-utils.el ends here
