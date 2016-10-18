;;; init.el --- Ben Swift's Emacs init file

;; Copyright (C) 2008-2015 Ben Swift

;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; Author: Ben Swift <ben@benswift.me>
;; Homepage: https://github.com/benswift/.dotfiles

;; This file is not part of GNU Emacs.

;;; Commentary:

;; My emacs init file - an ever-growing bag of tricks and hacks. Where
;; I've stolen code from emacswiki, stackoverflow and other places,
;; I've tried to acknowledge it.

;;; Code:

;;;;;;;;;;
;; elpa ;;
;;;;;;;;;;

(require 'package)

(setq
 package-archives
 '(("melpa" . "http://melpa.org/packages/")
   ("org" . "http://orgmode.org/elpa/")
   ("gnu" . "http://elpa.gnu.org/packages/"))
 package-archive-exclude-alist
 '(("melpa" org)
   ("gnu" org)
   ("org" org)))

(unless package--initialized
  (package-initialize))

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar ben-package-list
  '(ag
    async
    avy
    auctex
    cider
    cmake-mode
    clojure-snippets
    company
    counsel
    dash-at-point
    docker
    dockerfile-mode
    elpy
    erc-hl-nicks
    erc-terminal-notifier
    ess
    exec-path-from-shell
    extempore-mode
    flatui-theme
    flycheck
    glsl-mode
    htmlize
    hydra
    imenu-anywhere
    intero
    less-css-mode
    lispy
    jedi
    json-mode
    magit
    markdown-mode
    monokai-theme
    multiple-cursors
    osc
    org
    paradox
    persistent-scratch
    processing-mode
    projectile
    rainbow-delimiters
    rainbow-mode
    realgud
    sane-term
    s
    scss-mode
    smart-mode-line
    spacemacs-theme
    string-utils
    swiper
    undo-tree
    unidecode
    vagrant
    vimrc-mode
    wgrep
    wgrep-ag
    yaml-mode
    yasnippet
    zoom-frm)
  "a list of packages that Ben likes to have installed")

(dolist (package ben-package-list)
  (if (not (package-installed-p package))
      (package-install package)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cross-platform setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ben-set-default-faces (face-height)
  (set-face-attribute 'default nil :height face-height :family "Source Code Pro")
  (set-face-attribute 'variable-pitch nil :height face-height :family "Source Sans Pro"))

;; linux

(defun ben-linux-setup ()
  (ben-set-default-faces 140)
  (setq frame-maximization-mode 'maximized)
  (ben-setup-keybindings)
  ;; exec-path-from-shell is set up on OSX as well
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("EDITOR" "EXT_LLVM_DIR" "LD_LIBRARY_PATH")))

;; OSX

(defun spotlight-locate-make-command-line (search-string)
  (list "mdfind" "-interpret" search-string))

(defun ben-setup-keybindings ()
  (define-key global-map (kbd "s-k") 'kill-this-buffer))

(defun ben-osx-setup ()
  (ben-set-default-faces 140)
  (setq mac-option-modifier 'meta)
	(setq mac-command-modifier 'super)
  (setq helm-locate-command "mdfind -name %s %s")
  (setq locate-make-command-line 'spotlight-locate-make-command-line)
  (setq x-bitmap-file-path '("/usr/X11/include/X11/bitmaps"))
  (setq source-directory "/Library/Caches/Homebrew/emacs-mac--git")
  (setq dired-guess-shell-alist-user '(("\\.pdf\\'" "open")
                                       ("\\.doc\\'" "open")
                                       ("\\.docx\\'" "open")
                                       ("\\.pdf\\'" "open")
                                       ("\\.xls\\'" "open")))
  (setq frame-maximization-mode 'fullscreen)
  ;; for railwaycat emacs-mac
  (ben-setup-keybindings)
  ;; exec-path-from-shell is set up on linux as well
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("EDITOR" "EXT_LLVM_DIR" "LD_LIBRARY_PATH")))

;; Windows

(defun ben-windows-setup ()
  (setq default-directory "C:/Users/ben/" )
  (ben-set-default-faces 140)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (setq compile-command "cmake -G\"Visual Studio 14 2015 Win64\" .. && cmake --build . --config Release --target")
  (ben-setup-keybindings))

(cond ((equal system-type 'gnu/linux) (ben-linux-setup))
      ((equal system-type 'darwin) (ben-osx-setup))
      ((equal system-type 'windows-nt) (ben-windows-setup)))

;;;;;;;;;;;;;;;;;;;
;; customisation ;;
;;;;;;;;;;;;;;;;;;;

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;;;;;;;;;;;;;;
;; movement ;;
;;;;;;;;;;;;;;

(global-set-key (kbd "C-<") 'beginning-of-buffer-other-window)
(global-set-key (kbd "C->") 'end-of-buffer-other-window)

;;;;;;;;;
;; ivy ;;
;;;;;;;;;

(setq ivy-height 20)
(setq ivy-use-virtual-buffers t)
(ivy-mode 1)
(global-set-key (kbd "C-s") 'counsel-grep-or-swiper)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "<f1> l") 'counsel-load-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "C-c k") 'counsel-ag)
(global-set-key (kbd "C-x l") 'counsel-locate)
;; push/pop view
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;;;;;;;;;
;; avy ;;
;;;;;;;;;

(avy-setup-default)
(global-set-key (kbd "C-M-;") 'avy-goto-char)
(global-set-key (kbd "C-:") 'avy-goto-line)
(global-set-key (kbd "C-;") 'avy-goto-word-1)
(global-set-key (kbd "M-g e") 'avy-goto-word-0)
;; set avy keys to home keys
(setq avy-keys '(?a ?s ?d ?f ?j ?k ?l ?\;))

;;;;;;;;;;;
;; hydra ;;
;;;;;;;;;;;

;; this here so we don't shadow the avy binding
(setq iedit-toggle-key-default nil)

(require 'hydra)

(defhydra hydra-multiple-cursors (:hint nil :columns 3)
  "Multiple cursors"
  ("f" mc/mark-next-like-this "mark next")
  ("s" mc/mark-previous-like-this "mark prev")
  ("c" mc/mark-all-like-this-dwim "mark dwim")
  ("F" mc/skip-to-next-like-this "skip next")
  ("S" mc/skip-to-previous-like-this "skip prev")
  ("d" mc--mark-symbol-at-point "mark symbol")
  ("M-f" mc/unmark-next-like-this "unmark next")
  ("M-s" mc/unmark-previous-like-this "unmark prev")
  ("r" mc/mark-all-in-region-regexp "mark regexp" :exit t)
  ("n" mc/insert-numbers "numbers" :exit t)
  ("l" mc/insert-letters "letters" :exit t)
  ("q" nil "exit"))

(global-set-key (kbd "C-c z") #'hydra-multiple-cursors/body)

;;;;;;;;;;;
;; lispy ;;
;;;;;;;;;;;

(require 'lispy)

(add-hook #'emacs-lisp-mode-hook #'lispy-mode 1)
(add-hook #'extempore-mode-hook #'lispy-mode 1)
(add-hook #'clojure-mode-hook #'lispy-mode 1)
(add-hook #'scheme-mode-hook #'lispy-mode 1)

;; don't know why this doesn't have a keybinding
(with-eval-after-load "lispy"
  (lispy-define-key lispy-mode-map-special "L" 'lispy-wrap-round))

;;;;;;;;;;;;;;;;;;;;;;;;
;; garbage collection ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(setq gc-cons-threshold 50000000)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; display & appearance ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq visible-bell t)
(setq inhibit-startup-message t)
(setq color-theme-is-global t)
(setq bidi-display-reordering nil)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1)
  (dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
    (when (fboundp mode) (funcall mode -1))))

(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 2)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(global-set-key (kbd "C-c i") 'counsel-imenu)

(show-paren-mode 1)

(column-number-mode 1)
(hl-line-mode t)

;; from http://www.emacswiki.org/emacs/ToggleWindowSplit
(defun window-toggle-split-direction ()
  "Switch window split from horizontally to vertically, or vice versa.

i.e. change right window to bottom, or change bottom window to right."
  (interactive)
  (require 'windmove)
  (let ((done))
    (dolist (dirs '((right . down) (down . right)))
      (unless done
        (let* ((win (selected-window))
               (nextdir (car dirs))
               (neighbour-dir (cdr dirs))
               (next-win (windmove-find-other-window nextdir win))
               (neighbour1 (windmove-find-other-window neighbour-dir win))
               (neighbour2 (if next-win (with-selected-window next-win
                                          (windmove-find-other-window neighbour-dir next-win)))))
          ;;(message "win: %s\nnext-win: %s\nneighbour1: %s\nneighbour2:%s" win next-win neighbour1 neighbour2)
          (setq done (and (eq neighbour1 neighbour2)
                          (not (eq (minibuffer-window) next-win))))
          (if done
              (let* ((other-buf (window-buffer next-win)))
                (delete-window next-win)
                (if (eq nextdir 'right)
                    (split-window-vertically)
                  (split-window-horizontally))
                (set-window-buffer (windmove-find-other-window neighbour-dir) other-buf))))))))

(global-set-key (kbd "C-c y") 'window-toggle-split-direction)

;; show time and battery status in mode line

(setq display-time-format "%H:%M")
(display-time-mode 1)
(display-battery-mode 1)

;; whitespace

(setq sentence-end-double-space nil)
(setq shift-select-mode nil)
(setq whitespace-style '(face trailing lines-tail tabs))
(setq whitespace-line-column 80)
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; mark region commands as safe

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; text mode tweaks

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(remove-hook 'text-mode-hook 'smart-spacing-mode)

;; file visiting

(setq save-place t
      save-place-file (concat user-emacs-directory "places")
      recentf-max-saved-items 100)

;; file backups

(setq backup-by-copying t
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

(global-auto-revert-mode t)

;; uniquify

(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator ":")

;; other niceties

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq diff-switches "-u")

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(defalias 'yes-or-no-p 'y-or-n-p)

;; transparency

(defun set-current-frame-alpha (value)
  (interactive
   (list (read-number "Frame alpha: " 1.0)))
   (set-frame-parameter (selected-frame) 'alpha value))

;; full/half screen

(define-key global-map (kbd "C-s-f") 'toggle-frame-fullscreen)

(defun frame-resize-halfscreen-left ()
  (interactive)
  (set-frame-size (selected-frame) (- (/ (display-pixel-width) 2) 15) (display-pixel-height) :pixelwise)
  (set-frame-position (selected-frame) 0 0))

(defun frame-resize-halfscreen-right ()
  (interactive)
  (set-frame-size (selected-frame) (- (/ (display-pixel-width) 2) 15) (display-pixel-height) :pixelwise)
  (set-frame-position (selected-frame) (/ (display-pixel-width) 2) 0))

(define-key global-map (kbd "C-s-h") 'frame-resize-halfscreen-left)

;; hide certain minor modes from mode line

(setq eldoc-minor-mode-string nil)

;; pretty lambdas

(add-hook 'prog-mode-hook
          '(lambda ()
             (font-lock-add-keywords
              nil `(("(?\\(lambda\\>\\)"
                     (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                               ,(make-char 'greek-iso8859-7 107))
                               nil)))))))

;;;;;;;;;;;;;;;;;
;; color theme ;;
;;;;;;;;;;;;;;;;;

(setq spacemacs-theme-org-height nil)

(load-theme 'spacemacs-dark t)
(setq frame-background-mode 'dark)

;; (load-theme 'monokai t)
;; (load-theme 'flatui t)

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;

;; always use reindent-then-newline-and-indent

(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

;; handy shortcuts

(global-set-key (kbd "C-c g") 'ag)
(global-set-key (kbd "C-c u") 'ag-dired)

;; we don't use transpose-lines much - transpose-paragraphs is
;; probably more useful, so let's change the binding to that

(global-set-key (kbd "C-x C-t") 'transpose-paragraphs)

;; window navigation

(global-set-key (kbd "s-{") 'shrink-window-horizontally)
(global-set-key (kbd "s-}") 'enlarge-window-horizontally)

;;;;;;;;;;;;;;
;; zoom-frm ;;
;;;;;;;;;;;;;;

(require 'zoom-frm)

(global-set-key (kbd "C-x C-+") 'zoom-in/out)
(global-set-key (kbd "C-x C--") 'zoom-in/out)
(global-set-key (kbd "C-x C-=") 'zoom-in/out)
(global-set-key (kbd "C-x C-0") 'zoom-in/out)

;;;;;;;;;;;;;;;
;; undo-tree ;;
;;;;;;;;;;;;;;;

;; (require 'undo-tree)
;; (global-undo-tree-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
;; persistent-scratch ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(persistent-scratch-setup-default)

;;;;;;;;;;;;;;;;
;; mu (email) ;;
;;;;;;;;;;;;;;;;

;; on OSX, currently using brew --HEAD option.  to update, use
;; (shell-command "brew rm mu && brew install --HEAD --with-emacs mu --ignore-dependencies")

(when (require 'mu4e nil :noerror)
  (require 'mu4e-contrib) ;; for mu4e-shr2text
  (require 'smtpmail)

  (global-set-key (kbd "C-c m") 'mu4e)

  (setq mu4e-user-mail-address-list
        '("ben@benswift.me"
          "ben.swift@anu.edu.au"
          "benjamin.j.swift@gmail.com"
          "ben.swift@simeonnetwork.org"))

  ;; recieve

  (setq mu4e-maildir (expand-file-name "~/Maildir/fastmail")
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

  (global-set-key (kbd "C-x m") #'mu4e-compose-new)

  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-compose-signature-auto-include nil)
  (add-hook 'mu4e-compose-mode-hook #'flyspell-mode 1)
  (require 'company)
  (add-hook 'mu4e-compose-mode-hook #'company-mode-on)

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

  ;; read

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

  (require 'dash)

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

  (add-to-list 'mu4e-view-actions
               '("bView in browser" . mu4e-action-view-in-browser) t)

  ;; send

  (require 'smtpmail-async)

  (setq send-mail-function 'async-smtpmail-send-it
        message-send-mail-function 'async-smtpmail-send-it
        smtpmail-smtp-service 587
        smtpmail-debug-info t
        smtpmail-queue-dir (expand-file-name "~/Maildir/queue/cur"))

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
                  (user-full-name . "Ben Swift")
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
                  (user-full-name . "Ben Swift")
                  (smtpmail-starttls-credentials '(("smtp.office365.com" 587 nil nil)))
                  (smtpmail-smtp-server . "smtp.office365.com")))
         (make-mu4e-context
          :name "simeon-network"
          :enter-func (lambda () (mu4e-message "switching to Simeon Network context"))
          ;; leave-fun not defined
          :match-func (lambda (msg)
                        (when msg
                          (mu4e-message-contact-field-matches msg :to "ben.swift@simeonnetwork.org")))
          :vars '((user-mail-address . "ben.swift@simeonnetwork.org")
                  (user-full-name . "Ben Swift")
                  (smtpmail-starttls-credentials '(("mail.simeonnetwork.org" 587 nil nil)))
                  (smtpmail-smtp-server . "mail.simeonnetwork.org"))))))

;; spamming all the mateys

(defun ben-send-anu-email (email-address subject body &optional blocking)
  (with-temp-buffer
    (mu4e-context-switch nil "anu")
    (insert (format "From: Ben Swift <ben.swift@anu.edu.au>\nTo: %s\nSubject: %s\n--text follows this line--\n%s"
                    email-address subject body))
    (if blocking
        (message-smtpmail-send-it)
      (async-smtpmail-send-it))))

(defun ben-send-benswift-email (email-address subject body &optional blocking)
  (with-temp-buffer
    (mu4e-context-switch nil "personal")
    (insert (format "From: Ben Swift <ben@@benswift.me>\nTo: %s\nSubject: %s\n--text follows this line--\n%s"
                    email-address subject body))
    (if blocking
        (message-smtpmail-send-it)
      (async-smtpmail-send-it))))

;;;;;;;;;;;;;;
;; flyspell ;;
;;;;;;;;;;;;;;

(setq flyspell-auto-correct-binding [(super ?\;)])
(add-hook 'text-mode-hook 'turn-on-flyspell)

;;;;;;;;;;;;;;;;;;;;;
;; smart mode line ;;
;;;;;;;;;;;;;;;;;;;;;

(setq sml/theme 'respectful)
(setq sml/name-width (cons 10 40))
(setq rm-blacklist
      (regexp-opt '("MRev"
                    "Projectile"
                    "Fly"
                    "Ref"
                    "Fill"
                    "company"
                    "yas"
                    "LY"
                    "Undo-Tree"
                    "ivy")))
(sml/setup)

;;;;;;;;;;;;;
;; company ;;
;;;;;;;;;;;;;

(add-hook 'prog-mode-hook #'global-company-mode)

;;;;;;;;;;
;; dash ;;
;;;;;;;;;;

(global-set-key (kbd "C-c d") 'dash-at-point)

(add-to-list 'dash-at-point-mode-alist
             '(cmake-mode . "cmake"))

;;;;;;;;;;;;
;; eshell ;;
;;;;;;;;;;;;

(global-set-key (kbd "C-c e") 'eshell)

(defun ben-eshell-mode-hook ()
  ;; config vars
  (setq eshell-cmpl-cycle-completions nil)
  (setq eshell-save-history-on-exit t)
  (setq eshell-buffer-shorthand t)
  (setq eshell-cmpl-dir-ignore "\\`\\(\\.\\.?\\|CVS\\|\\.svn\\|\\.git\\)/\\'")
  ;; environment vars
  (setenv "EDITOR" "export EDITOR=\"emacsclient --alternate-editor=emacs --no-wait\"")
  ;; keybindings
  (define-key eshell-mode-map (kbd "<C-up>") 'eshell-previous-matching-input-from-input)
  (define-key eshell-mode-map (kbd "<C-down>") 'eshell-next-matching-input-from-input)
  (define-key eshell-mode-map (kbd "<up>") 'previous-line)
  (define-key eshell-mode-map (kbd "<down>") 'next-line)
  ;; prompt helpers
  (setq eshell-directory-name (concat user-emacs-directory "eshell/"))
  (setq eshell-prompt-regexp "^[^@]*@[^ ]* [^ ]* [$#] ")
  (setq eshell-prompt-function
        (lambda ()
          (concat (user-login-name) "@" (host-name) " "
                  (file-name-nondirectory (eshell/pwd))
                  (if (= (user-uid) 0) " # " " $ "))))
  ;; helpful bits and pieces
  (eldoc-mode 1)
  (add-to-list 'eshell-command-completions-alist
               '("gunzip" "gz\\'"))
  (add-to-list 'eshell-command-completions-alist
               '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))
  (add-to-list 'eshell-visual-commands "ssh"))

(add-hook 'eshell-mode-hook 'ben-eshell-mode-hook)

(defun host-name ()
  "Returns the name of the current host minus the domain."
  (let ((hostname (downcase (system-name))))
    (save-match-data
      (substring hostname
                 (string-match "^[^.]+" hostname)
                 (match-end 0)))))

;;;;;;;;;;;
;; elisp ;;
;;;;;;;;;;;

(add-hook 'emacs-lisp-mode-hook 'eldoc-mode 1)

;;;;;;;;;;;
;; dired ;;
;;;;;;;;;;;

(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer t)
(setq dired-dwim-target t)

;;;;;;;;;;;;;;;;
;; type-break ;;
;;;;;;;;;;;;;;;;

;; (type-break-mode 1)

;;;;;;;;;;;;;
;; ibuffer ;;
;;;;;;;;;;;;;

(global-set-key (kbd "C-x C-b") 'ibuffer)

;;;;;;;;;;;
;; magit ;;
;;;;;;;;;;;

(setq vc-display-status nil)
(global-set-key (kbd "C-x g") 'magit-status)
(setq magit-push-always-verify nil)
(setq magit-completing-read-function 'ivy-completing-read)

;; this isn't really about magit, but if you're exclusively using
;; magit then turning off the other vc modes might speed things up a
;; bit
(setq vc-handled-backends nil)

;;;;;;;;;;;;;;
;; org mode ;;
;;;;;;;;;;;;;;

(require 'org)

(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "CANCELED")))

(setq org-directory
      (if (equal system-type 'windows-nt)
          "c:/Users/ben/Dropbox/org/"
        "~/Dropbox/org/"))

(setq org-agenda-files
      (if (equal system-type 'windows-nt)
          '("c:/Users/ben/Dropbox/org/")
        '("~/Dropbox/org/")))

(setq org-capture-templates
      '(("a" "My TODO task format." entry
         (file "todo.org")
         "* TODO %?\n\tSCHEDULED: %t\n")))

(setq org-enforce-todo-dependencies t
      org-log-done '(time))

(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c t a") 'org-agenda-list)

;; org-beamer

(with-eval-after-load "ox-latex"
  (add-to-list 'org-latex-classes
               '("metropolis"
                 "% -*- TeX-engine: xetex; TeX-command-extra-options: \"-shell-escape\"; -*-
\\documentclass{beamer}
\\usetheme{metropolis}
\\setbeamertemplate{itemize items}{$\\bullet$}
[NO-DEFAULT-PACKAGES]
[PACKAGES]
[EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ben is On the Tubes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; need to use the command `org-html-htmlize-generate-css' to extract
;; class definitions
(setq org-html-htmlize-output-type 'inline-css
      org-export-with-toc nil)

;; (setq org-html-htmlize-font-prefix "")

(setq org-html-footnotes-section
      "<div id=\"footnotes\">
<h2>%s </h2>
<div id=\"text-footnotes\">
%s
</div>
</div>")
(setq org-html-footnote-format "[%s] ")
(setq org-html-footnote-separator "")
(setq org-publish-project-alist
      '(("biott-posts"
         :base-directory "~/Documents/biott/"
         :base-extension "org"
         :exclude "draft-posts/*"
         :publishing-directory "~/Code/clojure/benswift.me/resources/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t
         :html-head-include-default-style nil
         :section-numbers nil
         :html-link-home "/"
         :html-link-use-abs-url t)
        ("biott-images"
         :base-directory "~/Documents/biott/public/img/"
         :base-extension "png\\|jpg\\|pdf"
         :publishing-directory "~/Code/clojure/benswift.me/resources/public/img/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("biott" :components ("biott-posts" "biott-images"))))

(setq org-export-html-mathjax-options
      '((path "http://orgmode.org/mathjax/MathJax.js")
        (scale "100")
        (align "center")
        (indent "2em")
        (mathml t)))


(require 'url-util) ; needed for url-unerserved-chars

(defun biott-sanitise-post-name (post-name)
  (apply #'string (reverse (cl-reduce (lambda (processed char)
                                      (if (member char url-unreserved-chars)
                                          (cons char processed)
                                        (if (and processed
                                                 (= (first processed) ?-))
                                            processed
                                          (cons ?- processed))))
                                      (string-to-list post-name)
                    :initial-value '()))))

(defun biott-new-post (post-name)
  (interactive "sPost title: ")
  (let ((post-url-basename
         (concat (format-time-string "%Y-%m-%d-")
                 (downcase (biott-sanitise-post-name post-name)))))
    (find-file (concat "~/Documents/biott/posts/"
                       post-url-basename
                       ".org"))
    (insert (format
             "#+PROPERTY: header-args:extempore :tangle /tmp/%s.xtm
#+begin_html
---
title: %s
alias: [\"./%s.html\"]
tags:
---
#+end_html
"
             post-url-basename post-name post-url-basename))))

(defun biott-push-to-gh-pages ()
  (interactive)
  (let ((default-directory "~/Code/clojure/benswift.me/"))
    (async-shell-command "./gh-pages-deploy.sh")))

;;;;;;;;;;;;;;
;; hunspell ;;
;;;;;;;;;;;;;;

(when (executable-find "hunspell")
  (setq ispell-program-name (executable-find "hunspell"))
  (setq ispell-dictionary "en_AU"))

;;;;;;;;;;;;;;;;
;; projectile ;;
;;;;;;;;;;;;;;;;

(projectile-global-mode)
(setq projectile-completion-system 'ivy)

;;;;;;;;;
;; erc ;;
;;;;;;;;;

(if (load "~/.dotfiles/secrets/ercpass" t)
    (progn
      (erc-services-mode 1)
      (setq erc-nick "benswift")
      (setq erc-prompt-for-password nil)
      (setq erc-prompt-for-nickserv-password nil)
      (setq erc-autojoin-channels-alist '(("freenode.net" "#extempore")))
      (setq erc-notify-list '("digego")))
  (message "Couldn't find the secrets file, you need to pull it down from dropbox."))

;;;;;;;;;;;;;;;;;;;;
;; LaTeX & reftex ;;
;;;;;;;;;;;;;;;;;;;;

(require 'latex)
(require 'reftex)

(setq font-latex-fontify-sectioning 'color)
(setq TeX-master t)
(setq TeX-PDF-mode t)
(setq TeX-auto-untabify t)
(setq TeX-parse-self t)
(setq TeX-auto-save t)

(add-to-list 'auto-mode-alist '("\\.cls" . LaTeX-mode))

;; use Skim for pdfs on OSX
(when (equal system-type 'darwin)
  (add-to-list
   'TeX-view-program-list
   '("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))
  (add-to-list 'TeX-view-program-selection '(output-pdf "Skim")))

;; synctex
(setq TeX-source-correlate-mode t)
(setq TeX-source-correlate-method 'synctex)

;; reftex

(setq reftex-enable-partial-scans t)
(setq reftex-save-parse-info t)
(setq reftex-plug-into-AUCTeX t)
(setq reftex-cite-prompt-optional-args nil)
(setq reftex-cite-cleanup-optional-args t)
(setq reftex-bibliography-commands '("bibliography" "nobibliography" "setupbibtex\\[.*?database=" "addbibresource"))

;; reftex keybindings

(define-key LaTeX-mode-map (kbd "C-c =") 'reftex-toc)
(define-key LaTeX-mode-map (kbd "C-c c") 'reftex-citation)
(define-key LaTeX-mode-map (kbd "C-c r") 'reftex-reference)

(defun latex-word-count ()
  (interactive)
  (let* ((tex-file (if (stringp TeX-master)
                       TeX-master
                     (buffer-file-name)))
         (enc-str (symbol-name buffer-file-coding-system))
         (enc-opt (cond
                   ((string-match "utf-8" enc-str) "-utf8")
                   ((string-match "latin" enc-str) "-latin1")
                   ("-encoding=guess")))
         (word-count
          (with-output-to-string
            (with-current-buffer standard-output
              (call-process "texcount" nil t nil "-1" "-merge" enc-opt tex-file)))))
    (message word-count)))

(define-key LaTeX-mode-map (kbd "C-c w") 'latex-word-count)

;; beamer helpers

(defun latex-minted-bounds ()
  "Determine if point is inside a minted environment, and if true return the bounds of teh minted region"
  (let ((case-fold-search nil))
    (save-excursion
      (re-search-backward "\\\\\\(begin\\|end\\){\\([^}]+\\)}" nil :noerror)
      (let ((minted-begin (match-end 0)))
        (when (and (string= (match-string 1) "begin")
                   (string= (match-string 2) "minted"))
          (search-forward "\\end{minted}" nil :noerror)
          (list minted-begin (match-beginning 0)))))))

(defvar latex-minted-source-markers nil)

(defun latex-minted-jump-to-src-buffer ()
  (interactive)
  (let* ((latex-buffer (current-buffer))
         (bounds (latex-minted-bounds))
         (str (and bounds (apply #'buffer-substring-no-properties bounds))))
    (unless bounds
      (error "Not currently in a {minted} environment"))
    (string-match "^{\\([^}]+\\)}\\(.*\\)" str)
    (let* ((minted-lang (match-string 1 str))
           (minted-begin (+ 1 (match-end 1) (car bounds)))
           (minted-lang-mode (intern (concat minted-lang "-mode")))
           (minted-body (substring str (+ 1 (match-end 2)))))
      (unless (fboundp minted-lang-mode)
        (error "Cannot find major mode for %s" minted-lang))
      (with-current-buffer (get-buffer-create "*minted*")
        (delete-region (point-min) (point-max))
        (funcall minted-lang-mode)
        (insert minted-body)
        (indent-region (point-min) (point-max))
        (whitespace-cleanup)
        (setq latex-minted-source-markers
              (list (set-marker (make-marker) minted-begin latex-buffer)
                    (set-marker (make-marker) (cadr bounds) latex-buffer)))
        (pop-to-buffer (current-buffer))))))

(defun latex-minted-update-buffer ()
  (interactive)
  (when (string= (buffer-name) "*minted*")
    (let ((src (buffer-substring-no-properties (point-min) (point-max)))
          (buffer (marker-buffer (car latex-minted-source-markers))))
      (with-current-buffer buffer
        (save-excursion
          (apply #'delete-region latex-minted-source-markers)
          (insert (concat "\n" src)))
        (delete-window)))))

(defun latex-minted-dwim ()
  (interactive)
  (if (string= (buffer-name) "*minted*")
      (latex-minted-update-buffer)
    (latex-minted-jump-to-src-buffer)))

(with-eval-after-load "latex"
  (define-key LaTeX-mode-map (kbd "C-c C-'") 'latex-minted-dwim))

;; to clean Biber cache, use
;; (shell-command "echo rm -r `biber --cache`")

;; for minted

;; (eval-after-load "tex"
;;   '(setcdr (assoc "LaTeX" TeX-command-list)
;;           '("%`%l%(mode) -shell-escape%' %t"
;;           TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")))

;;;;;;;;;;;;;;;
;; extempore ;;
;;;;;;;;;;;;;;;

;; extempore customisation
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

(add-to-list 'dash-at-point-mode-alist '(extempore-mode . "gl4,gl3,gl2,c,c++,osx"))

(defun ben-extempore-mode-hook ()
  (eldoc-mode 1)
  (setq eldoc-documentation-function
        'extempore-eldoc-documentation-function)
  ;; (if (and (not extempore-logger-mode)
  ;;          (yes-or-no-p "Do you want to log this session?"))
  ;;     (extempore-logger-mode 1))
  (yas-minor-mode-on)
  ;; extempore-mode and lispy interaction
  (setq-local lispy-no-space t)
  ;; monokai-bg is #272822 (monokai 20140109.2253)
  (set-face-attribute 'extempore-blink-face nil :foreground "#272822" :background "#FD971F")
  (set-face-attribute 'extempore-sb-blink-face nil :foreground "#272822" :background "#39FF14"))

(add-hook 'extempore-mode-hook 'ben-extempore-mode-hook)

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

(defun htmlize-and-open-in-browser (beg end)
  (interactive "r")
  (let ((src (buffer-substring beg end))
        (mode major-mode)
        (outfile "/tmp/fontified.html"))
    (with-temp-buffer
      (funcall mode)
      (insert src)
      (htmlfontify-buffer)
      (write-file outfile))
    (async-shell-command (format "open -a /Applications/Safari.app %s" outfile))))

(defun extempore-run-integration-test ()
  (interactive)
  (let ((default-directory (concat extempore-share-directory "extras/")))
    (async-shell-command "./integration-test.sh"
                         (get-buffer-create "*extempore-integration-test*"))))

;; yasnippet helpers


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

;;;;;;;;;;;;
;; OpenCL ;;
;;;;;;;;;;;;

;; hopefully this will be added to MELPA at some stage, then we can
;; just blow it away.

(add-to-list 'load-path (concat user-emacs-directory "opencl-mode-emacs/"))
(require 'opencl-mode nil :noerror)
(add-to-list 'auto-mode-alist '("\\.cl\\'" . opencl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;
;; rainwow-delimiters ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; enable in prog modes only
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;;;;;;;;;;;;
;; clojure ;;
;;;;;;;;;;;;;

;; cider

(with-eval-after-load "clojure"
  (define-key clojure-mode-map (kbd "C-:") nil))

;;;;;;;;;;;;;;
;; markdown ;;
;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

(defun ben-markdown-mode-hook ()
  ;; unset these keys in markdown-mode-map
  (define-key markdown-mode-map (kbd "<M-left>") nil)
  (define-key markdown-mode-map (kbd "<M-right>") nil))

(add-hook 'markdown-mode-hook 'ben-markdown-mode-hook)

;;;;;;;;;;
;; yaml ;;
;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;;;;;;;;
;; git ;;
;;;;;;;;;

(add-to-list 'auto-mode-alist '(".*gitconfig$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '(".*gitignore$" . conf-unix-mode))

;;;;;;;;;;;;;
;; systemd ;;
;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.service$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '("\\.socket$" . conf-unix-mode))

;;;;;;;
;; R ;;
;;;;;;;

(require 'ess-site)

(add-to-list 'auto-mode-alist '("\\.r$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))

;; don't do the smart arrow thing. it's dumb
(setq ess-S-assign "_")

;;;;;;;;;;;
;; julia ;;
;;;;;;;;;;;

(require 'ess-julia)
(setq inferior-julia-program-name "julia")

;;;;;;;;;;;;;;
;; flycheck ;;
;;;;;;;;;;;;;;

;; I reckon this is more trouble than it's worth, usually
;; (add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;;;;;;
;; haskell ;;
;;;;;;;;;;;;;

(add-hook 'haskell-mode-hook 'intero-mode)

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(setq python-indent-offset 4)

(defun pyvenv-create (name)
  "create a virtualenv, just the way Ben likes it"
  (interactive "sname: ")
  (let* ((cmd (format "virtualenv -p python3 --always-copy %s" name))
         (retcode (shell-command cmd)))
    (if (= retcode 0)
        (pyvenv-activate name)
      (error "Error: % failed with code %d" cmd retcode))))

;; elpy setup

(when (require 'elpy nil :noerror)
  (elpy-enable))

;; scons

(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;

(require 'yasnippet)

(setq yas-prompt-functions '(yas-ido-prompt yas-no-prompt))
(setq yas-triggers-in-field t)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;
;; ttl (Turtle) mode ;;
;;;;;;;;;;;;;;;;;;;;;;;

(autoload 'ttl-mode "ttl-mode" "Major mode for OWL or Turtle files" t)
(add-to-list 'auto-mode-alist '("\\.n3" . ttl-mode))
(add-to-list 'auto-mode-alist '("\\.ttl" . ttl-mode))
(add-hook 'ttl-mode-hook 'turn-on-font-lock)

;;;;;;;;;;;;;
;; Nyquist ;;
;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.ny" . lisp-mode))

;;;;;;;;;;;;;;
;; lilypond ;;
;;;;;;;;;;;;;;

(autoload 'LilyPond-mode "lilypond-mode")
(add-to-list 'auto-mode-alist '("\\.ly$" . LilyPond-mode))
(add-hook 'LilyPond-mode-hook 'turn-on-font-lock)

;;;;;;;;;
;; abc ;;
;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.abc\\'"  . abc-mode))
(add-to-list 'auto-mode-alist '("\\.abp\\'"  . abc-mode))

;;;;;;;;;;;;;;;;;;;;;;
;; multiple-cursors ;;
;;;;;;;;;;;;;;;;;;;;;;

(require 'multiple-cursors)

(global-set-key (kbd "<C-S-up>") 'mc/edit-lines)
(global-set-key (kbd "<C-S-down>") 'mc/mark-all-like-this-dwim)
(global-set-key (kbd "<C-S-right>") 'mc/mark-next-like-this)
(global-set-key (kbd "<C-S-left>") 'mc/mark-previous-like-this)

(global-set-key (kbd "C-c n") 'mc/insert-numbers)

;; redefine mc/insert-numbers to do characters as well

(defun mc/insert-numbers (num-or-char)
  "Insert increasing numbers or characters for each cursor, starting at 0 or NUM-OR-CHAR."
  (interactive "cStart from (default 0): ")
  (setq mc--insert-numbers-number (if (= num-or-char (string-to-char (kbd "RET")))
                                      ?0
                                    num-or-char))
  (mc/for-each-cursor-ordered
   (mc/execute-command-for-fake-cursor 'mc--insert-number-and-increase cursor)))

(defvar mc--insert-numbers-number ?0)

(defun mc--insert-number-and-increase ()
  (interactive)
  (insert mc--insert-numbers-number)
  (setq mc--insert-numbers-number (1+ mc--insert-numbers-number)))

;;;;;;;;;;;;;;;;
;; Processing ;;
;;;;;;;;;;;;;;;;

(setq processing-sketchbook-dir "~/Code/processing")
(setq processing-location "/usr/local/bin/processing-java")

;;;;;;;;;;;;;;;;
;; kubernetes ;;
;;;;;;;;;;;;;;;;

(require 's)

(defun minikube-env-export (line)
  (let ((split-string (s-split "=" (s-chop-prefix "export " line))))
    (setenv (car split-string) (read (cadr split-string)))))

(defun minikube-docker-env ()
  (interactive)
  "Parse and set environment variables from 'minikube docker-env' output"
  (--each-while
      (s-lines (shell-command-to-string "minikube docker-env"))
      (s-prefix? "export" it)
    (minikube-env-export it)))


;;;;;;;;;;;;;;;;;;
;; clang-format ;;
;;;;;;;;;;;;;;;;;;

;; we get it through homebrew on OSX

(load "/usr/local/opt/clang-format/share/clang/clang-format.el" :noerror)

(setq clang-format-style "LLVM")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; handy, misc. elisp functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun copy-buffer-file-name-as-kill ()
  (interactive)
  (let ((fname (buffer-file-name)))
    (if fname
        (progn
          (kill-new fname)
          (message "%s" fname))
      (message "current buffer is not visiting any file."))))

(defun read-lines (fpath)
  "Return a list of lines of a file at at FPATH."
  (with-temp-buffer
    (insert-file-contents fpath)
    (split-string (buffer-string) "\n" t)))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; from http://www.emacswiki.org/emacs/CommentingCode
(defun comment-dwim-line (&optional arg)
  "Replacement for the `comment-dwim' command.
If no region is selected and current line is not blank and we are not at the end of the line,
then comment current line.
Replaces default behaviour of `comment-dwim', when it inserts comment at the end of the line."
  (interactive "*P")
  (comment-normalize-vars)
  (if (and (not (region-active-p)) (not (looking-at "[ \t]*$")))
      (comment-or-uncomment-region (line-beginning-position) (line-end-position))
    (comment-dwim arg)))

;; replace binding for `comment-dwim' as well
(global-set-key (kbd "M-;") 'comment-dwim-line)

;; from
;; http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs

(defun duplicate-line ()
  "Clone line at cursor, leaving the latter intact."
  (interactive "*")
  (save-excursion
    ;; The last line of the buffer cannot be killed
    ;; if it is empty. Instead, simply add a new line.
    (if (and (eobp) (bolp))
	(newline)
      ;; Otherwise kill the whole line, and yank it back.
      (let ((kill-read-only-ok t)
	    deactivate-mark)
	(read-only-mode 1)
	(kill-whole-line)
	(read-only-mode 0)
	(yank)))))

;; (global-set-key (kbd "C-c d") 'duplicate-line)
;; (global-set-key (kbd "C-c b") 'comment-box)

;; unstick "stuck" color codes in shell buffer

(defun unstick-ansi-color-codes ()
  (interactive)
  (goto-char (point-max))
  (insert "echo -e \"\033[m\"")
  (comint-send-input nil t))

;; church music helper functions

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

;; lisp debugging

(defun ben-insert-debug-printlns ()
  "put a debugging println before every single function call"
  (interactive)
  (save-excursion
    (let (beg end count)
      (setq beg (point))
      (setq count 0)
      (end-of-defun)
      (setq end (point))
      (goto-char beg)
      (while (search-forward "(" end :noerror)
        (backward-char)
        (lispy-wrap-round)
        (insert (format "begin (println %03d) " count))
        (setq count (1+ count))
        (forward-char)))))

(defun ben-wrap-sexp-in-println-checkpoints ()
  "put a debugging println before every single function call"
  (interactive)
  (save-excursion
    (lispy-wrap-round)
    (insert "begin (println '-------------------checkpoint-BEGIN) ")
    (sp-up-sexp)
    (backward-char)
    (insert "(println '-------------------checkpoint-END)")))

(defun titlecase-word ()
  "Convert word at point (or selected region) to Title Case."
  (interactive)
  (let* ((bounds (if (use-region-p)
                     (cons (region-beginning) (region-end))
                   (bounds-of-thing-at-point 'word)))
         (text   (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (when bounds
      (delete-region (car bounds) (cdr bounds))
      (insert (s-titleize text)))))

(global-set-key (kbd "M-t") 'titlecase-word)

;;;;;;;;;;;;;;;;;;
;; emacs server ;;
;;;;;;;;;;;;;;;;;;

(require 'server)

;; (setq server-name "ben")

(unless (server-running-p)
  (server-start))

;; toggle fullscreen

(if (display-graphic-p)
    (if (fboundp #'toggle-frame-fullscreen)
        (toggle-frame-fullscreen)
      (message "toggle-frame-fullscreen not defined - are you on Emacs 24.4 or greater?")))

;;;;;;;;;;;;;;;;;;;;
;; screen capture ;;
;;;;;;;;;;;;;;;;;;;;

;; OSX-only for now, should add others in future

(defun osx-screencapture (filename)
  (interactive "sfilename: ")
  (shell-command (format "screencapture -i \"%s.png\"" filename)))

;;;;;;;;;
;; pic ;;
;;;;;;;;;

;; ppic2

(defun ppic2-mpirun-remote ()
  (interactive)
  (let ((default-directory "/Users/ben/Code/src/skeleton-pic/"))
    (async-shell-command "mpirun --app ppic2/xtm/ppic2.app"
                         (get-buffer-create "*extempore-remote*"))))

(defun ppic2-run-remote ()
  (interactive)
  (let ((default-directory "/Users/ben/Code/src/skeleton-pic/"))
    (async-shell-command "extempore --port 7097 --run ppic2/xtm/ppic2-run-remote.xtm"
                         (get-buffer-create "*extempore-remote*"))))

;; pbpic2

(defun pbpic2-mpirun-laptop ()
  (interactive)
  (let ((default-directory "/Users/ben/Code/src/skeleton-pic/"))
    (async-shell-command "mpirun --app pbpic2-laptop/xtm/pbpic2.app"
                         (get-buffer-create "*extempore-MPI*"))))

;;; init.el ends here
