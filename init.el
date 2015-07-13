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
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))

(unless package--initialized
  (package-initialize))

(when (null package-archive-contents)
  (package-refresh-contents))

(setq ben-package-list
      '(ace-jump-mode
        ag
        async
        bbdb
        auctex
        auto-complete
        cider
        cmake-mode
        clojure-snippets
        company
        dash-at-point
        dockerfile-mode
        xcscope
        elpy
        erc-hl-nicks
        erc-terminal-notifier
        ess
        exec-path-from-shell
        fasm-mode
        flatui-theme
        flx-ido
        flycheck
        ggtags
        gist
        glsl-mode
        htmlize
        ido-ubiquitous
        imenu-anywhere
        isearch+
        less-css-mode
        jedi
        magit
        markdown-mode
        malinka
        monokai-theme
        multiple-cursors
        ;; nrepl-ritz ;; shadows cider-mode
        osc
        org
        paradox
        persistent-scratch
        projectile
        rainbow-delimiters
        rtags
        sane-term
        s
        scss-mode
        smartparens
        smart-mode-line
        smex
        string-utils
        unidecode
        vagrant
        vagrant-tramp
        vimrc-mode
        wgrep
        wgrep-ag
        workgroups2
        yaml-mode
        yasnippet
        zoom-frm))

(dolist (package ben-package-list)
  (if (not (package-installed-p package))
      (package-install package)))

;; paradox (souped up package.el)

(setq paradox-execute-asynchronously t)

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
  (exec-path-from-shell-copy-envs '("EDITOR" "EXT_LLVM_DIR" "LD_LIBRARY_PATH" "PYTHONPATH")))

;; OSX

(defun spotlight-locate-make-command-line (search-string)
  (list "mdfind" "-interpret" search-string))

(defun ben-setup-keybindings ()
  (define-key global-map (kbd "s-a") 'mark-whole-buffer)
  (define-key global-map (kbd "s-k") 'kill-this-buffer)
  (define-key global-map (kbd "s-l") 'goto-line)
  (define-key global-map (kbd "s-n") 'make-frame)
  (define-key global-map (kbd "s-q") 'save-buffers-kill-emacs)
  (define-key global-map (kbd "s-s") 'save-buffer)
  (define-key global-map (kbd "s-u") 'revert-buffer)
  (define-key global-map (kbd "s-v") 'yank)
  (define-key global-map (kbd "s-c") 'kill-ring-save)
  (define-key global-map (kbd "s-w") 'delete-frame)
  (define-key global-map (kbd "s-x") 'kill-region))

(defun ben-osx-setup ()
  (ben-set-default-faces 140)
  (setq mac-option-modifier 'meta)
	(setq mac-command-modifier 'super)
  (setq helm-locate-command "mdfind -name %s %s")
  (setq locate-make-command-line 'spotlight-locate-make-command-line)
  (setq x-bitmap-file-path '("/usr/X11/include/X11/bitmaps"))
  (setq source-directory "/Library/Caches/Homebrew/emacs-mac--git")
  (setq dired-guess-shell-alist-user '(("\\.pdf\\'" "open")))
  (setq frame-maximization-mode 'fullscreen)
  ;; for railwaycat emacs-mac
  (ben-setup-keybindings)
  ;; exec-path-from-shell is set up on linux as well
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs '("EDITOR" "EXT_LLVM_DIR" "LD_LIBRARY_PATH" "PYTHONPATH")))

;; Windows

(defun ben-windows-setup ()
  (ben-set-default-faces 160)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super)
  (ben-setup-keybindings))

(cond ((equal system-type 'gnu/linux) (ben-linux-setup))
      ((equal system-type 'darwin) (ben-osx-setup))
      ((equal system-type 'windows-nt) (ben-windows-setup)))

;;;;;;;;;;;;;;;;;;;
;; customisation ;;
;;;;;;;;;;;;;;;;;;;

(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file t)

;;;;;;;;;;;;;;;;
;; smex & ido ;;
;;;;;;;;;;;;;;;;

(require 'flx-ido)

(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)

(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

(ido-mode 1)
(ido-ubiquitous-mode 1)
(flx-ido-mode 1)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "C-c i") 'imenu-anywhere)
(global-set-key (kbd "C-c o") 'occur)

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

(display-time-mode 1)
(setq display-time-format "%H:%M")
(display-battery-mode -1)

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
(add-hook 'text-mode-hook 'turn-on-flyspell)
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
(setq ispell-dictionary "en_GB")

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(defalias 'yes-or-no-p 'y-or-n-p)

;; transparency

(defun set-current-frame-alpha (value)
  (interactive
   (list (read-number "Frame alpha: " 1.0)))
   (set-frame-parameter (selected-frame) 'alpha value))

(global-set-key (kbd "C-c t") 'set-current-frame-alpha)

(define-key global-map (kbd "C-s-f") 'toggle-frame-fullscreen)

;; hide certain minor modes from mode line

(setq eldoc-minor-mode-string nil)
(setq eldoc-argument-case 'downcase)

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

(let ((theme 'monokai))
  (if (display-graphic-p)
      (case theme
        ('monokai (load-theme 'monokai t)
                  (add-to-list 'default-frame-alist
                               '(background-mode . dark))
                  (set-cursor-color "white"))
        ;; flatui
        ('flatui  (load-theme 'flatui t)
                  (add-to-list 'default-frame-alist
                               '(background-mode . light))))))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;

;; always use reindent-then-newline-and-indent

(global-set-key (kbd "RET") 'reindent-then-newline-and-indent)

;; handy shortcuts

(global-set-key (kbd "C-c g") 'ag)
(global-set-key (kbd "C-c u") 'ag-dired)

;; window navigation

(global-set-key (kbd "s-[") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "s-]") 'other-window)

(global-set-key (kbd "s-{") 'shrink-window-horizontally)
(global-set-key (kbd "s-}") 'enlarge-window-horizontally)

;; Mac OS X-like

(global-set-key (kbd "s-z") 'undo)

(global-set-key (kbd "<s-left>") 'move-beginning-of-line)
(global-set-key (kbd "<s-right>") 'move-end-of-line)
(global-set-key (kbd "<s-up>") 'beginning-of-buffer)
(global-set-key (kbd "<s-down>") 'end-of-buffer)
(global-set-key (kbd "<M-delete>") 'kill-word)
(global-set-key (kbd "<M-backspace>") 'backward-kill-word)
(global-set-key (kbd "<s-backspace>") (lambda () (interactive) (kill-visual-line 0)))

;;;;;;;;;;;;;;
;; zoom-frm ;;
;;;;;;;;;;;;;;

(require 'zoom-frm)

(global-set-key (kbd "C-x C-+") 'zoom-in/out)
(global-set-key (kbd "C-x C--") 'zoom-in/out)
(global-set-key (kbd "C-x C-=") 'zoom-in/out)
(global-set-key (kbd "C-x C-0") 'zoom-in/out)

;;;;;;;;;;;;;;;;;
;; workgroups2 ;;
;;;;;;;;;;;;;;;;;

;; (workgroups-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;
;; persistent-scratch ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(persistent-scratch-setup-default)

;;;;;;;;;;;;;;;;
;; mu (email) ;;
;;;;;;;;;;;;;;;;

;; on OSX, currently using brew --HEAD option.  to update, use
;; (shell-command "brew rm mu && brew install --HEAD --with-emacs mu --ignore-dependencies")

;; only set this up when mu4e is installed
(if (require 'mu4e nil :noerror)
    (progn
      (require 'smtpmail)

      ;; smtp
      (setq message-send-mail-function 'smtpmail-send-it
            smtpmail-starttls-credentials
            '(("mail.messagingengine.com" 587 nil nil))
            smtpmail-default-smtp-server "mail.messagingengine.com"
            smtpmail-smtp-server "mail.messagingengine.com"
            smtpmail-smtp-service 587
            smtpmail-debug-info t)

      (global-set-key (kbd "C-c m") 'mu4e)

      (setq mu4e-maildir (expand-file-name "~/Maildir/fastmail"))
      (setq smtpmail-queue-dir (expand-file-name "~/Desktop/queued-mail"))

      (setq mu4e-sent-folder   "/Sent Items")
      (setq mu4e-refile-folder "/Archive")
      (setq mu4e-drafts-folder "/Drafts")
      (setq mu4e-trash-folder  "/Trash")

      (setq mu4e-attachment-dir  "~/Downloads")

      ;; get mail
      (setq mu4e-get-mail-command "timelimit -t 240 -T 270 mbsync fastmail"
            mu4e-update-interval 300
            mu4e-headers-auto-update t
            mu4e-compose-signature-auto-include nil
            mu4e-change-filenames-when-moving t
            mu4e-view-show-addresses t)

      ;; html email handling
      (require 'mu4e-contrib)
      (setq mu4e-html2text-command #'mu4e-shr2text)
      ;; make sure fg-bg contrast is high enough
      (setq shr-color-visible-luminance-min 80)

      (setq mu4e-maildir-shortcuts
            '(("/INBOX"      . ?i)
              ("/Sent Items" . ?s)
              ("/Archive"    . ?a)
              ("/Drafts"     . ?d)
              ("/Trash"      . ?t)
              ("/Spam"       . ?j)))

      ;; headers view
      (setq mu4e-headers-date-format "%e %b %y"
            mu4e-headers-fields '((:human-date . 12)
                                  (:flags . 6)
                                  (:maildir . 10)
                                  (:from . 22)
                                  (:subject)))

      ;; bookmarks
      (add-to-list 'mu4e-bookmarks
                   '("list:extemporelang.googlegroups.com" "Extempore list" ?e) t)
      (add-to-list 'mu4e-bookmarks
                   '("from:Henry Gardner" "Hballs" ?h) t)
      (add-to-list 'mu4e-bookmarks
                   '("to:benjamin.j.swift@gmail.com" "to gmail" ?g) t)

      ;; actions
      (add-to-list 'mu4e-view-actions
                   '("bView in browser" . mu4e-action-view-in-browser) t)

      ;; fancy graphics
      (setq mu4e-show-images t
            mu4e-use-fancy-chars nil)

      ;; use imagemagick, if available
      (when (fboundp 'imagemagick-register-types)
        (imagemagick-register-types))

      ;; general emacs mail settings; used when composing e-mail
      ;; the non-mu4e-* stuff is inherited from emacs/message-mode
      (setq mu4e-reply-to-address "ben@benswift.me"
            user-mail-address     "ben@benswift.me"
            user-full-name        "Ben Swift")
      (setq mu4e-user-mail-address-list
            '("ben@benswift.me"
              "benjamin.j.swift@gmail.com"
              "ben.swift@anu.edu.au"))
      (setq mu4e-compose-dont-reply-to-self t)

      ;; mailing lists
      (setq mu4e-user-mailing-lists
            '(("extemporelang.googlegroups.com" . "Extempore")
              ("livecode.group.lurk.org"        . "TOPLAP")
              ("acma-l.list.waikato.ac.nz"      . "ACMA")))

      (defun ben-mu4e-compose-insert-template ()
        (let ((msg mu4e-compose-parent-message)
              (bomp (+ (progn (goto-char (point-min))
                              (search-forward mail-header-separator))
                       1)))
          (goto-char bomp)
          (if msg ;; reply or forward (use "(string= user-mail-address (cdar (mu4e-msg-field msg :from)))" to test for forward)
              (progn
                (insert
                 (cond
                  ((mu4e-message-contact-field-matches
                    msg :from "\\(joyli90@gmail.com\\|joy.y.swift@gmail.com\\|joy.swift@abs.gov.au\\)")
                   "Hi Bunny\n\n\n\nLove,\nBun\n")
                  ((mu4e-message-contact-field-matches
                    msg :from "walknuts@gmail.com")
                   "Hi Dad\n\n\n\nLove,\nBen\n")
                  (t (format "Hi %s\n\n\n\nCheers,\nBen\n"
                             (car (split-string (or (caar (mu4e-msg-field msg :from)) "mate")))))))
                (if (mu4e-message-contact-field-matches
                     msg :to "benjamin.j.swift@gmail.com")
                    (insert "\nP.S. I'm getting rid of this gmail address soon, my new address is ben@benswift.me\n"))
                (goto-char bomp)
                (forward-line 2))
            (progn ;; compose new
              (goto-char bomp)
              (insert "Hi mate\n\n\n\nCheers,\nBen\n")
              (goto-char (point-min))
              (forward-line)
              (move-end-of-line 1)))))

      (defun ben-asciify-buffer-or-region (beg end)
        (interactive "r")
        (let ((asciify-alist '(("’" . "'")
                               ("‘" . "'")
                               ("“" . "\"")
                               ("”" . "\""))))
          (unless (region-active-p)
            (setq beg (point-min))
            (setq end (point-max)))
          (save-excursion
            (-each asciify-alist
              (lambda (nonascii-char-pair)
                (goto-char beg)
                (while (search-forward (car nonascii-char-pair) end :noerror)
                  (replace-match (cdr nonascii-char-pair) nil :literal)))))))

      ;; spell check
      (add-hook 'mu4e-compose-mode-hook
                (defun ben-mu4e-compose-mode-hook ()
                  "My settings for message composition."
                  (ben-asciify-buffer-or-region (point-min) (point-max))
                  (flyspell-mode 1)
                  (ben-mu4e-compose-insert-template)))

      ;; for using dired to specify attachments
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

      )) ;; end "(if (require 'mu4e nil :noerror) ..."

;;;;;;;;;;;;;;;;;;;;;
;; smart mode line ;;
;;;;;;;;;;;;;;;;;;;;;

(sml/setup)

(setq sml/name-width (cons 10 40))

(setq rm-blacklist
      (regexp-opt '("SP/s"
                    "MRev"
                    "Projectile"
                    "Fly"
                    "Ref"
                    "Fill")))

;;;;;;;;;;;;;;;;;;;
;; ace-jump-mode ;;
;;;;;;;;;;;;;;;;;;;

(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(autoload
  'ace-jump-mode-pop-mark
  "ace-jump-mode"
  "Ace jump back:-)"
  t)
(eval-after-load "ace-jump-mode"
  '(ace-jump-mode-enable-mark-sync))
(define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)

;;;;;;;;;;;;;
;; company ;;
;;;;;;;;;;;;;

(add-hook 'after-init-hook 'global-company-mode)

;;;;;;;;;;;
;; rtags ;;
;;;;;;;;;;;

;; (rtags-enable-standard-keybindings)

;;;;;;;;;;;;;
;; malinka ;;
;;;;;;;;;;;;;

;; (require 'malinka)

;; (setq malinka-completion-system 'ido)

;; (add-hook 'c-mode-common-hook 'malinka-mode)

;; (malinka-define-project
;;  :name "Extempore"
;;  :root-directory "~/Code/extempore"
;;  :build-directory "~/Code/extempore"
;;  ;; :cpp-defines '("TARGET_OS_MAC" "USE_GLUT")
;;  :compiler-flags '("-Wall" "-g")
;;  :configure-cmd ""
;;  :compile-cmd "./all.bash"
;;  :test-cmd "./test-all.sh")

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
                  (base-name (eshell/pwd))
                  (if (= (user-uid) 0) " # " " $ "))))
  ;; helpful bits and pieces
  (turn-on-eldoc-mode)
  (add-to-list 'eshell-command-completions-alist
               '("gunzip" "gz\\'"))
  (add-to-list 'eshell-command-completions-alist
               '("tar" "\\(\\.tar|\\.tgz\\|\\.tar\\.gz\\)\\'"))
  (add-to-list 'eshell-visual-commands "ssh"))

(add-hook 'eshell-mode-hook 'ben-eshell-mode-hook)

(defun base-name (path)
  "Returns the base name of the given path."
  (let ((path (abbreviate-file-name path)))
    (if (string-match "\\(.*\\)/\\(.*\\)$" path)
        (if (= 0 (length (match-string 1 path)))
            (concat "/" (match-string 2 path))
          (match-string 2 path))
      path)))

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

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

;;;;;;;;;;;
;; dired ;;
;;;;;;;;;;;

(setq dired-listing-switches "-alh")
(setq dired-auto-revert-buffer t)

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
(global-set-key (kbd "<f5>") 'magit-status)

;;;;;;;;;;;;;
;; cc-mode ;;
;;;;;;;;;;;;;

;; (setq c-default-style "k&r")

;;;;;;;;;;;;;;
;; org mode ;;
;;;;;;;;;;;;;;

(require 'org)

(setq org-completion-use-ido t)
(setq org-export-with-toc nil)

(defun ben-org-mode-hook ()
  ;; ;; org-latex export
  ;; (add-to-list 'org-export-latex-classes
  ;;              '("scrartcl"
  ;;                "\\documentclass[12pt,a4paper]{scrartcl}"
  ;;                ("\\section{%s}" . "\\section*{%s}")
  ;;                ("\\subsection{%s}" . "\\subsection*{%s}")
  ;;                ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ;;                ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ;;                ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
  ;; (setq org-export-latex-default-class "scrartcl")
  ;; keymappings
  (define-key org-mode-map (kbd "<M-left>") 'backward-word)
  (define-key org-mode-map (kbd "<M-right>") 'forward-word)
  (define-key org-mode-map (kbd "<C-left>") 'org-metaleft)
  (define-key org-mode-map (kbd "<C-right>") 'org-metaright)
  ;; stop org-mode shadowing the mc keybindings
  (define-key org-mode-map (kbd "<C-S-up>") nil)
  (define-key org-mode-map (kbd "<C-S-down>") nil)
  (define-key org-mode-map (kbd "<C-S-right>") nil)
  (define-key org-mode-map (kbd "<C-S-left>") nil))

(add-hook 'org-mode-hook 'ben-org-mode-hook)

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ben is On the Tubes ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;; need to use the command `org-html-htmlize-generate-css' to extract
;; class definitions
(setq org-html-htmlize-output-type 'inline-css)

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

;; (require 'ispell)

;; (setq ispell-program-name "/usr/local/bin/hunspell")
;; (setq ispell-dictionary "en_AU")
;; (add-to-list 'ispell-dictionary-alist
;;              '("en_AU" "[[:alpha:]]" "[^[:alpha:]]" "" t ("-d" "/Library/Spelling/en_AU") nil iso-8859-1))

;; (setq rw-hunspell-dicpath-list '("/Library/Spelling"))

;;;;;;;;;;;;;;;;
;; projectile ;;
;;;;;;;;;;;;;;;;

(projectile-global-mode)

(global-set-key (kbd "<f6>") 'projectile-compile-project)


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

(defun ben-latex-mode-hook ()
  (setq TeX-master t)
  (setq TeX-PDF-mode t)
  (setq TeX-auto-untabify t)
  (setq TeX-parse-self t)
  (setq TeX-auto-save t)
  (add-to-list 'auto-mode-alist '("\\.cls" . LaTeX-mode))
  ;; use Skim for pdfs on OSX
  (add-to-list 'TeX-view-program-list
               '("Skim" "~/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))
  (if (equal system-type 'darwin)
      (add-to-list 'TeX-view-program-selection '(output-pdf "Skim")))
  ;; synctex
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-method 'synctex)
  ;; latex keybindings
  (define-key LaTeX-mode-map (kbd "C-c w") 'latex-word-count)
  ;; reftex
  (setq reftex-enable-partial-scans t)
  (setq reftex-save-parse-info t)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-cite-prompt-optional-args nil)
  (setq reftex-cite-cleanup-optional-args t)
  (setq reftex-bibliography-commands '("bibliography" "nobibliography" "setupbibtex\\[.*?database=" "addbibresource"))
  (reftex-mode 1)
  ;; reftex keybindings
  (define-key LaTeX-mode-map (kbd "C-c =") 'reftex-toc)
  (define-key LaTeX-mode-map (kbd "C-c c") 'reftex-citation)
  (define-key LaTeX-mode-map (kbd "C-c r") 'reftex-reference))

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

(add-hook 'LaTeX-mode-hook 'ben-latex-mode-hook)

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
 ((string= system-name "lonyx")
  (setq extempore-program-args "--device 0 --frames 1024")
  (setq user-extempore-directory "/home/ben/Code/extempore/")
  (setq user-extempore-lib-directory "/home/ben/Code/xtm/lib/"))
 ((string= system-name "debian-vm")
  (setq extempore-program-args "--device 1 --frames 1024")
  (setq user-extempore-directory "/home/ben/Code/extempore/")
  (setq user-extempore-lib-directory "/home/ben/Code/xtm/lib/"))
 ((or (string= system-name "hodgey.local")
      (string= system-name "hodgey.lan")
      t) ;; probably running on hodgey
  (setq extempore-program-args nil)
  ;; (setq extempore-program-args "--device 1 --frames 1024")
  (setq user-extempore-directory "/Users/ben/Code/extempore/")
  (setq user-extempore-lib-directory "/Users/ben/Code/xtm/lib/")))

(autoload 'extempore-mode (concat user-extempore-directory "extras/extempore.el") nil t)
(autoload 'extempore-run (concat user-extempore-directory "extras/extempore.el") nil t)
(autoload 'extempore-repl (concat user-extempore-directory "extras/extempore.el") nil t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))
(add-to-list 'dash-at-point-mode-alist '(extempore-mode . "gl4,gl3,gl2,c,c++,osx"))

(defun ben-extempore-mode-hook ()
  (turn-on-eldoc-mode)
  (setq eldoc-documentation-function
        'extempore-eldoc-documentation-function)
  ;; (if (and (not extempore-logger-mode)
  ;;          (yes-or-no-p "Do you want to log this session?"))
  ;;     (extempore-logger-mode 1))
  (yas-minor-mode-on)
  ;; monokai-bg is #272822 (monokai 20140109.2253)
  (set-face-attribute 'extempore-blink-face nil :foreground "#272822" :background "#FD971F")
  (set-face-attribute 'extempore-sb-blink-face nil :foreground "#272822" :background "#39FF14"))

(add-hook 'extempore-mode-hook 'ben-extempore-mode-hook)

;; more extempore-related goodies

(autoload #'llvm-mode (concat user-extempore-directory "extras/llvm-mode.el")
  "Major mode for editing LLVM IR files" t)

;; to pull down the lldb-aware gud.el
;; (async-shell-command (format "curl -o %sextras/gud-lldb.el http://www.opensource.apple.com/source/lldb/lldb-69/utils/emacs/gud.el?txt" user-extempore-directory))

(autoload #'lldb (concat user-extempore-directory "extras/gud-lldb.el")
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

;; (let ((extempore-snippet-dir
;;        "/Users/ben/.dotfiles/snippets/extempore-mode"))
;;   (dolist (name extempore-yas-oscillator-list)
;;     (with-temp-buffer
;;       (insert (format "# -*- mode: snippet -*-
;; # name: %s
;; # key: %screate
;; # --
;; " name name))
;;       (insert (format "(%s_mc_c ${1:nchan} ${2:})" name))
;;       (write-region (point-min)
;;                     (point-max)
;;                     (format "%s/%s-create"
;;                             extempore-snippet-dir
;;                             name)))
;;     (with-temp-buffer
;;       (insert (format "# -*- mode: snippet -*-
;; # name: %s
;; # key: %scall
;; # --
;; " name name))
;;       (insert (format "(%s ${1:chan} ${2:})" name))
;;       (write-region (point-min)
;;                     (point-max)
;;                     (format "%s/%s-call"
;;                             extempore-snippet-dir
;;                             name)))))

;; (let* ((snippet-dir "/Users/ben/.dotfiles/snippets/extempore-mode/")
;;        (snippets (directory-files snippet-dir)))
;;   (dolist (filename snippets)
;;     (if (string-match "\\(-create\\)" filename)
;;         (with-current-buffer (find-file (format "%s%s"
;;                                                 snippet-dir
;;                                                 filename))
;;           (while (re-search-forward "# name: .*$" nil t)
;;             (replace-match (concat (match-string 0) "-create") t nil))
;;           (save-buffer)))))

;;;;;;;;;;;;
;; OpenCL ;;
;;;;;;;;;;;;

;; hopefully this will be added to MELPA at some stage, then we can
;; just blow it away.

(add-to-list 'load-path (concat user-emacs-directory "opencl-mode-emacs/"))
(require 'opencl-mode nil :noerror)
(add-to-list 'auto-mode-alist '("\\.cl\\'" . opencl-mode))

;;;;;;;;;;;;;;;;;
;; smartparens ;;
;;;;;;;;;;;;;;;;;

(require 'smartparens)
(require 'smartparens-config)

(add-to-list 'sp-ignore-modes-list 'org-mode)
(add-to-list 'sp-ignore-modes-list 'shell-mode)

(setq sp-highlight-wrap-tag-overlay nil
      sp-highlight-wrap-overlay nil
      sp-highlight-pair-overlay nil
      sp-hybrid-kill-excessive-whitespace t)

(defun ben-smartparens-mode-hook ()
  (define-key sp-keymap (kbd "M-<down>") 'sp-splice-sexp-killing-forward)
  (define-key sp-keymap (kbd "M-<up>") 'sp-splice-sexp-killing-backward)
  (define-key sp-keymap (kbd "s-<left>") 'sp-backward-up-sexp)
  (define-key sp-keymap (kbd "s-<right>") 'sp-up-sexp)
  (define-key sp-keymap (kbd "s-S-<left>") 'sp-backward-down-sexp)
  (define-key sp-keymap (kbd "s-S-<right>") 'sp-down-sexp)
  (define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
  (define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
  (define-key sp-keymap (kbd "M-S-<up>") 'sp-splice-sexp-killing-around)
  (define-key sp-keymap (kbd "M-S-<left>") 'sp-convolute-sexp)
  (define-key sp-keymap (kbd "M-S-<right>") 'sp-transpose-sexp)
  (define-key sp-keymap (kbd "s-S-<down>") 'sp-duplicate-next-sexp)
  (define-key sp-keymap (kbd "M-S-<down>") 'sp-wrap-with-paren)
  (add-to-list 'sp--lisp-modes 'extempore-mode))

(add-hook 'smartparens-enabled-hook 'ben-smartparens-mode-hook)

(defun sp-wrap-with-paren (&optional arg)
  (interactive "p")
  (sp-select-next-thing-exchange arg)
  (execute-kbd-macro (kbd "(")))

(defun sp-duplicate-next-sexp (&optional arg)
  (interactive "p")
  (sp-select-next-thing arg)
  (kill-ring-save (mark) (point))
  (reindent-then-newline-and-indent)
  (yank)
  (sp-backward-sexp))

(defun sp-reindent-defun (&optional argument)
  "Reindent the definition that the point is on.

If the point is in a string or a comment, fill the paragraph
instead, and with a prefix argument, justify as well."
  (interactive "P")
  (if (or (sp-point-in-string)
          (sp-point-in-comment))
      (lisp-fill-paragraph argument)
    (save-excursion
      (end-of-defun)
      (beginning-of-defun)
      (indent-sexp))))

(define-key lisp-mode-shared-map (kbd "M-q") 'sp-reindent-defun)

(smartparens-global-mode 1)
(smartparens-global-strict-mode 1)

;; make the quote (') character work right in lisp modes
;; taken from https://github.com/Fuco1/smartparens/issues/286
(defun sp--org-skip-markup (ms mb me)
  (save-excursion
    (and (progn
           (goto-char mb)
           (save-match-data (looking-back "\\sw\\|\\s_\\|\\s.")))
         (progn
           (goto-char me)
           (save-match-data (looking-at "\\sw\\|\\s_\\|\\s."))))))

(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))
  (sp-local-pair "`" nil
                 :skip-match (lambda (ms mb me)
                               (cond
                                ((equal ms "'")
                                 (or (sp--org-skip-markup ms mb me)
                                     (not (sp-point-in-string-or-comment))))
                                (t (not (sp-point-in-string-or-comment)))))))

;;;;;;;;;;;;;;;;;;;;;;;;
;; rainwow-delimiters ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; enable in prog modes only
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;;;;;;;;;;;;;
;; clojure ;;
;;;;;;;;;;;;;

;; cider

(setq nrepl-hide-special-buffers t)
(setq cider-auto-select-error-buffer nil)
(setq cider-repl-use-pretty-printing nil)
(setq cider-repl-use-clojure-font-lock t)

;; for debugging
(setq nrepl-log-messages t)

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

(defun ben-ess-R-post-run-hook ()
  (smartparens-mode t)
  (set (make-local-variable 'sp-hybrid-kill-excessive-whitespace) nil))

(add-hook 'ess-R-post-run-hook 'ben-ess-R-post-run-hook)

;;;;;;;;;;;
;; julia ;;
;;;;;;;;;;;

(require 'ess-julia)
(setq inferior-julia-program-name "julia")

;;;;;;;;;;;;;;
;; flycheck ;;
;;;;;;;;;;;;;;

(setq flycheck-completion-system 'ido)

(add-hook 'after-init-hook #'global-flycheck-mode)

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

(setq python-indent-offset 2)

;; elpy setup

(elpy-enable)
(elpy-use-ipython)

;; (setq elpy-rpc-backend "jedi")

(setq  elpy-default-minor-modes
       '(eldoc-mode
         flycheck-mode
         highlight-indentation-mode
         auto-complete-mode))

;; scons

(add-to-list 'auto-mode-alist '("SConstruct" . python-mode))

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;

(require 'yasnippet)

(setq yas-prompt-functions '(yas-ido-prompt yas-no-prompt))
(setq yas-triggers-in-field t)
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;
;; autocomplete ;;
;;;;;;;;;;;;;;;;;;

;; autocomplete needs to be set up after yasnippet

;; (require 'auto-complete-config)

;; ;; (ac-set-trigger-key "<tab>")
;; (add-to-list 'ac-dictionary-directories (concat user-emacs-directory ".ac-dict"))
;; (setq ac-auto-start 2)
;; (ac-config-default)

;; for using Clang autocomplete

;; (require 'auto-complete-clang)

;; (defun my-ac-cc-mode-setup ()
;;   (setq ac-sources (append '(ac-source-clang ac-source-yasnippet ac-source-gtags) ac-sources)))

;; (defun my-ac-config ()
;;   (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
;;   (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
;;   (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
;;   (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
;;   (add-hook 'css-mode-hook 'ac-css-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (global-auto-complete-mode t))

;; (my-ac-config)

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

(global-set-key (kbd "s-'") 'comment-dwim-line)

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
  (end-of-buffer)
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
         (charts (loop repeat num-songs collect (ido-completing-read "chart: " candidates nil :require-match))))
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
        (sp-wrap-with-paren)
        (insert (format "begin (println %03d) " count))
        (setq count (1+ count))
        (forward-char)))))

(defun ben-wrap-sexp-in-println-checkpoints ()
  "put a debugging println before every single function call"
  (interactive)
  (save-excursion
    (sp-wrap-with-paren)
    (insert "begin (println '-------------------checkpoint-BEGIN) ")
    (sp-up-sexp)
    (backward-char)
    (insert "(println '-------------------checkpoint-END)")))

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

;;; init.el ends here
