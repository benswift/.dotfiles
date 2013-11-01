;;;;;;;;;;;;;;;;;;;;;;;;
;: ben swift's .emacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; dotfiles repo: https://github.com/benswift/.dotfiles

;;;;;;;;;;
;; elpa ;;
;;;;;;;;;;

(require 'package)
(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(unless package--initialized
  (package-initialize))

(when (null package-archive-contents)
  (package-refresh-contents))

(dolist (package
         '(auctex
           auto-complete
           bookmark+
           elpy
           epl
           ess
           flx-ido
           gist
           htmlize
           ido-ubiquitous
           imenu-anywhere
           isearch+
           magit
           markdown-mode
           monokai-theme
           multiple-cursors
           org
           paredit
           scss-mode
           smart-mode-line
           smex
           yaml-mode
           yasnippet))
  (if (not (package-installed-p package))
      (package-install package)))

(global-set-key (kbd "C-c p") 'list-packages)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cross-platform setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(exec-path-from-shell-initialize)

;; linux

(defun ben-linux-setup ()
  (setq base-face-height 140))

;; OSX

(defun spotlight-locate-make-command-line (search-string)
  (list "mdfind" "-interpret" search-string))

(defun ben-setup-osx-keybindings ()
  (setq mac-option-modifier 'meta)
	(setq mac-command-modifier 'super)
  (define-key global-map (kbd "s-a") 'mark-whole-buffer)
  (define-key global-map (kbd "s-j") 'exchange-point-and-mark)
  (define-key global-map (kbd "s-k") 'kill-this-buffer)
  (define-key global-map (kbd "s-l") 'goto-line)
  (define-key global-map (kbd "s-m") 'iconify-frame)
  (define-key global-map (kbd "s-n") 'make-frame)
  (define-key global-map (kbd "s-q") 'save-buffers-kill-emacs)
  (define-key global-map (kbd "s-s") 'save-buffer)
  (define-key global-map (kbd "s-u") 'revert-buffer)
  (define-key global-map (kbd "s-v") 'yank)
  (define-key global-map (kbd "s-c") 'kill-ring-save)
  (define-key global-map (kbd "s-w") 'delete-frame)
  (define-key global-map (kbd "s-x") 'kill-region))

(defun ben-osx-setup ()
  (setq base-face-height 160)
  (setq helm-locate-command "mdfind -name %s %s")
  (setq locate-make-command-line 'spotlight-locate-make-command-line)
  (setq x-bitmap-file-path '("/usr/X11/include/X11/bitmaps"))
  (setq source-directory "/Library/Caches/Homebrew/emacs--git")
  (setq dired-guess-shell-alist-user '(("\\.pdf\\'" "open")))
  ;; for railwaycat emacs-mac
  (ben-setup-osx-keybindings))

;; Windows

(defun ben-windows-setup ()
  (setq base-face-height 160)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super))

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

;; show time and battery status in mode line

(display-time-mode 1)
(setq display-time-format "%H:%M")
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
(add-hook 'text-mode-hook 'turn-on-flyspell)
(remove-hook 'text-mode-hook 'smart-spacing-mode)

;; file visiting stuff

(setq save-place t)
(setq save-place-file (concat user-emacs-directory "places"))
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq recentf-max-saved-items 100)

(global-auto-revert-mode t)

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

;; fullscreen

(defun toggle-frame-fullscreen ()
  "Toggle fullscreen mode of the selected frame.
Enable fullscreen mode of the selected frame or disable if it is
already fullscreen.  Ignore window manager screen decorations.
When turning on fullscreen mode, remember the previous value of the
maximization state in the temporary frame parameter `maximized'.
Restore the maximization state when turning off fullscreen mode.
See also `toggle-frame-maximized'."
  (interactive)
  (modify-frame-parameters
   nil
   `((maximized
      . ,(unless (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
	   (frame-parameter nil 'fullscreen)))
     (fullscreen
      . ,(if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
	     (if (eq (frame-parameter nil 'maximized) 'maximized)
		 'maximized)
	   'fullscreen)))))

(define-key global-map (kbd "<f11>") 'toggle-frame-fullscreen)

;; pretty lambdas

(add-hook 'prog-mode-hook
          '(lambda ()
             (font-lock-add-keywords
              nil `(("(?\\(lambda\\>\\)"
                     (0 (progn (compose-region (match-beginning 1) (match-end 1)
                                               ,(make-char 'greek-iso8859-7 107))
                               nil)))))))

;;;;;;;;;;;;;;;;;;;
;; monokai theme ;;
;;;;;;;;;;;;;;;;;;;

(if (display-graphic-p)
    (progn (load-theme 'monokai t)
           (add-to-list 'default-frame-alist
                        '(background-mode . dark))
           (set-cursor-color "white")))

;; this is fragile, but necessary since they took this macro out in a
;; recent change

;; TODO look into the `monokai-add-font-lock-keywords' variable and
;; see if it's a better way to do this.

(defmacro monokai-with-color-variables (&rest body)
  "`let' bind all colors defined in `monokai-colors-alist'.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (car cons) (cdr cons)))
                   (if window-system
                       (cdar monokai-colors)
                     (cdadr monokai-colors))))
     ,@body))

;;;;;;;;;;;
;; faces ;;
;;;;;;;;;;;

(set-face-attribute 'default nil :height base-face-height :family "Source Code Pro")
(set-face-attribute 'variable-pitch nil :height base-face-height :family "Fira Sans OT")
;; (set-face-attribute 'variable-pitch nil :height base-face-height :family "Ubuntu")

;; get colours right in terminal (and related) modes

(defun ben-set-monokai-term-colors ()
  (monokai-with-color-variables
    (set-face-attribute 'term-color-black nil :background nil :foreground monokai-fg-1)
    (set-face-attribute 'term-color-blue nil :background nil :foreground monokai-blue)
    (set-face-attribute 'term-color-cyan nil :background nil :foreground monokai-cyan)
    (set-face-attribute 'term-color-green nil :background nil :foreground monokai-green)
    (set-face-attribute 'term-color-magenta nil :background nil :foreground monokai-purple)
    (set-face-attribute 'term-color-red nil :background nil :foreground monokai-magenta)
    (set-face-attribute 'term-color-white nil :background nil :foreground monokai-fg)
    (set-face-attribute 'term-color-yellow nil :background nil :foreground monokai-yellow)))

(if (display-graphic-p)
    (add-hook 'term-hook 'ben-set-monokai-term-colors))

(require 'ansi-color)

(if (display-graphic-p)
    (monokai-with-color-variables
      (setq ansi-color-names-vector (vector monokai-fg-1
                                            monokai-purple
                                            monokai-green
                                            monokai-yellow
                                            monokai-blue
                                            monokai-magenta
                                            monokai-green
                                            monokai-fg)
            ansi-color-map (ansi-color-make-color-map))))

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;

;; handy shortcuts

(global-set-key (kbd "<f5>") 'magit-status)
(global-set-key (kbd "<f6>") 'compile)
(global-set-key (kbd "C-x g") 'find-grep)
(global-set-key (kbd "C-x u") 'find-dired)

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

;;;;;;;;;;;;;;;;;;;;;
;; smart mode line ;;
;;;;;;;;;;;;;;;;;;;;;

;; only works in a graphical frame

(if (display-graphic-p)
    (progn
      (require 'smart-mode-line)

      (setq sml/name-width 30)
      (setq sml/mode-width 'full)
      (setq sml/shorten-modes t)

      (add-to-list 'sml/hidden-modes " ElDoc")
      (add-to-list 'sml/hidden-modes " Paredit")
      (add-to-list 'sml/hidden-modes " AC")
      (add-to-list 'sml/hidden-modes " yas")

      ;; directory shorteners
      (add-to-list 'sml/replacer-regexp-list '("^~/Code/extempore/" ":extempore:"))
      (add-to-list 'sml/replacer-regexp-list '("^~/Code/xtm/" ":xtm:"))
      (add-to-list 'sml/replacer-regexp-list '("^~/Documents/School/postdoc/papers/" ":papers:"))

      ;; monokai-ize the smart-mode-line
      (monokai-with-color-variables
        ;; modeline foreground/background
        (setq sml/active-foreground-color monokai-fg)
        (setq sml/active-background-color monokai-bg+1)
        (setq sml/inactive-foreground-color monokai-fg-1)
        (setq sml/inactive-background-color monokai-bg-1)
        (set-face-attribute 'sml/global nil :foreground monokai-fg)
        ;; other faces
        (set-face-attribute 'sml/time nil :foreground monokai-fg :weight 'bold)
        (set-face-attribute 'sml/filename nil :foreground monokai-yellow)
        (set-face-attribute 'sml/prefix nil :foreground monokai-green))

      (add-hook 'after-init-hook 'sml/setup)))

;;;;;;;;;;;;
;; eshell ;;
;;;;;;;;;;;;

(setq eshell-aliases-file "~/.dotfiles/eshell-alias")
(global-set-key (kbd "C-c s") 'eshell)

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
  ;;faces
  (monokai-with-color-variables
    (set-face-attribute 'eshell-prompt nil :foreground monokai-orange))
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

;;;;;;;;;;;
;; magit ;;
;;;;;;;;;;;

(setq magit-save-some-buffers nil)

;;;;;;;;;;;;;
;; cc-mode ;;
;;;;;;;;;;;;;

;; (setq c-default-style "k&r")

;;;;;;;;;;;;;
;; ebrowse ;;
;;;;;;;;;;;;;

(defun ben-ebrowse-set-faces ()
  (set-face-attribute 'ebrowse-root-class nil :foreground nil :inherit font-lock-type-face)
  (set-face-attribute 'ebrowse-member-class nil :foreground nil :inherit font-lock-function-name-face)
  (set-face-attribute 'ebrowse-member-attribute nil :foreground nil :inherit font-lock-string-face))

(add-hook 'ebrowse-tree-mode 'ben-ebrowse-set-faces)

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

;; (setq org-format-latex-header
;;       "\\documentclass[12pt,a4paper]{scrartcl}
;; \\usepackage{libertineotf}
;; \\usepackage{fontspec}
;; \\setmonofont[Scale=MatchLowercase,Mapping=tex-text]{Source Code Pro}

;; \\usepackage{booktabs}
;; \\usepackage{tabularx}
;; \\renewcommand{\\arraystretch}{1.2}

;; % biblatex

;; \\usepackage[%
;; backend=biber,
;; natbib=true,
;; backref=true,
;; citecounter=true,
;; dashed=false,
;; backrefstyle=three,
;; citestyle=authoryear-icomp,
;; firstinits=true,
;; maxcitenames=2,
;; maxbibnames=10,
;; uniquename=mininit,
;; bibstyle=authoryear,
;; % refsegment=chapter,
;; % ibidtracker=strict,
;; url=false,
;; doi=false]{biblatex}

;; % to use year-only bib format
;; \\AtEveryBibitem{\\clearfield{month}}
;; \\AtEveryCitekey{\\clearfield{month}}

;; % specify the bib file here
;; \\addbibresource{papers.bib}

;; % IMPORTANT: to actually print the bibliography in the document,
;; % insert the command: \\printbibliography[title=References]

;; % csquotes

;; \\usepackage[english=british,threshold=15,thresholdtype=words]{csquotes}
;; \\SetCiteCommand{\\parencite}

;; \\newenvironment*{smallquote}
;;   {\\quote\\small}
;;   {\\endquote}
;; \\SetBlockEnvironment{smallquote}

;; % hyperref & bookmark

;; \\usepackage[svgnames,hyperref]{xcolor}

;; \\usepackage[%
;; unicode=true,
;; hyperindex=true,
;; bookmarks=true,
;; colorlinks=true, % change to false for final
;; pdfborder=0,
;; allcolors=DarkBlue,
;; % plainpages=false,
;; pdfpagelabels,
;; hyperfootnotes=true]{hyperref}

;; ")

;;;;;;;;;;;;;;;
;; octopress ;;
;;;;;;;;;;;;;;;

(setq org-publish-project-alist
      '(("biott-posts"
         ;; Path to your org files.
         :base-directory "~/Documents/biott/org/"
         :base-extension "org"
         :exclude "drafts/*"
         ;; Path to your Jekyll project.
         :publishing-directory "~/Code/octopress/source/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t)
        ("biott-images"
         :base-directory "~/Documents/biott/images/"
         :base-extension "png\\|jpg\\|pdf"
         :publishing-directory "/Users/ben/Code/octopress/source/images/"
         :recursive t
         :publishing-function org-publish-attachment)
        ("biott" :components ("biott-posts" "biott-images"))))

(setq org-export-html-mathjax-options
      '((path "http://orgmode.org/mathjax/MathJax.js")
        (scale "100")
        (align "center")
        (indent "2em")
        (mathml t)))

(defun biott-new-post (post-name)
  (interactive "sPost title: ")
  (find-file (concat "~/Documents/biott/org/_posts/drafts/"
                     (format-time-string "%Y-%m-%d-")
                     (downcase (subst-char-in-string 32 45 post-name))
                     ".org"))
  (insert (concat
           "#+begin_html
---
layout: post
title: \"" post-name "\"
date: " (format-time-string "%Y-%m-%d %R") "
comments: true
categories:
---
#+end_html
")))

;;;;;;;;;
;; erc ;;
;;;;;;;;;

(erc-services-mode 1)
(setq erc-nick "benswift")
(load "~/.dotfiles/secrets/ercpass")
(setq erc-prompt-for-password nil)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-autojoin-channels-alist '(("freenode.net" "#extempore")))

(defun ben-erc-set-faces ()
  (monokai-with-color-variables
    (set-face-attribute 'erc-input-face nil :foreground monokai-yellow)))

(add-hook 'erc-mode-hook 'ben-erc-set-faces)

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
               '("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b"))
  (if (string= system-type "darwin")
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

;;;;;;;;;;;;;;;
;; extempore ;;
;;;;;;;;;;;;;;;

(setq extempore-path "~/Code/extempore/")
(autoload 'extempore-mode (concat extempore-path "extras/extempore.el") "" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))

;; extempore customisation
(setq extempore-tab-completion nil)

;; device-specific Extempore config
(cond
 ((string= system-name "lonyx")
  (setq extempore-program-args "--device 1 --run libs/xtm.xtm"))
 ((string= system-name "cyril.local")
  (setq extempore-program-args "--device 2 --run libs/xtm.xtm"))
 (t nil))

(defun ben-extempore-mode-hook ()
  (turn-on-eldoc-mode)
  (setq eldoc-documentation-function
        'extempore-eldoc-documentation-function)
  (yas-minor-mode-on)
  ;; (if (and (not extempore-logger-mode)
  ;;          (yes-or-no-p "Do you want to log this session?"))
  ;;     (extempore-logger-mode 1))
  (monokai-with-color-variables
    (set-face-attribute 'extempore-blink-face nil :foreground monokai-bg :background "#FD971F")
    (set-face-attribute 'extempore-sb-blink-face nil :foreground monokai-bg :background "#39FF14")))

(add-hook 'extempore-mode-hook 'ben-extempore-mode-hook)

(autoload #'llvm-mode (concat extempore-path "extras/llvm-mode.el")
  "Major mode for editing LLVM IR files" t)

(add-to-list 'auto-mode-alist '("\\.ir$" . llvm-mode))
(add-to-list 'auto-mode-alist '("\\.ll$" . llvm-mode))

;; session setup

(defun ben-create-extempore-template-file (base-path filename &optional header)
  (unless (file-exists-p (concat base-path "/" filename))
    (progn
      (find-file (concat base-path "/" filename))
      (if header (insert header))
      (save-buffer)
      (kill-buffer))))

(defun ben-create-extempore-template-dir (name)
  "Set up the directory structure and files for a new extempore session/gig."
  (interactive "sSession name: ")
  (let* ((xtm-dir "~/Code/xtm")
         (base-path (concat xtm-dir "/sessions/" name))
         (setup-header
          (concat ";;; setup.xtm --- setup file for " name "\n"
                  "(sys:load \"libs/xtm.xtm\")\n"
                  "(ipc:load \"" xtm-dir "/lib/ben-lib.xtm\")\n"
                  "(ipc:load \"utility\" \"" xtm-dir "/lib/ben-lib.xtm\")\n"
                  "(ipc:load \"" xtm-dir "/lib/sampler-maps.xtm\")\n"
                  "(ipc:load \"utility\" \"" xtm-dir "/lib/sampler-maps.xtm\")\n")))
    (if (file-exists-p base-path)
        (error "Cannot create xtm session: directory already exists."))
    (make-directory base-path)
    ;; practice files
    (ben-create-extempore-template-file
     base-path "practice-scm.xtm" "scmhead")
    (ben-create-extempore-template-file
     base-path "practice-xtlang.xtm" "xthead")
    ;; gig files
    (ben-create-extempore-template-file
     base-path "gig-scm.xtm" "scmhead")
    (ben-create-extempore-template-file
     base-path "gig-xtlang.xtm" "xthead")
    ;; setup file
    (ben-create-extempore-template-file
     base-path "setup.xtm" setup-header)
    (dired base-path)))

;;;;;;;;;;;;;
;; paredit ;;
;;;;;;;;;;;;;

;; from https://gist.github.com/malk/4962126

(defun point-is-inside-list ()
  "Whether point is currently inside list or not."
  (nth 1 (syntax-ppss)))

(defun point-is-inside-string ()
  "Whether point is currently inside string or not."
  (nth 3 (syntax-ppss)))

(defun point-is-inside-comment ()
  "Whether point is currently inside a comment or not."
  (nth 4 (syntax-ppss)))

(defun paredit--is-at-opening-paren ()
  (and (looking-at "\\s(")
       (not (point-is-inside-string))
       (not (point-is-inside-comment))))

(defun paredit-skip-to-start-of-sexp-at-point ()
  "Skips to start of current sexp."
  (interactive)
  (while (not (paredit--is-at-opening-paren))
    (if (point-is-inside-string)
        (paredit-backward-up)
      (paredit-backward))))

(defun paredit-duplicate-rest-of-closest-sexp ()
  (interactive)
  (cond
   ((paredit--is-at-opening-paren)
    (paredit-copy-sexps-as-kill)
    (forward-sexp)
    (paredit-newline)
    (yank)
    (exchange-point-and-mark))
   ((point-is-inside-list)
    (while (looking-at " ") (forward-char))
    (if (not (= (point) (car (bounds-of-thing-at-point 'sexp))))
        (progn (forward-sexp)
               (while (looking-at " ") (forward-char))))
    (let ((sexp-inside-end (- (paredit-next-up/down-point 1 1) 1)))
      (kill-ring-save (point) sexp-inside-end)
      (goto-char sexp-inside-end))
    (paredit-newline)
    (yank)
    (exchange-point-and-mark))))

(defface paredit-paren-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face for parentheses.  Taken from ESK.")

(defun ben-paredit-mode-hook ()
  (define-key paredit-mode-map (kbd "<M-delete>") 'paredit-forward-kill-word)
  (define-key paredit-mode-map (kbd "<M-backspace>") 'paredit-backward-kill-word)
  (define-key paredit-mode-map (kbd "<s-left>") 'paredit-backward-up)
  (define-key paredit-mode-map (kbd "<s-S-left>") 'paredit-backward-down)
  (define-key paredit-mode-map (kbd "<s-right>") 'paredit-forward-up)
  (define-key paredit-mode-map (kbd "<s-S-right>") 'paredit-forward-down)
  (define-key paredit-mode-map (kbd "<M-S-up>") 'paredit-raise-sexp)
  (define-key paredit-mode-map (kbd "<M-S-down>") 'paredit-wrap-sexp)
  (define-key paredit-mode-map (kbd "<M-S-left>") 'paredit-convolute-sexp)
  (define-key paredit-mode-map (kbd "<M-S-right>") 'transpose-sexps)
  (define-key paredit-mode-map (kbd "<s-S-down>") 'paredit-duplicate-rest-of-closest-sexp))

(add-hook 'paredit-mode-hook 'ben-paredit-mode-hook)

;; turn on paredit by default in all 'lispy' modes

(dolist (mode '(scheme emacs-lisp lisp clojure clojurescript extempore))
  (when (> (display-color-cells) 8)
    (font-lock-add-keywords (intern (concat (symbol-name mode) "-mode"))
                            '(("(\\|)" . 'paredit-paren-face))))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            'paredit-mode))

;; taken from
;; http://emacsredux.com/blog/2013/04/18/evaluate-emacs-lisp-in-the-minibuffer/

(defun conditionally-enable-paredit-mode ()
  "Enable `paredit-mode' in the minibuffer, during `eval-expression'."
  (if (eq this-command 'eval-expression)
      (paredit-mode 1)))

(add-hook 'minibuffer-setup-hook 'conditionally-enable-paredit-mode)

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

;;;;;;;
;; R ;;
;;;;;;;

(require 'ess-site)

(add-to-list 'auto-mode-alist '("\\.r$" . R-mode))
(add-to-list 'auto-mode-alist '("\\.R$" . R-mode))

;;;;;;;;;;;;
;; Python ;;
;;;;;;;;;;;;

;; jedi setup

(add-hook 'python-mode-hook 'jedi:setup)

;; elpy setup

;; (elpy-enable)
;; (setq elpy-rpc-backend 'jedi)
;; (setq python-indent-offset 2)

;; (setq  elpy-default-minor-modes
;;        '(eldoc-mode
;;          flymake-mode
;;          ;; highlight-indentation-mode
;;          auto-complete-mode))

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;

(require 'yasnippet)

(setq yas-prompt-functions '(yas-ido-prompt yas-no-prompt))

(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")
(yas-global-mode 1)

;;;;;;;;;;;;;;;;;;
;; autocomplete ;;
;;;;;;;;;;;;;;;;;;

;; autocomplete needs to be set up after yasnippet

(require 'auto-complete-config)

;; (ac-set-trigger-key "<tab>")
(add-to-list 'ac-dictionary-directories (concat user-emacs-directory ".ac-dict"))
(setq ac-auto-start 2)
(ac-config-default)

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

;;;;;;;;;;
;; misc ;;
;;;;;;;;;;

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

(global-set-key (kbd "C-c d") 'duplicate-line)
(global-set-key (kbd "C-c b") 'comment-box)

;;;;;;;;;;;;;;;;;;
;; emacs server ;;
;;;;;;;;;;;;;;;;;;

(require 'server)

;; (setq server-name "ben")

(unless (server-running-p)
  (server-start))

;; toggle fullscreen

(if (and (display-graphic-p) (fboundp 'toggle-frame-fullscreen))
    (toggle-frame-fullscreen))
