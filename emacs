;;;;;;;;;;;;;;;;;;;;;;;;
;: ben swift's .emacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; dotfiles repo: https://github.com/benswift/.dotfiles

;;;;;;;;;;;
;; cedet ;;
;;;;;;;;;;;

;; uses Emacs-supplied CEDET (for Emacs >= 24)

;; (setq semantic-imenu-bucketize-file nil)
(setq semantic-default-submodes
      '(global-semanticdb-minor-mode             ;; Maintain tag database.
        global-semantic-idle-scheduler-mode      ;; Reparse buffer when idle.
        global-semantic-idle-summary-mode        ;; Show summary of tag at point.
        ;; global-semantic-idle-completions-mode    ;; Show completions when idle.
        global-semantic-decoration-mode          ;; Additional tag decorations.
        ;; global-semantic-highlight-func-mode      ;; Highlight the current tag.
        global-semantic-stickyfunc-mode          ;; Show current fun in header line.
        ;; global-semantic-mru-bookmark-mode        ;; Provide `switch-to-buffer'-like keybinding for tag names.
        ))

(defun ben-c-mode-common-hook ()
  ;; turn on semantic mode
  (semantic-mode 1)
  ;; set up some keybindings
  (local-set-key (kbd "C-<return>") 'semantic-ia-complete-symbol)
  (local-set-key (kbd "C-c ?") 'semantic-ia-complete-symbol-menu)
  (local-set-key (kbd "C-c >") 'semantic-complete-analyze-inline)
  (local-set-key (kbd "C-c p") 'semantic-analyze-proto-impl-toggle)
  (local-set-key (kbd "C-c j") 'semantic-ia-fast-jump)
  (local-set-key "." 'semantic-complete-self-insert)
  (local-set-key ">" 'semantic-complete-self-insert))

(add-hook 'c-mode-common-hook 'ben-c-mode-common-hook)

;; Enable EDE (Project Management) features
;; (global-ede-mode 1)

;; (ede-cpp-root-project "Extempore"
;;                       :name "Extempore"
;;                       :file "~/Code/extempore/README.md"
;;                       :web-site-url "http://extempore.moso.com.au"
;;                       :spp-table '(("TARGET_OS_MAC" . "")))

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

(dolist (p '(auctex
             epl
             ess
             gist
             helm
             htmlize
             ;; elpy
             magit
             markdown-mode
             monokai-theme
             org
             paredit
             scss-mode
             yaml-mode
             yasnippet
             smart-mode-line
             multiple-cursors
             auto-complete))
  (if (not (package-installed-p p))
      (package-install p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cross-platform setup ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq ben-home-dir (getenv "HOME"))
(setq source-directory (concat ben-home-dir "/Code/emacs-24.3"))

(setq ben-path
      (list (concat ben-home-dir  "/.rbenv/shims")
            (concat ben-home-dir  "/bin")
            "/usr/local/bin" "/usr/bin" "/bin"
            "/usr/local/sbin" "/usr/sbin" "/sbin"
            "/usr/X11/bin" "/usr/texbin"))

(defun nix-specific-setup ()
  (setenv "PATH" (mapconcat 'identity ben-path ":"))
  (setq exec-path ben-path))

(defun linux-specific-setup ()
  (setq base-face-height 140)
  (nix-specific-setup))

(defun osx-specific-setup ()
  (setq base-face-height 160)
  (setq browse-default-macosx-browser "/Applications/Safari.app")
  (add-to-list 'ben-path "/Applications/Emacs.app/Contents/MacOS/bin")
  (nix-specific-setup))

(defun windows-specific-setup ()
  (setq base-face-height 160)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super))

(cond ((string-equal system-type "gnu/linux") (linux-specific-setup))
      ((string-equal system-type "darwin") (osx-specific-setup))
      ((string-equal system-type "windows-nt") (windows-specific-setup))
      (t (message "Unknown operating system")))

;;;;;;;;;;;;;;;;;;;
;; customisation ;;
;;;;;;;;;;;;;;;;;;;

(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-readable-p custom-file)
    (load custom-file))

;;;;;;;;;;
;; helm ;;
;;;;;;;;;;

(helm-mode 1)

(global-set-key (kbd "C-c h") 'helm-mini)
(global-set-key (kbd "C-c <SPC>") 'helm-all-mark-rings)
(global-set-key (kbd "C-c i") 'helm-imenu)

;;;;;;;;;;;;;;;;
;; one-liners ;;
;;;;;;;;;;;;;;;;

(column-number-mode 1)
(global-auto-revert-mode t)
;; (setq display-buffer-alist nil)
(remove-hook 'text-mode-hook 'smart-spacing-mode)
(setq bidi-display-reordering nil)
(setq ispell-dictionary "en_GB")
(setq recentf-max-saved-items 100)
(put 'narrow-to-region 'disabled nil)

(display-time)
(setq display-time-format "%H:%M")

;; mark these commands as 'safe'

(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;;;;;;;;;;;;;
;; from ESK ;;
;;;;;;;;;;;;;;

(setq save-place t)
(hl-line-mode t)

(setq visible-bell t
      inhibit-startup-message t
      color-theme-is-global t
      sentence-end-double-space nil
      shift-select-mode nil
      mouse-yank-at-point t
      uniquify-buffer-name-style 'forward
      whitespace-style '(face trailing lines-tail tabs)
      whitespace-line-column 80
      ediff-window-setup-function 'ediff-setup-windows-plain
      save-place-file (concat user-emacs-directory "places")
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      diff-switches "-u")

(add-to-list 'safe-local-variable-values '(lexical-binding . t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

;; Set this to whatever browser you use
;; (setq browse-url-browser-function 'browse-url-firefox)
;; (setq browse-url-browser-function 'browse-default-macosx-browser)
;; (setq browse-url-browser-function 'browse-default-windows-browser)
;; (setq browse-url-browser-function 'browse-default-kde)
;; (setq browse-url-browser-function 'browse-default-epiphany)
;; (setq browse-url-browser-function 'browse-default-w3m)
;; (setq browse-url-browser-function 'browse-url-generic
;;       browse-url-generic-program "~/src/conkeror/conkeror")

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(set-default 'indent-tabs-mode nil)
(set-default 'tab-width 2)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'text-mode-hook 'turn-on-flyspell)

(define-key lisp-mode-shared-map (kbd "RET") 'reindent-then-newline-and-indent)

(defalias 'yes-or-no-p 'y-or-n-p)

;;;;;;;;;;;;;;;;
;; appearance ;;
;;;;;;;;;;;;;;;;

(if (display-graphic-p)
    (progn (load-theme 'monokai t)
           (add-to-list 'default-frame-alist
                        '(background-mode . dark))
           (set-cursor-color "white")))

(dolist (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

;; transparency

(defun set-current-frame-transparency (value)
   (interactive "nTransparency Value 0 (transparent) - 100 (opaque): ")
   (set-frame-parameter (selected-frame) 'alpha value))

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
(set-face-attribute 'variable-pitch nil :height base-face-height :family "Ubuntu")

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

(global-set-key (kbd "s-[")
                (lambda ()
                  (interactive)
                  (other-window -1)))

(global-set-key (kbd "s-]")
                (lambda ()
                  (interactive)
                  (other-window 1)))

(global-set-key (kbd "s-{") 'shrink-window-horizontally)
(global-set-key (kbd "s-}") 'enlarge-window-horizontally)

;; Mac OS X-like

(global-set-key (kbd "s-z") 'undo)

(global-set-key (kbd "<s-left>")
                (lambda ()
                  (interactive)
                  (move-beginning-of-line 1)))

(global-set-key (kbd "<s-right>")
                (lambda ()
                  (interactive)
                  (move-end-of-line 1)))

(global-set-key (kbd "<s-up>")
                (lambda ()
                  (interactive)
                  (goto-char (point-min))))

(global-set-key (kbd "<s-down>")
                (lambda ()
                  (interactive)
                  (goto-char (point-max))))

(global-set-key (kbd "<M-kp-delete>")
                (lambda ()
                  (interactive)
                  (kill-word 1)))

(global-set-key (kbd "<M-backspace>")
                (lambda ()
                  (interactive)
                  (backward-kill-word 1)))

(global-set-key (kbd "<s-backspace>")
                (lambda ()
                  (interactive)
                  (kill-visual-line 0)))

(global-set-key (kbd "<s-kp-delete>")
                (lambda ()
                  (interactive)
                  (kill-visual-line)))

(global-set-key (kbd "<A-backspace>")
                (lambda ()
                  (interactive)
                  (kill-visual-line 0)))

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

(defun ben-org-mode-hook ()
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

;; org-latex export

(setq org-format-latex-header
      "\\documentclass[12pt,a4paper]{scrartcl}
\\usepackage{libertineotf}
\\usepackage{fontspec}
\\setmonofont[Scale=MatchLowercase,Mapping=tex-text]{Source Code Pro}

\\usepackage{booktabs}
\\usepackage{tabularx}
\\renewcommand{\\arraystretch}{1.2}

% biblatex

\\usepackage[%
backend=biber,
natbib=true,
backref=true,
citecounter=true,
dashed=false,
backrefstyle=three,
citestyle=authoryear-icomp,
firstinits=true,
maxcitenames=2,
maxbibnames=10,
uniquename=mininit,
bibstyle=authoryear,
% refsegment=chapter,
% ibidtracker=strict,
url=false,
doi=false]{biblatex}

% to use year-only bib format
\\AtEveryBibitem{\\clearfield{month}}
\\AtEveryCitekey{\\clearfield{month}}

% specify the bib file here
\\addbibresource{papers.bib}

% IMPORTANT: to actually print the bibliography in the document,
% insert the command: \\printbibliography[title=References]

% csquotes

\\usepackage[english=british,threshold=15,thresholdtype=words]{csquotes}
\\SetCiteCommand{\\parencite}

\\newenvironment*{smallquote}
  {\\quote\\small}
  {\\endquote}
\\SetBlockEnvironment{smallquote}

% hyperref & bookmark

\\usepackage[svgnames,hyperref]{xcolor}

\\usepackage[%
unicode=true,
hyperindex=true,
bookmarks=true,
colorlinks=true, % change to false for final
pdfborder=0,
allcolors=DarkBlue,
% plainpages=false,
pdfpagelabels,
hyperfootnotes=true]{hyperref}

")

;;;;;;;;;;;;;;
;; blogging ;;
;;;;;;;;;;;;;;

(setq org-publish-project-alist
      '(("biott-posts"
         ;; Path to your org files.
         :base-directory "~/Documents/biott/org/"
         :base-extension "org"
         :exclude "drafts/*"
         ;; Path to your Jekyll project.
         :publishing-directory "~/Code/octopress/source/"
         :recursive t
         :publishing-function org-publish-org-to-html
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

;;;;;;;;;;;
;; LaTeX ;;
;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.cls" . LaTeX-mode))

(defun ben-latex-mode-hook ()
  ;; basic config vars
  (setq TeX-master 't
        TeX-engine 'xetex
        TeX-PDF-mode t
        TeX-auto-untabify t
        TeX-parse-self t
        TeX-auto-save t)
  ;; Skim
  ;; (setq TeX-view-program-selection '((output-pdf "Skim"))
  ;;       TeX-view-program-list
  ;;       '(("Skim" "/Applications/Skim.app/Contents/SharedSupport/displayline -b -g %n %o %b")))
  ;; Biber
  (add-to-list 'TeX-command-list
               '("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber"))
  (add-to-list 'TeX-command-list
               '("Glossary" "makeglossaries %s" TeX-run-command nil t :help "Create glossaries"))
  ;; ispell
  (setq ispell-tex-skip-alists
        '((;; in their own commands:
           ("\\\\addcontentsline"                          ispell-tex-arg-end 2)
           ("\\\\add\\(tocontents\\|vspace\\)"             ispell-tex-arg-end)
           ("\\\\\\([aA]lph\\|arabic\\)"                   ispell-tex-arg-end)
           ("\\\\author"                                   ispell-tex-arg-end)
           ("\\\\gls\\(pl\\|reset\\)?"                     ispell-tex-arg-end)
           ("\\\\newacronym"                               ispell-tex-arg-end 2)
           ("\\\\\\(re\\)?newcommand"                      ispell-tex-arg-end 0)
           ("\\\\\\(full\\|text\\|paren\\)cite\\*?"        ispell-tex-arg-end)
           ("\\\\cite\\(t\\|p\\|year\\|yearpar\\|title\\|author\\)" ispell-tex-arg-end)
           ("\\\\bibliographystyle"                        ispell-tex-arg-end)
           ("\\\\\\(block\\|text\\)cquote"                 ispell-tex-arg-end 1)
           ("\\\\c?ref"                                    ispell-tex-arg-end)
           ("\\\\makebox"                                  ispell-tex-arg-end 0)
           ("\\\\e?psfig"                                  ispell-tex-arg-end)
           ("\\\\document\\(class\\|style\\)" .
            "\\\\begin[ \t\n]*{[ \t\n]*document[ \t\n]*}"))
          (;; delimited with \begin
           ("\\(figure\\|table\\)\\*?"                     ispell-tex-arg-end 0)
           ("tabular"                                      ispell-tex-arg-end 1)
           ("tabularx"                                     ispell-tex-arg-end 2)
           ("list"                                         ispell-tex-arg-end 2)
           ("program"             . "\\\\end[ \t\n]*{[ \t\n]*program[ \t\n]*}")
           ("verbatim\\*?"        . "\\\\end[ \t\n]*{[ \t\n]*verbatim\\*?[ \t\n]*}")))))

(defun ben-latex-keybindings ()
  (define-key LaTeX-mode-map (kbd "C-c t") 'switch-to-toc-other-frame)
  (define-key LaTeX-mode-map (kbd "C-c w") 'latex-word-count))

(defun ben-reftex-setup ()
  (turn-on-reftex)
  (setq reftex-default-bibliography '("papers.bib"))
  (setq reftex-enable-partial-scans t)
  (setq reftex-save-parse-info t)
  (setq reftex-use-multiple-selection-buffers t)
  (setq reftex-plug-into-AUCTeX t)
  (setq reftex-cite-prompt-optional-args nil)
  (setq reftex-cite-cleanup-optional-args t)
  ;; RefTeX formats for biblatex (not natbib)
  (setq reftex-cite-format
        '((?\C-m . "\\cite[]{%l}")
          (?t . "\\textcite{%l}")
          (?a . "\\autocite[]{%l}")
          (?p . "\\parencite{%l}")
          (?f . "\\footcite[][]{%l}")
          (?F . "\\fullcite[]{%l}")
          (?x . "[]{%l}")
          (?X . "{%l}")))
  (setq font-latex-match-reference-keywords
        '(("cite" "[{")
          ("cites" "[{}]")
          ("footcite" "[{")
          ("footcites" "[{")
          ("parencite" "[{")
          ("textcite" "[{")
          ("fullcite" "[{")
          ("citetitle" "[{")
          ("citetitles" "[{")
          ("headlessfullcite" "[{"))))

(defun latex-word-count ()
  (interactive)
  (let*
      ((tex-file (if (stringp TeX-master)
                     TeX-master
                   (buffer-file-name)))
       (enc-str (symbol-name buffer-file-coding-system))
       (enc-opt
        (cond
         ((string-match "utf-8" enc-str) "-utf8")
         ((string-match "latin" enc-str) "-latin1")
         ("-encoding=guess")))
       (word-count
        (with-output-to-string
          (with-current-buffer standard-output
            (call-process "texcount" nil t nil "-1" "-merge" enc-opt tex-file)))))
    (message word-count)))

(defun switch-to-toc-other-frame ()
  (interactive)
  (if (not (get-buffer "*toc*"))
      (progn (reftex-toc)
             (delete-window)))
  (switch-to-buffer-other-window "*toc*"))

(add-hook 'LaTeX-mode-hook 'ben-latex-mode-hook)
(add-hook 'LaTeX-mode-hook 'ben-latex-keybindings)
(add-hook 'LaTeX-mode-hook 'ben-reftex-setup)

;; Biber under AUCTeX
(defun TeX-run-Biber (name command file)
  "Create a process for NAME using COMMAND to format FILE with Biber."
  (let ((process (TeX-run-command name command file)))
    (setq TeX-sentinel-function 'TeX-Biber-sentinel)
    (if TeX-process-asynchronous
        process
      (TeX-synchronous-sentinel name file process))))

(defun TeX-Biber-sentinel (process name)
  "Cleanup TeX output buffer after running Biber."
  (goto-char (point-max))
  (cond
   ;; Check whether Biber reports any warnings or errors.
   ((re-search-backward (concat
                         "^(There \\(?:was\\|were\\) \\([0-9]+\\) "
                         "\\(warnings?\\|error messages?\\))") nil t)
    ;; Tell the user their number so that she sees whether the
    ;; situation is getting better or worse.
    (message (concat "Biber finished with %s %s. "
                     "Type `%s' to display output.")
             (match-string 1) (match-string 2)
             (substitute-command-keys
              "\\\\[TeX-recenter-output-buffer]")))
   (t
    (message (concat "Biber finished successfully. "
                     "Run LaTeX again to get citations right."))))
  (setq TeX-command-next TeX-command-default))

;;;;;;;;;;;;;;;
;; extempore ;;
;;;;;;;;;;;;;;;

(setq extempore-path (concat ben-home-dir "/Code/extempore/"))
(autoload 'extempore-mode (concat extempore-path "extras/extempore.el") "" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))
(add-to-list 'auto-mode-alist '("\\.xtmh$" . extempore-mode))

;; extempore customisation
(setq extempore-tab-completion nil
      extempore-process-args "--run libs/xtm.xtm")

(setq extempore-default-device-number
      (cond
       ((string= system-name "lonyx") 1)
       ((string= system-name "cyril.local") 2)
       (t nil)))

(defun ben-extempore-mode-hook ()
  (turn-on-eldoc-mode)
  (setq eldoc-documentation-function
        'extempore-eldoc-documentation-function)
  (yas-minor-mode-on)
  ;; (if (and (not extempore-logger-mode)
  ;;          (yes-or-no-p "Do you want to log this session?"))
  ;;     (extempore-logger-mode 1))
  (monokai-with-color-variables
    (set-face-attribute 'extempore-blink-eval-face nil :foreground monokai-bg :background "#FD971F")
    (set-face-attribute 'extempore-sb-blink-eval-face nil :foreground monokai-bg :background "#39FF14")))

(add-hook 'extempore-mode-hook 'ben-extempore-mode-hook)

;; syntax highlighting for LLVM IR files

(let ((llvm-mode-file (concat extempore-path "/extras/llvm-mode.el")))
  (if (file-exists-p llvm-mode-file)
      (progn (load-file llvm-mode-file)
             (add-to-list 'auto-mode-alist '("\\.ir$" . llvm-mode)))))

;; session setup

(defun ben-create-extempore-template-file (base-path filename &optional header)
  (unless (file-exists-p (concat base-path filename))
    (progn
      (find-file (concat base-path filename))
      (if header (insert header))
      (save-buffer)
      (kill-buffer))))

(defun ben-create-extempore-template-dir (name)
  "Set up the directory structure and files for a new extempore session/gig."
  (interactive "sSession name: ")
  (let* ((base-path (concat ben-home-dir "/Code/xtm/sessions/" name "/"))
         (setup-header
          (concat ";;; setup.xtm --- setup file for " name "\n"
                  ""
                  "(sys:load \"libs/xtm.xtm\")\n"
                  "(load \"" ben-home-dir "/Code/xtm/lib/ben-lib.xtm\")\n"
                  "(load \"" ben-home-dir "/Code/xtm/lib/sampler-maps.xtm\")\n")))
    (if (file-exists-p base-path)
        (error "Cannot create xtm session: directory \"%s\" already exists" base-path))
    (make-directory base-path)
    ;; practice files
    (ben-create-extempore-template-file
     base-path "practice-scm.xtm" "scmhead")
    (ben-create-extempore-template-file
     base-path "practice-xtlang.xtm" "xthead")
    ;; setup file
    (ben-create-extempore-template-file
     base-path "setup.xtm" setup-header)
    (dired base-path)))

;; RBC minor mode
(autoload 'remote-buffer-control-mode (concat extempore-path "extras/remote-buffer-control.el") "" t)


;;;;;;;;;;;;;
;; paredit ;;
;;;;;;;;;;;;;

;; from https://gist.github.com/malk/4962126

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

(defun paredit-duplicate-closest-sexp ()
  (interactive)
  (paredit-skip-to-start-of-sexp-at-point)
  (set-mark-command nil)
  ;; while we find sexps we move forward on the line
  (while (and (bounds-of-thing-at-point 'sexp)
              (<= (point) (car (bounds-of-thing-at-point 'sexp)))
              (not (= (point) (line-end-position))))
    (forward-sexp)
    (while (looking-at " ")
      (forward-char)))
  (kill-ring-save (mark) (point))
  ;; go to the next line and copy the sexprs we encountered
  (paredit-newline)
  (yank)
  (exchange-point-and-mark))

(defface paredit-paren-face
  '((((class color) (background dark))
     (:foreground "grey50"))
    (((class color) (background light))
     (:foreground "grey55")))
  "Face for parentheses.  Taken from ESK.")

(defun ben-paredit-mode-hook ()
  (define-key paredit-mode-map (kbd "<s-left>") 'paredit-backward-up)
  (define-key paredit-mode-map (kbd "<s-S-left>") 'paredit-backward-down)
  (define-key paredit-mode-map (kbd "<s-right>") 'paredit-forward-up)
  (define-key paredit-mode-map (kbd "<s-S-right>") 'paredit-forward-down)
  (define-key paredit-mode-map (kbd "<M-S-up>") 'paredit-raise-sexp)
  (define-key paredit-mode-map (kbd "<M-S-down>") 'paredit-wrap-sexp)
  (define-key paredit-mode-map (kbd "<M-S-left>") 'paredit-convolute-sexp)
  (define-key paredit-mode-map (kbd "<M-S-right>") 'transpose-sexps)
  (define-key paredit-mode-map (kbd "<s-S-down>") 'paredit-duplicate-closest-sexp))

(add-hook 'paredit-mode-hook 'ben-paredit-mode-hook)

;; turn on paredit by default in all 'lispy' modes

(dolist (mode '(scheme emacs-lisp lisp clojure clojurescript extempore))
  (when (> (display-color-cells) 8)
    (font-lock-add-keywords (intern (concat (symbol-name mode) "-mode"))
                            '(("(\\|)" . 'paredit-paren-face))))
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            'paredit-mode))

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
(yas-global-mode 1)
(setq yas-prompt-functions '(yas-ido-prompt
                             yas-no-prompt))

(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets")

(global-set-key (kbd "C-c C-s") 'yas-insert-snippet)

;;;;;;;;;;;;;;;;;;
;; autocomplete ;;
;;;;;;;;;;;;;;;;;;

;; autocomplete needs to be set up after yasnippet

(require 'auto-complete-config)

;; (ac-set-trigger-key "<tab>")
(add-to-list 'ac-dictionary-directories (concat ben-home-dir "/.emacs.d/ac-dict"))
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

;;;;;;;;;;;;;
;; keyfreq ;;
;;;;;;;;;;;;;

(setq keyfreq-file (concat user-emacs-directory "keyfreq")
      keyfreq-file-lock (concat user-emacs-directory "keyfreq.lock"))

;;;;;;;;;;;;;;
;; floobits ;;
;;;;;;;;;;;;;;

;; (if (file-exists-p "~/.emacs.d/floobits/floobits.el")
;;     (load "~/.emacs.d/floobits/floobits.el"))

;;;;;;;;;;;
;; rudel ;;
;;;;;;;;;;;

(if (file-exists-p "~/.emacs.d/rudel-0.2-4/rudel-loaddefs.el")
    (load-file "~/.emacs.d/rudel-0.2-4/rudel-loaddefs.el"))

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
