;;;;;;;;;;;;;;;;;;;;;;;;
;: ben swift's .emacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;

(package-initialize)

;; cross-platform setup

(defun nix-specific-setup ()
  (setq ben-home-dir (substitute-in-file-name "$HOME"))
  (setq ben-path (append (list
                          (concat ben-home-dir  "/.rbenv/shims")
                          (concat ben-home-dir  "/bin")
                          "/usr/local/bin" "/usr/bin" "/bin"
                          "/usr/local/sbin" "/usr/sbin" "/sbin"
                          "/usr/X11/bin" "/usr/texbin")
                         ben-path))
  (setenv "PATH" (mapconcat 'identity ben-path ":"))
  (setq exec-path ben-path))

(defun linux-specific-setup ()
  (setq base-face-height 160)
  (setq ben-path '())
  (nix-specific-setup))

(defun osx-specific-setup ()
  (setq base-face-height 200)
  (setq browse-default-macosx-browser "/Applications/Safari.app")
  (setq ben-path
        '("/usr/local/Library/Contributions/examples"
          "/usr/local/Cellar/emacs/HEAD/libexec/emacs/24.1.50/i386-apple-darwin11.4.0"))
  (nix-specific-setup))

(defun windows-specific-setup ()
 (setq base-face-height 160)
 (setq w32-pass-lwindow-to-system nil)
 (setq w32-lwindow-modifier 'super))

(cond ((string-equal system-type "gnu/linux") (linux-specific-setup))
      ((string-equal system-type "darwin") (osx-specific-setup))
      ((string-equal system-type "windows-nt") (windows-specific-setup))
      (t (message "Unknown operating system")))

;; customisation

(setq custom-file (concat user-emacs-directory "custom.el"))
(if (file-readable-p custom-file)
    (load custom-file))

;; time and date

(setq display-time-day-and-date 1)
(display-time-mode 1)

;; one-liners

(global-auto-revert-mode t)
(setq special-display-regexps nil)
(remove-hook 'text-mode-hook 'smart-spacing-mode)
(setq bidi-display-reordering nil)

;;;;;;;;;;
;; elpa ;;
;;;;;;;;;;

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

(defun ben-install-packages ()
  "Install some handy packages from marmalade."
  (interactive)
  (package-initialize)
  (package-refresh-contents)
  (dolist (p '(starter-kit
	       starter-kit-lisp
	       starter-kit-eshell
	       monokai-theme
	       solarized-theme
	       markdown-mode
	       yaml-mode
	       yasnippet
               yasnippet-bundle
               htmlize
               org
	       ess
	       auctex
	       gist))
    (when (not (package-installed-p p))
      (package-install p))))

;;;;;;;;;;;
;; theme ;;
;;;;;;;;;;;

(load-theme 'monokai t)
(add-to-list 'default-frame-alist '(background-mode . dark))

;;;;;;;;;;;
;; faces ;;
;;;;;;;;;;;

(set-face-attribute 'default nil :height base-face-height :family "Inconsolata")
(set-face-attribute 'variable-pitch nil :height base-face-height :family "Lucida Grande")

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f5>") 'magit-status)

(global-set-key (kbd "<S-s-return>") 'ns-toggle-fullscreen)

(global-set-key (kbd "<A-backspace>")
		     (lambda () 
		       (interactive)
		       (kill-visual-line 0)))

(global-set-key (kbd "C-x u") 'find-grep)
(global-set-key (kbd "C-c b") 'browse-url-at-point)

;; window navigation

(global-set-key (kbd "s-[") (lambda ()
                              (interactive)
                              (other-window -1)))
(global-set-key (kbd "s-]") (lambda ()
                              (interactive)
                              (other-window 1)))
(global-set-key (kbd "s-{") 'shrink-window-horizontally)
(global-set-key (kbd "s-}") 'enlarge-window-horizontally)

;; Mac OS X-like

(global-set-key (kbd "<s-left>") (lambda ()
                                   (interactive)
                                   (move-beginning-of-line 1)))
(global-set-key (kbd "<s-right>") (lambda ()
                                    (interactive)
                                    (move-end-of-line 1)))
(global-set-key (kbd "<s-up>") (lambda ()
                                 (interactive)
                                 (goto-char (point-min))))
(global-set-key (kbd "<s-down>") (lambda ()
                                   (interactive)
                                   (goto-char (point-max))))
(global-set-key (kbd "<M-kp-delete>") (lambda ()
                                        (interactive)
                                        (kill-word 1)))
(global-set-key (kbd "<M-backspace>") (lambda ()
                                        (interactive)
                                        (backward-kill-word 1)))
(global-set-key (kbd "<s-backspace>") (lambda ()
                                        (interactive)
                                        (kill-visual-line 0)))
(global-set-key (kbd "<s-kp-delete>") (lambda ()
                                        (interactive)
                                        (kill-visual-line)))

;;;;;;;;;;;;
;; eshell ;;
;;;;;;;;;;;;

(setq eshell-aliases-file "~/.dotfiles/eshell-alias")

(add-hook 'eshell-mode-hook
          '(lambda ()
             ;; keybindings
             (define-key eshell-mode-map (kbd "<C-up>") 'eshell-previous-matching-input-from-input)
             (define-key eshell-mode-map (kbd "<C-down>") 'eshell-next-matching-input-from-input)
             (define-key eshell-mode-map (kbd "<up>") 'previous-line)
             (define-key eshell-mode-map (kbd "<down>") 'next-line)
             ;;faces
             (set-face-attribute 'eshell-prompt nil :inherit font-lock-function-name-face)
             ;; prompt helpers
             (setq eshell-directory-name (concat user-emacs-directory "eshell/"))
             (setq eshell-prompt-regexp "^[^@]*@[^ ]* [^ ]* [$#] ")
             (setq eshell-prompt-function
                   (lambda ()
                     (concat (user-login-name) "@" (host-name) " "
                             (base-name (eshell/pwd))
                             (if (= (user-uid) 0) " # " " $ "))))
             ;; helpful modes
             (turn-on-eldoc-mode)))

(global-set-key (kbd "C-c s") 'eshell)

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
;; dired ;;
;;;;;;;;;;;

(setq dired-listing-switches "-alh")

;;;;;;;;;;;
;; magit ;;
;;;;;;;;;;;

(setq magit-save-some-buffers nil)

;;;;;;;;;;;;;;
;; org mode ;;
;;;;;;;;;;;;;;

(setq org-completion-use-ido t)

(add-hook 'org-mode-hook
          '(lambda ()
             ;; keymappings
             (define-key org-mode-map (kbd "<M-left>") 'backward-word)
             (define-key org-mode-map (kbd "<M-right>") 'forward-word)
             (define-key org-mode-map (kbd "<C-left>") 'org-metaleft)
             (define-key org-mode-map (kbd "<C-right>") 'org-metaright)
             ;;faces
             (set-face-attribute 'outline-2 nil :inherit font-lock-string-face)
             (set-face-attribute 'outline-3 nil :inherit font-lock-type-face)
             (set-face-attribute 'outline-4 nil :inherit font-lock-keyword-face)
             (set-face-attribute 'outline-5 nil :inherit font-lock-constant-face)
             (set-face-attribute 'outline-6 nil :inherit font-lock-comment-face)))

;; for octopress blogging
;; taken from http://jaderholm.com/blog/blogging-with-org-mode-and-octopress

(setq org-publish-project-alist
   '(("blog" .  (:base-directory "~/Documents/blog/source/org-posts/"
                 :base-extension "org"
                 :exclude "drafts/*"                
                 :publishing-directory "~/Documents/blog/source/_posts/"
                 :sub-superscript ""
                 :recursive t
                 :publishing-function org-publish-org-to-html
                 :headline-levels 4
                 :html-extension "markdown"
                 :body-only t))))

;;;;;;;;;
;; erc ;;
;;;;;;;;;

(erc-services-mode 1)
(setq erc-nick "benswift")
(load "~/.dotfiles/secrets/ercpass")
(setq erc-prompt-for-password nil)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-autojoin-channels-alist '(("freenode.net" "#extempore")))

(add-hook 'erc-mode-hook
          '(lambda ()
             ;; faces
             (set-face-attribute 'erc-input-face nil :foreground nil :inherit font-lock-string-face)
             (set-face-attribute 'erc-my-nick-face nil :foreground nil :inherit font-lock-keyword-face)
             (set-face-attribute 'erc-notice-face nil :foreground nil :inherit font-lock-warning-face)))

;;;;;;;;;;;
;; LaTeX ;;
;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.cls" . LaTeX-mode))

(defun ben-latex-setup ()
  (setq TeX-engine 'xetex)
  (setq TeX-PDF-mode t)
  (setq TeX-auto-untabify t)
  (setq TeX-parse-self t) ; Enable parse on load.
  (setq TeX-auto-save t) ; Enable parse on save.
  (setq TeX-view-program-selection '(output-pdf "Skim"))
  (add-to-list 'TeX-command-list
                  '("Biber" "biber %s" TeX-run-Biber nil t :help "Run Biber"))
  (add-to-list 'TeX-command-list
               '("Glossary" "makeglossaries %s" TeX-run-command nil t :help "Create glossaries"))
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

(add-hook 'LaTeX-mode-hook 'ben-latex-setup) 
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

(autoload 'extempore-mode (concat ben-home-dir "/Code/extempore/extras/extempore.el") "" t)
(add-to-list 'auto-mode-alist '("\\.xtm$" . extempore-mode))
(add-hook 'extempore-mode-hook 'enable-paredit-mode)
(setq extempore-tab-completion nil)

;; on cyril

(cond ((string-equal (system-name) "cyril.local")
       (setq extempore-path (concat ben-home-dir "/Code/extempore"))))

;;;;;;;;;;;;;
;; paredit ;;
;;;;;;;;;;;;;

(add-hook 'paredit-mode-hook
          '(lambda ()
             (define-key paredit-mode-map (kbd "<M-S-up>") 'paredit-raise-sexp)
             (define-key paredit-mode-map (kbd "<M-S-down>") 'paredit-wrap-sexp)
             (define-key paredit-mode-map (kbd "<M-S-left>") 'paredit-convolute-sexp)
             (define-key paredit-mode-map (kbd "<M-S-right>") 'transpose-sexps)))

;;;;;;;;;;;;;;
;; markdown ;;
;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

;;;;;;;;;;
;; yaml ;;
;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;;;;;;;;;;;;;;
;; yasnippet ;;
;;;;;;;;;;;;;;;

(setq yas/root-directory "~/.dotfiles/yasnippets")
(yas/load-directory yas/root-directory)
(setq yas/prompt-functions '(yas/ido-prompt
                             yas/dropdown-prompt
                             yas/completing-prompt
                             yas/no-prompt))

;;;;;;;;;
;; git ;;
;;;;;;;;;

(add-to-list 'auto-mode-alist '(".*gitconfig$" . conf-unix-mode))
(add-to-list 'auto-mode-alist '(".*gitignore$" . conf-unix-mode))

;;;;;;;
;; R ;;
;;;;;;;

(require 'ess-site)

(add-hook 'ess-mode-hook
          '(lambda()
             (setq-default ess-language "R")
             (setq ess-my-extra-R-function-keywords
                   (read-lines (concat user-emacs-directory
                                       "R-function-names.txt")))
             (setq ess-R-mode-font-lock-keywords
                   (append ess-R-mode-font-lock-keywords
                           (list (cons (concat "\\<" (regexp-opt
                                                      ess-my-extra-R-function-keywords 'enc-paren) "\\>")
                                       'font-lock-function-name-face))))))

;;;;;;;;;;;;;;;;;;;;;
;; bits and pieces ;;
;;;;;;;;;;;;;;;;;;;;;

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
        (toggle-read-only 1)
        (kill-whole-line)
        (toggle-read-only 0)
        (yank)))))

(global-set-key (kbd "C-c d") 'duplicate-line)
