;;;;;;;;;;;;;;;;;;;;;;;;
;: ben swift's .emacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;; path

(setenv "PATH" "/Users/ben/.rbenv/shims:/Users/ben/bin:/usr/local/bin:/usr/bin:/bin:/usr/local/sbin:/usr/sbin:/sbin:/usr/X11/bin:/usr/texbin")
(setq exec-path '("/Users/ben/.rbenv/shims" "/usr/local/bin" "/usr/bin" "/bin" "/usr/local/sbin" "/usr/sbin" "/sbin" "/usr/X11/bin" "/usr/texbin" "/usr/local/Library/Contributions/examples" "/private/tmp/homebrew-emacs-HEAD-fekR/lib-src" "/usr/local/Cellar/emacs/HEAD/libexec/emacs/24.0.94/i386-apple-darwin11.3.0"))

;; customisation

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; one-liners

(global-auto-revert-mode t)
(remove-hook 'text-mode-hook 'smart-spacing-mode)
;; (remove-hook 'text-mode-hook 'tabbar-mode)
(add-hook 'text-mode-hook 'auto-fill-mode)
(setq shift-select-mode t)

;; set up browser

(setq browse-default-macosx-browser "/Applications/Safari.app")

;; elpa

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

(when (null package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(starter-kit
                      starter-kit-lisp
                      starter-kit-eshell
                      monokai-theme
                      solarized-theme
                      clojure-mode
                      clojure-test-mode
                      markdown-mode
                      yaml-mode
                      yasnippet
                      yasnippet-bundle
                      ess
                      auctex
                      gist
                      dired+
                      marmalade))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;; yasnippet

(setq yas/root-directory (concat user-emacs-directory "snippets"))

;; aspell

(ispell-change-dictionary "en_GB" t)

;;;;;;;;;;;;;;;;
;; appearance ;;
;;;;;;;;;;;;;;;;

; (load-theme 'deeper-blue t)

;; frames

(setq special-display-regexps nil)

;; faces

(set-face-attribute 'default nil :height 200 :family "Inconsolata")
(set-face-attribute 'variable-pitch nil :height 200 :family "Lucida Grande")

;; time and date

(setq display-time-mode 1)
(setq display-time-day-and-date 1)

;;;;;;;;;;;;;;;;;
;; keybindings ;;
;;;;;;;;;;;;;;;;;

(global-set-key (kbd "<f5>") 'magit-status)
(global-set-key (kbd "<f8>") 'deft)

(global-set-key (kbd "<S-s-return>") 'ns-toggle-fullscreen)

(global-set-key (kbd "<A-backspace>")
		     (lambda () 
		       (interactive)
		       (kill-visual-line 0)))

(global-set-key (kbd "C-c w") 'count-words)

;; Mac OS X-like

(global-set-key (kbd "s-'") 'comment-or-uncomment-region)
(global-set-key (kbd "s-[") (lambda ()
                              (interactive)
                              (other-window -1)))
(global-set-key (kbd "s-]") (lambda ()
                              (interactive)
                              (other-window 1)))
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

(setq eshell-directory-name (concat user-emacs-directory "eshell/"))
(setq eshell-prompt-regexp "^[^@]*@[^ ]* [^ ]* [$#] ")

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

(setq eshell-prompt-function
      (lambda ()
        (concat (user-login-name)
                "@"
                (host-name)
                " "
                (base-name (eshell/pwd))
                (if (= (user-uid) 0) " # " " $ "))))

;;;;;;;;;;;;;;
;; org mode ;;
;;;;;;;;;;;;;;

(add-hook 'org-mode-hook
          '(lambda ()
             ;; keymappings
             (define-key org-mode-map (kbd "<M-left>") 'backward-word)
             (define-key org-mode-map (kbd "<M-right>") 'forward-word)
             (define-key org-mode-map (kbd "<C-left>") 'org-metaleft)
             (define-key org-mode-map (kbd "<C-right>") 'org-metaright)
             (setq org-support-shift-select 'always)))

;; for octopress blogging
;; taken from http://jaderholm.com/blog/blogging-with-org-mode-and-octopress

(setq org-publish-project-alist
   '(("blog" .  (:base-directory "~/Documents/blog/source/org-posts/"
                 :base-extension "org"
                 :publishing-directory "~/Documents/blog/source/_posts/"
                 :sub-superscript ""
                 :recursive t
                 :publishing-function org-publish-org-to-html
                 :headline-levels 4
                 :html-extension "markdown"
                 :body-only t))))

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
  (define-key LaTeX-mode-map (kbd "C-c w") 'latex-word-count)
  (define-key LaTeX-mode-map (kbd "C-c s") 'ispell-buffer))

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

(autoload 'extempore-mode "/Users/ben/Code/extempore/extras/extempore.el" "" t)

;;;;;;;;;;;;;;;;;;;
;; markdown mode ;;
;;;;;;;;;;;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.md" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown" . markdown-mode))

;;;;;;;;;;
;; yaml ;;
;;;;;;;;;;

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;;;;;;;;;
;; git ;;
;;;;;;;;;

(add-to-list 'auto-mode-alist '("\\.gitconfig$" . conf-unix-mode))

;;;;;;;
;; R ;;
;;;;;;;

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
