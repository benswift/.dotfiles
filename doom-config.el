;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; some things adapted from
;; https://github.com/hlissner/.doom.d/blob/master/config.el
;;
;; note: there's some nice magit & org config stuff in there as well - worth
;; having a look at when I've got my head around Doom a bit better

(setq user-full-name "Ben Swift"
      user-mail-address "ben@benswift.me")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Operator Mono" :size 16)
      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 16))

(setq doom-theme 'doom-one)

(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Documents/org/")

;; start fullscreen
(toggle-frame-fullscreen)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; even with the :editor format module activated, we need to require the aphelia
;; package otherwise the first invocation of +format/region will error out with
;; "Symbol's function definition is void: apheleia--get-formatters".
(require 'apheleia)

;; I like my snipe to stay in the buffer
(after! evil-snipe
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer))

;;; elixir

(after! eglot
  (add-to-list 'eglot-server-programs
               '((elixir-ts-mode heex-ts-mode elixir-mode)
                 "nextls" "--stdio=true")))

(after! elixir
  (require 'inf-elixir)
  (defalias '+elixir/open-repl #'inf-elixir-project)
  (set-repl-handler! 'elixir-mode #'+elixir/open-repl))

;; just like Spacemacs
(setq doom-localleader-key ",")

;; Keybinds
(defun open-main-todo-file ()
  (interactive)
  (find-file "~/Documents/md-scratch/todo.md"))

(map! :leader :desc "open main todo.md file" :n "b o" #'open-main-todo-file)

;; don't use autocomplete in markdown mode - it just slows things down
(add-hook! 'markdown-mode-hook (company-mode -1))

(map! (:after evil-org
       :map evil-org-mode-map
       :n "gk" (cmd! (if (org-at-heading-p)
                         (org-backward-element)
                       (evil-previous-visual-line)))
       :n "gj" (cmd! (if (org-at-heading-p)
                         (org-forward-element)
                       (evil-next-visual-line))))

      :o "o" #'evil-inner-symbol

      :leader "x" #'evil-switch-to-windows-last-buffer)

;;; org-mode

(after! evil-org
  (setq org-roam-dailies-capture-templates
        '(("d" "default" entry "* %?"
           :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d (%A)>

* tasks for today [/]
- [ ]
* journal
")))))

;;;;;;;;;;;;;;;
;; Extempore ;;
;;;;;;;;;;;;;;;

(use-package! extempore-mode
  :mode ("\\.xtm$" . extempore-mode)
  :config
  (setq extempore-program-args "--frames 32")
  (setq extempore-path (expand-file-name "~/Desktop/extempore/"))
  (setq user-extempore-lib-directory (expand-file-name "~/Documents/research/extemporelang/xtm/"))
  (set-face-attribute 'extempore-blink-face nil :foreground "#272822" :background "#FD971F")
  (set-face-attribute 'extempore-sb-blink-face nil :foreground "#272822" :background "#39FF14")
  (sp-local-pair 'extempore-mode "'" nil :actions nil)
  (sp-local-pair 'extempore-mode "`" nil :actions nil)
  )

(map! (:after extempore-mode
       :map extempore-mode-map
       :localleader "cc" #'switch-to-extempore
       :localleader "cj" #'extempore-connect
       :localleader "," #'extempore-send-dwim
       :localleader "ef" #'extempore-send-dwim
       ;; these here to mimic the old Spacemacs lisp state (which I miss)
       :leader "kw" #'sp-wrap-round
       :leader "kW" #'sp-unwrap-sexp
       :leader "ks" #'sp-forward-slurp-sexp
       :leader "kS" #'sp-backward-slurp-sexp
       :leader "kb" #'sp-forward-barf-sexp
       :leader "kB" #'sp-backward-barf-sexp
       :leader "kr" #'sp-raise-sexp
       :leader "kc" #'sp-clone-sexp
       :leader "kC" #'sp-convolute-sexp
       :leader "kt" #'sp-transpose-sexp
       :leader "kk" #'sp-backward-up-sexp
       :leader "kd" #'sp-delete-sexp
       ))

(load! "ben-utils.el" "~/.dotfiles")
