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
(setq doom-font (font-spec :family "Operator Mono" :size 16 :weight 'semi-light)
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

;;;;;;;;;;;;;;;;;;;;;;
;; Language Servers ;;
;;;;;;;;;;;;;;;;;;;;;;

;; NextLS
;;
;; to download latest NextLS, `curl -L -o nextls "https://github.com/elixir-tools/next-ls/releases/download/$VERSION/next_ls_darwin_arm64"'
;; if NextLS isn't starting, epmd might not be running; `epmd -daemon' to start

(after! eglot
  (add-to-list 'eglot-server-programs
               '((elixir-mode elixir-ts-mode heex-ts-mode)
                 "/Users/ben/LSP/elixir/nextls" "--stdio=true")))

(after! evil-snipe
  (setq evil-snipe-scope 'buffer
        evil-snipe-repeat-scope 'buffer))

;;; elixir

(after! elixir (require 'inf-elixir))

;; not sure why these aren't getting called if they're inside the above `after!
;; elixir' block... will investigate later
(defalias '+elixir/open-repl #'inf-elixir-project)
(set-repl-handler! 'elixir-mode #'+elixir/open-repl)

;; just like Spacemacs
(setq doom-localleader-key ",")

;;
;;; Keybinds

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

;;; ben-utils

(defun ben-sync-org-directory-to-github ()
  (interactive)
  (let ((default-directory org-directory))
    (async-shell-command
     (format "git add *.org roam/*.org roam/daily/*.org && git commit -m 'org directory auto-commit script @ %s' && git pull --rebase origin master && git push origin master"
             (format-time-string "%FT%T%z")))))
