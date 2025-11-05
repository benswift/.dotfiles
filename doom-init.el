;;; init.el -*- lexical-binding: t; -*-

;; This file controls what Doom modules are enabled and what order they load
;; in. Remember to run 'doom sync' after modifying it!

;; NOTE Press 'SPC h d h' (or 'C-h d h' for non-vim users) to access Doom's
;;      documentation. There you'll find a link to Doom's Module Index where all
;;      of our modules are listed, including what flags they support.

;; NOTE Move your cursor over a module's name (or its flags) and press 'K' (or
;;      'C-c c k' for non-vim users) to view its documentation. This works on
;;      flags as well (those symbols that start with a plus).
;;
;;      Alternatively, press 'gd' (or 'C-c c d') on a module to browse its
;;      directory (for easy access to its source code).

(doom! :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       ;;chinese
       ;;japanese
       ;;layout            ; auie,ctsrnm is the superior home row

       :completion
       company             ; the ultimate code completion backend
       vertico             ; the search engine of the future

       :ui
       doom                ; what makes DOOM look the way it does
       hl-todo             ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       modeline            ; snazzy, Atom-inspired modeline, plus API
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       vi-tilde-fringe     ; fringe tildes to mark beyond EOB

       :editor
       (evil +everywhere)  ; come to the dark side, we have cookies
       snippets            ; my elves. They type so I don't have to

       :emacs
       dired               ; making dired pretty [functional]
       electric            ; smarter, keyword-based electric-indent
       undo                ; persistent, smarter undo for your inevitable mistakes

       :checkers
       syntax              ; tasing you for every semicolon you forget

       :tools
       (eval +overlay)     ; run code, run (also, repls)
       lookup              ; navigate your code and its documentation
       magit               ; a git porcelain for Emacs

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS

       :lang
       emacs-lisp          ; drown in parentheses
       markdown            ; writing docs for people to ignore
       scheme              ; a fully conniving family of lisps

       :config
       ;;literate
       (default +bindings +smartparens))
