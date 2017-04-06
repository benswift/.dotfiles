;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused
   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     erc
     ivy
     (auto-completion :variables auto-completion-tab-key-behavior 'cycle)
     emacs-lisp
     git
     markdown
     mu4e
     org
     asciidoc
     asm
     bibtex
     c-c++
     clojure
     common-lisp
     csharp
     csv
     emacs-lisp
     ess
     extra-langs
     fsharp
     go
     graphviz
     haskell
     html
     ipython-notebook
     ;; java ;; because eclim is balls
     javascript
     latex
     lua
     markdown
     php
     python
     racket
     ruby
     rust
     scheme
     shaders
     (shell :variables shell-default-shell 'shell)
     shell-scripts
     sql
     swift
     windows-scripts
     yaml
     spell-checking
     syntax-checking
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages '(processing-mode dockerfile-mode extempore-mode lice)
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages '()
   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'vim
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("Source Code Pro"
                               :size 16
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state t
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers 'relative
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;; ispell
  (when (executable-find "hunspell")
    (setq ispell-program-name (executable-find "hunspell"))
    (setq ispell-dictionary "en_AU"))

  ;; spacemacs
  (setq spacemacs-theme-org-height nil)

  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (ben-mu4e-config)

  ;; time
  (setq display-time-format "%H:%M")
  (display-time-mode 1)

  ;; emacs
  (setq mac-option-modifier 'meta)
  (setq mac-command-modifier 'super)

  (add-hook 'text-mode-hook 'turn-on-auto-fill)

  ;; ERC
  (if (load "~/.dotfiles/secrets/ercpass" t)
      (progn
        (erc-services-mode 1)
        (setq erc-nick "benswift")
        (setq erc-prompt-for-password nil)
        (setq erc-prompt-for-nickserv-password nil)
        (setq erc-autojoin-channels-alist '(("freenode.net" "#extempore")))
        (setq erc-notify-list '("digego")))
    (message "Couldn't find the secrets file, you need to pull it down from dropbox."))

  ;; processing
  (setq processing-sketchbook-dir "~/Code/processing")
  (setq processing-location "/usr/local/bin/processing-java")

  (defun osx-screencapture (filename)
    (interactive "sfilename: ")
    (shell-command (format "screencapture -i \"%s.png\"" filename)))

  ;; yasnippet
  (add-to-list 'yas-snippet-dirs "~/.dotfiles/snippets/")

  ;; biott
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
      (find-file (format "~/Code/clojure/biott-redux/resources/templates/md/posts/%s.md"
                         post-url-basename))
      (insert (format
               "{:title \"%s\"
 :layout :post}
"
               post-name))))

  ;; chord charts
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
                               output-filename))))))

(defun ben-mu4e-config ()
  "user-config for mu4e"

  (require 'mu4e-contrib) ;; for mu4e-shr2text
  (require 'smtpmail)

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

  (setq mu4e-compose-dont-reply-to-self t)
  (setq mu4e-compose-signature-auto-include nil)
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
                  (smtpmail-smtp-server . "mail.simeonnetwork.org")))))


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
        (async-smtpmail-send-it)))))

(defun ben-extempore-config ()
  "customisation for Extempore"

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

  (defun extempore-run-integration-test ()
    (interactive)
    (let ((default-directory (concat extempore-share-directory "extras/")))
      (async-shell-command "./integration-test.sh"
                           (get-buffer-create "*extempore-integration-test*"))))

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

  (autoload 'extempore-debovinate-file "~/.dotfiles/extempore-debovinator.el" "debovinate all the things!" :interactive))

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (flyspell-correct-ivy cider eclim counsel ess company-quickhelp haskell-mode flycheck helm helm-core magit with-editor rust-mode js2-mode flycheck-rust flycheck-pos-tip flycheck-haskell flyspell-correct auto-dictionary helm-themes helm-swoop helm-projectile helm-mode-manager helm-flx helm-descbinds helm-ag ace-jump-helm-line yapfify yaml-mode x86-lookup ws-butler wolfram-mode window-numbering which-key wgrep web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toml-mode toc-org thrift tagedit swift-mode stan-mode sql-indent spacemacs-theme spaceline smex smeargle slime-company slim-mode scss-mode scad-mode sass-mode rvm ruby-tools ruby-test-mode rubocop rspec-mode robe restart-emacs rbenv rake rainbow-delimiters racket-mode racer quelpa qml-mode pyvenv pytest pyenv-mode py-isort pug-mode powershell popwin pip-requirements phpunit phpcbf php-extras php-auto-yasnippets persp-mode paradox orgit org-ref org-projectile org-present org-pomodoro org-plus-contrib org-download org-bullets open-junk-file omnisharp neotree nasm-mode mu4e-maildirs-extension mu4e-alert move-text mmm-mode minitest matlab-mode markdown-toc magit-gitflow lua-mode lorem-ipsum livid-mode live-py-mode linum-relative link-hint less-css-mode json-mode js2-refactor js-doc ivy-hydra intero insert-shebang info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hlint-refactor hl-todo hindent highlight-parentheses highlight-numbers highlight-indentation hide-comnt help-fns+ helm-make haskell-snippets graphviz-dot-mode google-translate golden-ratio go-guru go-eldoc gnuplot glsl-mode gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md geiser fsharp-mode flx-ido fish-mode fill-column-indicator fancy-battery eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-tutor evil-surround evil-search-highlight-persist evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-args evil-anzu ess-smart-equals ess-R-object-popup ess-R-data-view erc-yt erc-view-log erc-terminal-notifier erc-social-graph erc-image erc-hl-nicks emmet-mode elisp-slime-nav ein dumb-jump drupal-mode disaster define-word cython-mode csv-mode counsel-projectile company-web company-tern company-statistics company-shell company-go company-ghci company-ghc company-emacs-eclim company-cabal company-c-headers company-auctex company-anaconda common-lisp-snippets column-enforce-mode coffee-mode cmm-mode cmake-mode clojure-snippets clj-refactor clean-aindent-mode clang-format cider-eval-sexp-fu chruby cargo bundler auto-yasnippet auto-highlight-symbol auto-compile auctex-latexmk arduino-mode ahk-mode aggressive-indent adoc-mode adaptive-wrap ace-window ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
