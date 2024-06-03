;;; init.el -*- lexical-binding: t; -*-
(doom! :input
       ;;bidi              ; (tfel ot) thgir etirw uoy gnipleh
       (chinese +rime
                +childframe)
       ;;japanese
       layout            ; auie,ctsrnm is the superior home row

       :completion
       ;;(company +auto
       ;;         +capf)   ; the ultimate code completion backend
       ;;helm            ; the *other* search engine for love and life
       ;;ido             ; the other *other* search engine...
       ;;ivy             ; a search engine for love and life
       (corfu +icons)
       (vertico +icons
                +childframe)  ; the search engine of the future

       :ui
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       hl-todo           ; highlight TODO/FIXME/NOTE/DEPRECATED/HACK/REVIEW
       ;;indent-guides     ; highlighted indent columns
       (ligatures +extra); ligatures and symbols to make your code pretty again
       ;;minimap           ; show a map of the code on the side
       modeline          ; snazzy, Atom-inspired modeline, plus API
       ;;nav-flash         ; blink cursor line after big motions
       ophints           ; highlight the region an operation acts on
       (popup +defaults)   ; tame sudden yet inevitable temporary windows
       ;;tabs              ; a tab bar for Emacs
       treemacs          ; a project drawer, like neotree but cooler
       unicode           ; extended unicode support for various languages
       (vc-gutter +pretty
                  +diff-hl) ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       (window-select +numbers)     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       (evil +everywhere)  ; come to the dark side, we have cookies
       file-templates      ; auto-snippets for empty files
       fold                ; (nigh) universal code folding
       ;;(format +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       ;;objed             ; text object editing for the innocent
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       snippets          ; my elves. They type so I don't have to
       word-wrap

       :emacs
       (dired +icons)    ; making dired pretty [functional]
       electric          ; smarter, keyword-based electric-indent
       (ibuffer +icons)  ; interactive buffer management
       (undo +tree)      ; persistent, smarter undo for your inevitable mistakes
       vc                ; version-control and Emacs, sitting in a tree

       :term
       ;;eshell            ; the elisp shell that works everywhere
       ;;shell             ; simple shell REPL for Emacs
       ;;term              ; basic terminal emulator for Emacs
       vterm             ; the best terminal emulation in Emacs

       :checkers
       syntax              ; tasing you for every semicolon you forget
       (spell +flyspell
              +hunspell)   ; tasing you for misspelling mispelling
       ; grammar             ; tasing grammar mistake every you make

       :tools
       ;;ansible
       ;;biblio            ; Writes a PhD for you (citation needed)
       ;;debugger          ; FIXME stepping through code, to help you add bugs
       direnv
       ;;docker
       editorconfig        ; let someone else argue about tabs vs spaces
       ein                 ; tame Jupyter notebooks with emacs
       (eval +overlay)     ; run code, run (also, repls)
       (lookup +docsets)   ; navigate your code and its documentation
       (lsp +eglot)        ; M-x vscode
       (magit +forge)      ; a git porcelain for Emacs
       make                ; run make tasks from Emacs
       ;;pass              ; password manager for nerds
       pdf                 ; pdf enhancements
       rgb                 ; creating color strings
       ;;taskrunner        ; taskrunner for all your projects
       ;;tmux              ; an API for interacting with tmux
       tree-sitter         ; syntax and parsing, sitting in a tree...
       upload              ; map local to remote projects via ssh/ftp

       :os
       (:if IS-MAC macos)  ; improve compatibility with macOS
       ;;tty               ; improve the terminal Emacs experience

       :lang
       ;;(cc +lsp)         ; C > C++ == 1
       ;;clojure           ; java with a lisp
       data                ; config/data formats
       ;;elm               ; care for a cup of TEA?
       (emacs-lisp +treesitter)       ; drown in parentheses
       fortran             ; in FORTRAN, GOD is REAL (unless declared INTEGER)
       (json +treesitter)  ; At least it ain't XML
       javascript          ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       (latex +latexmk
              +cdlatex)    ; writing papers in Emacs has never been so fun
       lua               ; one-based indices? one-based indices
       markdown            ; writing docs for people to ignore
       ;;nix               ; I hereby declare "nix geht mehr!"
       ;;ocaml             ; an objective camel
       (org
            +dragndrop
            +gnuplot
            +contacts
            +ipython
            +jupyter
            +present
            +pretty
            +pandoc
            +noter
            +roam2)        ; organize your plain life in plain text
       ;;plantuml          ; diagrams for confusing people more
       python              ; beautiful is better than ugly
       ;;qt                ; the 'cutest' gui framework ever
       rst               ; ReST in peace
       ruby
       ;;(rust +lsp)       ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (sh +zsh)           ; she sells {ba,z,fi}sh shells on the C xor
       ;;swift             ; who asked for emoji variables?
       ;;web               ; the tubes
       yaml              ; JSON, but readable

       :email
       ;;(mu4e +org +gmail)
       ;;notmuch
       ;;(wanderlust +gmail)

       :app
       calendar
       (rss +org)        ; emacs as an RSS reader

       :config
       ;;literate
       (default +bindings +smartparens)
)
