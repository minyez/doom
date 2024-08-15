;;; config.el -*- lexical-binding: t; -*-

(setq user-full-name "Minye Zhang"
      user-mail-address "minyez.physchem@gmail.com")

; (add-to-list 'load-path (concat doom-private-dir "lisp"))

(if (getenv "EMACS_PROF")
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(setq confirm-kill-emacs nil)     ; do not ask if I would like to go

;; ================================================
;; Personal variables
;; ================================================
(defvar my/org-dir
  (expand-file-name "org-roam/" "~/Library/CloudStorage/Dropbox")
  "org directory")

(defvar my/org-roam-inbox
  (expand-file-name my/org-dir)
  "location where usual org-roam-capture goes")

(defvar my/read-note-dir
  (expand-file-name "Review/" (concat my/org-dir))
  "directory for reading notes")

(defvar my/bibtex-file
  (expand-file-name "etc/bibliography.bib" my/org-dir)
  "all-in-one bibtex file for referecnes")

(defvar my/literature-note-dir
  (expand-file-name "Paper/" my/org-dir)
  "Directory to store the notes of literature")

(defvar my/talk-note-dir
  (expand-file-name "Talk/" my/org-dir)
  "Directory to store the notes of talks, conferences")

(defvar my/jekyll-root-directory "~/blogs/minyez.github.io_chirpy/"
  "Jekyll root directory")

(require 's) ;; for joining strings

(defvar my/org-latex-classes-common-header-passoptions
  (s-join "\n"
    '("\\PassOptionsToPackage{dvipsnames,x11names,table}{xcolor}"
      "\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Rhodamine,pdfborder={0 0 0},breaklinks=true,linktoc=all}{hyperref}"))
  "PassOptions setting before document class, included in org-latex-classes for exporting org to latex")

(defvar my/org-latex-classes-common-header-after-default-pkgs
  (s-join "\n"
    '("% redefine quote environment - blockquote from eisvogel"
      "\\definecolor{bg}{rgb}{0.95,0.95,0.95}"
      "\\definecolor{bq-border}{RGB}{0, 63, 126}"
      "\\newmdenv[rightline=false,bottomline=false,topline=false,linewidth=3pt,backgroundcolor=bg,%"
      "           linecolor=bq-border,skipabove=\\parskip]{customblockquote}"
      "\\renewenvironment{quote}{\\begin{customblockquote}\\itshape\\list{}{\\rightmargin=6pt\\leftmargin=6pt}%"
      "\\item\\relax\\ignorespaces}{\\unskip\\unskip\\endlist\\end{customblockquote}}"
      "\\let\\Oldtextbullet\\textbullet"
      "\\renewcommand{\\textbullet}{\\textcolor{bq-border}{\\Oldtextbullet}}"
      "% compact itemize by paralist packages"
      "\\usepackage{paralist}"
      "\\let\\itemize\\compactitem"
      "\\let\\description\\compactdesc"
      "\\let\\enumerate\\compactenum"
      ))
  "Headers after default packages setting, included in org-latex-classes for exporting org to latex")
;; ================================================
;; Key bindings
;;
;; - Extra binding map
;; - key-chord
;; ================================================
(map! :leader
      (:prefix-map ("e" . "extra-my")
        (:prefix-map ("D" . "drill")
          :desc "org-drill"          "d"   #'org-drill
          :desc "org-drill-resume"   "r"   #'org-drill-resume)
        (:prefix-map ("d" . "download")
          :desc "rename"                    "r"   #'org-download-rename-at-point
          :desc "image from clipboard"      "p"   #'org-download-clipboard
          :desc "download from kill URL"    "d"   #'org-download-yank)
        (:prefix-map ("i" . "input")
          :desc "Chinese (Pyim)"            "c"  '(lambda () (interactive) (set-input-method "pyim"))
          :desc "Japanese"                  "j"  '(lambda () (interactive) (set-input-method "japanese"))
          :desc "Tex"                       "t"  '(lambda () (interactive) (set-input-method "TeX")))
        :desc "org-roam-node-find"      "."   #'org-roam-node-find
        :desc "Find file Other Window"  "f"   #'find-file-other-window
        :desc "org-agenda-list"         "a"   #'org-agenda-list
        :desc "Sort entries by todo"    "S"   '(lambda () (interactive) (org-sort-entries t ?o))
        :desc "org-schedule"            "s"   #'org-schedule)

      ;; supplement existing map
      (:prefix-map ("t" . "toggle")
        :desc "truncate-lines"       "L"   #'toggle-truncate-lines)

      ;; supplement existing map
      (:prefix-map ("r" . "reference")
        :desc "citar-copy-reference"   "r"   #'citar-copy-reference
        :desc "citar-open"             "o"   #'citar-open
        :desc "citar-insert-citation"  "i"   #'citar-insert-citation
        :desc "citar-insert-keys"      "k"   #'citar-insert-keys)

      (:prefix-map ("g" . "git")
       (:when (modulep! :tools magit)
        ; "SPC g a" for stage file, just as "git add"
        :desc "stage current buffer file"    "a"    #'magit-stage-buffer-file))
)

(map!
  :nv "SPC m u" #'outline-up-heading)


(use-package! key-chord
  :config
  (key-chord-mode 1)
  ;; https://discourse.doomemacs.org/t/typing-jk-deletes-j-and-returns-to-normal-mode/59
  ;; commented out for double pinyin
  ;(with-eval-after-load 'evil-escape
  ;  (key-chord-define-global "jk" 'evil-escape))
)

;; ================================================
;; UI setup
;;
;; - font
;; - theme
;; - dashboard
;; - nerd-icons
;; - treemacs
;; - winum
;; - modeline
;; - svg-tag-mode
;; - hl-todo
;; - eldoc-box
;;
;; ================================================
(display-time-mode 1) ;; always show time
;; relative visual line, usually better than relative with folded blocks
(setq display-line-numbers-type 'visual)
;; calculate necessary width to correctly show all line numbers, since 28.1
(setq display-line-numbers-width-start t)
;; Disable prettify-symbols-mode, which makes hard to recognize keywords in code
;; Somehow not work, maybe it is hooked somewhere, not found in the Doom repo by grep.
;; Switching off the ligature module helps.
;; (prettify-symbols-mode -1)

(let ((font "Sarasa Fixed Slab SC Nerd Font")
      (size 16))
  (setq doom-font (font-spec :family font :size size)
        doom-variable-pitch-font (font-spec :family font :size size)
        ;; disable unicode font size set to resolve too large icon in dashboard/treemacs
        ;;doom-unicode-font (font-spec :family font :size size)
        doom-big-font (font-spec :family font :size (+ size 4))))

;; Faces with whitespace-mode can have impact for large files
;; See https://list.orgmode.org/orgmode/Zqjm0hyy5DjFNrgm@swain.home.arpa/
;;     https://www.reddit.com/r/orgmode/comments/1eovrxa/russell_adams_mlorg_mode_2024_speedup_on_large
(setq whitespace-style '(space-mark tab-mark))

;; Theme setup
(use-package! emacs
  :init
  ;; Add all your customizations prior to loading the themes
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs nil
        modus-themes-org-blocks 'gray-background
	;; 'gray-background is not very clear with modus-vivendi
	;; or 'tinted-background
        modus-themes-region '(bg-only no-extend)
        modus-themes-paren-match '(bold intense)
        modus-themes-links '(neutral-underline background)
        modus-themes-deuteranopia t
        modus-themes-mode-line '(accented borderless (padding . 4) (height . 0.9)))
  :bind ("<f5>" . modus-themes-toggle)
)
(setq doom-theme 'modus-operandi)

;; dashboard configuration
;; set custom splash image if it exists
(let ((img (expand-file-name "misc/splash-images/favicon.svg" doom-private-dir)))
 (if (file-exists-p img)
   (setq fancy-splash-image img)))

(when (modulep! :ui doom-dashboard)
  ;; remove the footer, i.e. the GitHub icon
  (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-footer)
  ;; remove short menu
  ;; (remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
  ;; add a new button
  (add-to-list '+doom-dashboard-menu-sections
               '("Browse roam nodes"
                 :icon (nerd-icons-sucicon "nf-custom-orgmode" :face 'doom-dashboard-menu-title)
                 :when (featurep! :lang org +roam2)
                 :face (:inherit (doom-dashboard-menu-title))
                 :action org-roam-node-find))
  ;; remove some of the buttons
  (dolist (btname '("Open project" "Open org-agenda"))
    (assoc-delete-all btname +doom-dashboard-menu-sections))
)

(use-package! nerd-icons
  :config
  (setq nerd-icons-scale-factor 1.0)
)

(use-package! treemacs
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "s-0") #'treemacs-select-window))
  :bind
  (:map global-map
        ("s-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :config
  (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay        0.5
        treemacs-directory-name-transformer      #'identity
        treemacs-display-in-side-window          t
        treemacs-eldoc-display                   'simple
        treemacs-file-event-delay                2000
        treemacs-file-extension-regex            treemacs-last-period-regex-value
        treemacs-file-follow-delay               0.2
        treemacs-file-name-transformer           #'identity
        treemacs-follow-after-init               t
        treemacs-expand-after-init               t
        treemacs-find-workspace-method           'find-for-file-or-pick-first
        treemacs-git-command-pipe                ""
        treemacs-goto-tag-strategy               'refetch-index
        treemacs-header-scroll-indicators        '(nil . "^^^^^^")
        treemacs-hide-dot-git-directory          t
        treemacs-indentation                     2
        treemacs-indentation-string              " "
        treemacs-is-never-other-window           nil
        treemacs-max-git-entries                 5000
        treemacs-missing-project-action          'ask
        treemacs-move-forward-on-expand          nil
        treemacs-no-png-images                   nil
        treemacs-no-delete-other-windows         t
        treemacs-project-follow-cleanup          nil
        treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                        'left
        treemacs-read-string-input               'from-child-frame
        treemacs-recenter-distance               0.1
        treemacs-recenter-after-file-follow      nil
        treemacs-recenter-after-tag-follow       nil
        treemacs-recenter-after-project-jump     'always
        treemacs-recenter-after-project-expand   'on-distance
        treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
        treemacs-project-follow-into-home        nil
        treemacs-show-cursor                     nil
        treemacs-show-hidden-files               t
        treemacs-silent-filewatch                nil
        treemacs-silent-refresh                  nil
        treemacs-sorting                         'alphabetic-asc
        treemacs-select-when-already-in-treemacs 'move-back
        treemacs-space-between-root-nodes        t
        treemacs-tag-follow-cleanup              t
        treemacs-tag-follow-delay                1.5
        treemacs-text-scale                      nil
        treemacs-user-mode-line-format           nil
        treemacs-user-header-line-format         nil
        treemacs-wide-toggle-width               70
        treemacs-width                           35
        treemacs-width-increment                 1
        treemacs-width-is-initially-locked       t
        treemacs-workspace-switch-cleanup        nil)

  ;; The default width and height of the icons is 22 pixels. If you are
  ;; using a Hi-DPI display, uncomment this to double the icon size.
  ;;(treemacs-resize-icons 44)

  ;;(treemacs-follow-mode t)
  (treemacs-filewatch-mode t)
  (treemacs-fringe-indicator-mode 'always)
  (when treemacs-python-executable
    (treemacs-git-commit-diff-mode t))

  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))

  (treemacs-hide-gitignored-files-mode nil)
)

(map! :leader
      (:prefix-map ("t" . "toggle")
       (:when (modulep! :ui treemacs)
        :desc "treemacs" "t" #'treemacs)))

(when (modulep! :ui window-select +numbers)
 (use-package! winum

  :bind
  (:map global-map
        ("s-0" . treemacs-select-window)
        ("s-1" . winum-select-window-1)
        ("s-2" . winum-select-window-2)
        ("s-3" . winum-select-window-3)
        ("s-4" . winum-select-window-4)
        ("s-5" . winum-select-window-5)
        ("s-6" . winum-select-window-6)
        ("s-7" . winum-select-window-7)
        ("s-8" . winum-select-window-8)
        ("s-9" . winum-select-window-9))
  (:map evil-normal-state-map
        ("s-0" . treemacs-select-window)
        ("s-1" . winum-select-window-1)
        ("s-2" . winum-select-window-2)
        ("s-3" . winum-select-window-3)
        ("s-4" . winum-select-window-4)
        ("s-5" . winum-select-window-5)
        ("s-6" . winum-select-window-6)
        ("s-7" . winum-select-window-7)
        ("s-8" . winum-select-window-8)
        ("s-9" . winum-select-window-9))))

(when (modulep! :ui modeline)
  (use-package! doom-modeline
    :config
    (setq doom-modeline-time t
          doom-modeline-enable-word-count t
          doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))))

(use-package! hl-todo
  :hook
  ((org-mode . hl-todo-mode)
   (markdown-mode . hl-todo-mode)
   (prog-mode . hl-todo-mode)))

(use-package! svg-tag-mode
  :init
  ;; from https://github.com/rougier/svg-tag-mode/blob/main/examples/example-2.el
  (defconst date-re "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}")
  (defconst time-re "[0-9]\\{2\\}:[0-9]\\{2\\}")
  (defconst day-re "[A-Za-z]\\{3\\}")
  (defconst day-time-re (format "\\(%s\\)? ?\\(%s\\)?" day-re time-re))

  (defun svg-progress-percent (value)
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ (string-to-number value) 100.0)
                                  nil :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag (concat value "%")
                             nil :stroke 0 :margin 0)) :ascent 'center))

  (defun svg-progress-count (value)
    (let* ((seq (mapcar #'string-to-number (split-string value "/")))
           (count (float (car seq)))
           (total (float (cadr seq))))
    (svg-image (svg-lib-concat
                (svg-lib-progress-bar (/ count total) nil
                                      :margin 0 :stroke 2 :radius 3 :padding 2 :width 11)
                (svg-lib-tag value nil
                             :stroke 0 :margin 0)) :ascent 'center)))

  (setq svg-tag-tags
    `(
      ;; Org tags
;      (":\\([A-Za-z0-9]+\\)" . ((lambda (tag) (svg-tag-make tag))))
;      (":\\([A-Za-z0-9]+[ \-]\\)" . ((lambda (tag) tag)))

      ;; Task priority
;      ("\\[#[A-Z]\\]" . ( (lambda (tag)
;                            (svg-tag-make tag :face 'org-priority
;                                          :beg 2 :end -1 :margin 0))))

      ;; Progress
;      ("\\(\\[[0-9]\\{1,3\\}%\\]\\)" . ((lambda (tag)
;                                          (svg-progress-percent (substring tag 1 -2)))))
;      ("\\(\\[[0-9]+/[0-9]+\\]\\)" . ((lambda (tag)
;                                        (svg-progress-count (substring tag 1 -1)))))

      ;; TODO / DONE
      ("[ :]\\(TODO\\)[ :]" . ((lambda (tag) (svg-tag-make "TODO" :padding 0 :face 'org-todo :inverse t :margin 0))))
      ("[ :]\\(WIP\\)[ :]" . ((lambda (tag) (svg-tag-make "WIP" :padding 0 :face '+org-todo-active :inverse t :margin 0))))
      ("[ :]\\(WAIT\\)[ :]" . ((lambda (tag) (svg-tag-make "WAIT" :padding 0 :face '+org-todo-onhold :inverse t :margin 0))))
      ("[ :]\\(HOLD\\)[ :]" . ((lambda (tag) (svg-tag-make "HOLD" :padding 0 :face '+org-todo-onhold :inverse t :margin 0))))
      ("[ :]\\(CANCELLED\\)[ :]" . ((lambda (tag) (svg-tag-make "CANCELLED" :padding 0 :face '+org-todo-cancel :inverse t :margin 0))))
      ("[ :]\\(DONE\\)[ :]" . ((lambda (tag) (svg-tag-make "DONE" :padding 0 :face 'org-done :margin 0))))


      ;; Citation of the form [cite:@Knuth:1984]
      ("\\(\\[cite:@[A-Za-z]+:\\)" . ((lambda (tag)
                                        (svg-tag-make tag
                                                      :inverse t
                                                      :beg 7 :end -1
                                                      :crop-right t))))
      ("\\[cite:@[A-Za-z]+:\\([0-9]+\\]\\)" . ((lambda (tag)
                                              (svg-tag-make tag
                                                            :end -1
                                                            :crop-left t))))

      ;; Active date (with or without day name, with or without time)
      (,(format "[\t\n ]\\(<%s>\\)\\([\t\n ]\\|$\\)" date-re) .
       ((lambda (tag)
          (svg-tag-make tag :beg 1 :end -1 :margin 0 :padding 2))))
      (,(format "[\t\n ]\\(<%s \\)%s>\\([\t\n ]\\|$\\)" date-re day-time-re) .
       ((lambda (tag)
          (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :padding 2))))
      (,(format "[\t\n ]<%s \\(%s>\\)\\([\t\n ]\\|$\\)" date-re day-time-re) .
       ((lambda (tag)
          (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0))))

      ;; Inactive date (with or without day name, with or without time) require surrounding spaces or at end of line
       (,(format "[\t\n ]\\(\\[%s\\]\\)\\([\t\n ]\\|$\\)" date-re) .
        ((lambda (tag)
           (svg-tag-make tag :beg 1 :end -1 :margin 0 :padding 2 :face 'org-date))))
       (,(format "[\t\n ]\\(\\[%s \\)%s\\]\\([\t\n ]\\|$\\)" date-re day-time-re) .
        ((lambda (tag)
           (svg-tag-make tag :beg 1 :inverse nil :crop-right t :margin 0 :padding 2 :face 'org-date))))
       (,(format "[\t\n ]\\[%s \\(%s\\]\\)\\([\t\n ]\\|$\\)" date-re day-time-re) .
        ((lambda (tag)
           (svg-tag-make tag :end -1 :inverse t :crop-left t :margin 0 :face 'org-date))))

      ;; Date without parentheses or brackets, but need surrounding spaces or at end of line
      (,(format "[\t\n ]\\(%s\\)\\([\t\n ]\\|$\\)" date-re) .
       ((lambda (tag) (svg-tag-make tag :margin 0 :padding 0 :face 'org-date))))
       (,(format "[\t\n ]\\(%s \\)%s\\([\t\n ]\\|$\\)" date-re day-time-re) .
        ((lambda (tag)
           (svg-tag-make tag :inverse nil :crop-right t :margin 0 :padding 0 :face 'org-date))))
       (,(format "[\t\n ]%s \\(%s\\)\\([\t\n ]\\|$\\)" date-re day-time-re) .
        ((lambda (tag)
           (svg-tag-make tag :inverse t :crop-left t :margin 0 :face 'org-date))))
                     ))
  :hook
  (org-mode . svg-tag-mode)
  :config
  ;; Fix of font issue, https://github.com/rougier/svg-tag-mode/issues/38
  ;; for font-get, https://emacs.stackexchange.com/questions/62046/how-to-get-size-from-font-spec
  (plist-put svg-lib-style-default :font-family (font-get doom-font :family))
  (plist-put svg-lib-style-default :font-size (font-get doom-font :size))
)

(use-package! dired-open
  :after dired
  :init
  (if (featurep :system 'macos)
    (setq-default dired-open-default-open-program "open"))
  (if (featurep :system 'linux)
    (setq-default dired-open-default-open-program "xdg-open"))
)


;; ================================================
;; Evil related
;; ================================================
(use-package! evil
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-set-initial-state 'dired-mode 'normal)
  (evil-set-initial-state 'elpaca-ui-mode 'motion)
  (dolist (mode '(delve-mode
                  elfeed-search-mode
                  easy-hugo-mode
                  eshell-mode
                  git-rebase-mode
                  pyim-dict-manager-mode
                  vterm-mode
                  term-mode
                  calc-mode))
    (evil-set-initial-state mode 'emacs)))

;; ================================================
;; CJK input related
;;
;; - pyim
;; ================================================
(use-package! pyim
  :custom
  ;; Disable word search in current buffer.
  (pyim-candidates-search-buffer-p nil)
  :config
  (global-set-key (kbd "M-\\") 'pyim-convert-string-at-point)
  (setq pyim-page-tooltip '(posframe popup))
  (setq pyim-dcache-auto-update t)
  ;; pyim-shuangping is derived from xiaohe-shuangpin
  ;;(pyim-default-scheme 'pyim-shuangpin)
  ;; disable pyim-outcome-trigger for pyim-shuangpin otherwise cannot type o for 欧
  ;;(setq pyim-outcome-trigger nil)
  (pyim-default-scheme 'xiaohe-shuangpin)
  (setq pyim-page-length 9)
  ;; 中文使用全角标点，英文使用半角标点。
  (setq pyim-punctuation-translate-p '(auto yes no))
  ;; 加入个人词库，包括发音和词频。
  (setq my/personal-dicts
        '(("手动记录" . "manual_personal.pyim")
          ("搜狗导出 (2022-02-19)" . "sougou_out_2022_02_19.pyim")))
  (dolist (elem my/personal-dicts)
    (add-to-list 'pyim-dicts
        `(:name ,(car elem)
          :file ,(concat doom-private-dir "dict/" (cdr elem)))))
  ;; painless CN/EN switch by probe
  (defun my/pyim-probe-org-src-block ()
    "自定义探针, 进入 org-mode source block 之后自动切换到英文输入"
    (when (eq major-mode 'org-mode)
      (not (eq (org-in-src-block-p) nil)))
    )
  ;; auto-english 会根据之前的字符来判断是否切换到英文输入, 输入空格时自动切换到英文
  ;; 具体可用 describe-function 查看 docstring 来了解
  ;; 在 latex 块和源码块中全部为英文输入
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-auto-english
                  pyim-probe-org-latex-mode
                  my/pyim-probe-org-src-block
                  ; pyim-probe-org-structure-template
                  pyim-probe-program-mode))
  ;; 半角标点。主要情形是在行首使用 yasnippet 时有用
  (setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning pyim-probe-punctuation-after-punctuation))
  ;; Enable key-cord when pyim input is switched on,
  ;; This is very useful in evil-insert-mode, if
  ;; from https://emacs-china.org/t/pyim-key-chord-pyim/20633/12
  ;; 这比之前用的 pyim-self-insert-command 延迟更长一点，但泛用性更高
  (defun pyim--enable-key-chord-fun (orig key)
    (if (key-chord-lookup-key (vector 'key-chord key))
        (let ((result (key-chord-input-method key)))
          (if (eq (car result) 'key-chord)
              result
            (funcall orig key)))
      (funcall orig key)))
  (advice-add 'pyim-input-method :around #'pyim--enable-key-chord-fun)

  ;; orderless 支持拼音搜索候选项功能
  (defun my-orderless-regexp (orig-func component)
    (let ((result (funcall orig-func component)))
      (pyim-cregexp-build result)))
  (advice-add 'orderless-regexp :around #'my-orderless-regexp)
)


;; ================================================
;; Searching and completion
;;
;; - corfu
;; - consult-omni
;; - rg
;; ================================================
(use-package! corfu
  :config
  ; disable tab
  ; https://new.reddit.com/r/emacs/comments/xqk1xj/disable_tab_completion_in_corfu/
  (setq tab-always-indent t)
  (define-key corfu-map [tab] nil)
  (define-key corfu-map "\t" nil)
)

(use-package! consult-omni
  :after consult
  :config
  (require 'consult-omni-sources)
  ;; load all sources
  (consult-omni-sources-load-modules)
)

(use-package! rg
  :config
  (setq rg-keymap-prefix "\C-cg")
  (setq rg-ignore-case 'smart)
  (rg-enable-default-bindings)
  (map! :leader
        (:prefix-map "s"
          (:prefix-map ("g" . "ripgrep")
           :desc "rg"                  "g"  #'rg
           :desc "rg-menu"             "m"  #'rg-menu
           :desc "Cursor word"         "w"  #'rg-dwim
           :desc "Cursor Word (file)"  "f"  #'rg-dwim-current-file)))
)


;; ================================================
;; undo-tree
;; ================================================
(after! undo-tree
  (defun radian--undo-tree-suppress-undo-history-saved-message
        (undo-tree-save-history &rest args)
      (let ((inhibit-message t))
        (apply undo-tree-save-history args)))

  (defun radian--undo-tree-suppress-buffer-modified-message
      (undo-tree-load-history &rest args)
    (let ((inhibit-message t))
      (apply undo-tree-load-history args)))

  (advice-add #'undo-tree-load-history :around
              #'radian--undo-tree-suppress-buffer-modified-message))


;; ================================================
;; command-log-mode
;; ================================================
(use-package! command-log-mode)


;; ================================================
;; spelling checker and dictionary
;;
;; - ispell
;; - osx-dictionary
;; ================================================
(when (modulep! :checkers spell)
  (after! ispell
    (setq ispell-personal-dictionary (concat doom-private-dir "words"))
  )
)

(if IS-MAC
  (progn
    (use-package! osx-dictionary
      :bind
      (:map global-map
            ("C-c d" . osx-dictionary-search-word-at-point))
      :config
    )
  )
)


;; ================================================
;; Org-mode and related
;;
;; - org
;; - org-cliplink
;; - org-download
;; - org-appear
;; - org-archive
;; - org-recur
;; - org-ref
;; - org-bars
;; - org-drill
;; - org-present
;; - org-roam
;; - org-roam-bibtex
;; - org-noter
;; ================================================

;; adapted from https://emacs-china.org/t/topic/6601/4
(defun my/org-insert-image ()
  "Insert PNG image from the clipboard to the buffer by using =pngpaste= (macos) or =xclip= (linux)

The image will be created under 'images' directory in =org-directory=
with the name from user input. If image with the same name exists, the paste
will be stopped, but the link will still be created.
Note that =pngpaste=/=xclip= should be installed outside Emacs"
  (interactive)
  (let*
    (
     (cpcmd (pcase system-type
        ('darwin "pngpaste %s")
        ('gnu/linux "xclip -selection clipboard -t image/png -o > %s")
        ))
     (path (concat my/org-dir "/images/"))
     (fn (format "%s" (read-string "Enter image name (w/o png):")))
  	   (image-file (concat path fn ".png"))
    )
      (if (not (file-exists-p path)) (mkdir path))
      (if (file-exists-p image-file)
  	(message (format "Warning: found image %s.png in %s" fn path))
              (if cpcmd (shell-command (format cpcmd image-file))
  	              (message "Warning: clipboard -> file not suppored on this OS")
                ))
       (insert (format "#+name: fig:%s\n" fn))
       (insert "#+caption:\n")
       (insert ":IMAGE:\n")
       (insert "#+attr_org: :width 300\n")
       (insert "#+attr_latex: :width 0.6\\linewidth\n")
   (org-insert-link nil (concat "file:./images/" fn ".png") "")
       ;(insert "\n:PROPERTIES:\n:CREATED: " (format-time-string "[%Y-%m-%d %a %H:%M]") "\n:END:\n")
       (insert "\n:END:")
       ;; may add further elisp expression to suppress interaction for description
  ) ;; (org-display-inline-images) ;; inline显示图片
)

(defun my/org-sort-entries-todo-deadline-level ()
  "Key generation function for sorting org entries by TODO state, deadline and priority level"
  (interactive))


(defun my/org-goto-today-node (&optional PREPEND DTFORMAT)
  "go to the node with the time string of today in DTFORMAT as heading"
  (let* ((prepend  PREPEND)
         (dtformat (if DTFORMAT DTFORMAT "%Y-%m-%d %a"))
         (headline (format-time-string dtformat (current-time))))
    ;; following code copied from org-capture.el L983
    (widen)
    (goto-char (point-min)) ;;; get to the very beginning in the file
    (if (re-search-forward (format org-complex-heading-regexp-format
                                   (regexp-quote headline))
                           nil t)
        (progn
          (unless prepend (org-end-of-subtree) (insert "\n"))
          (insert "\n"))
      ;; head not found, create a new headline after the buffer if it is not empty
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "* " headline "\n"))))

(use-package! org
  :init
  (setq org-directory my/org-dir)
  (setq org-fold-core-style 'overlays
        org-fold-catch-invisible-edits 'show)
  :hook
  ((org-mode . org-indent-mode)
   (org-mode . visual-line-mode)
   (before-save . org-update-all-dblocks))
  :custom
  (org-link-descriptive nil)    ;; show the full link
  (org-export-use-babel nil)    ;; never (re)evaluate code blocks during export
  :bind
  (:map org-mode-map
        ("C-c l" . org-insert-link)
        ("C-c m l l" . org-insert-link) ; similar to org-clip
        ("C-c m i" . org-toggle-item)
        ("C-c m h" . org-toggle-heading)
        ("C-c m o" . org-set-property)
        ("C-c i" . my/org-insert-image)
        ("C-c C-i" . org-time-stamp-inactive)
        ("C-c e v" . (lambda () "make verbatim"       (interactive) (org-emphasize 61)))  ; =
        ("C-c e b" . (lambda () "make bold"           (interactive) (org-emphasize 42)))  ; *
        ("C-c e s" . (lambda () "make strike-through" (interactive) (org-emphasize 43)))  ; +
        ("C-c e i" . (lambda () "make italic"         (interactive) (org-emphasize 47)))  ; /
        ("C-c e u" . (lambda () "make underline"      (interactive) (org-emphasize 95)))  ; _
        ("C-c e c" . (lambda () "make code"           (interactive) (org-emphasize 126))) ; ~
      )
  :config
  (map! :map org-mode-map
        :nv "SPC d"     #'+org/remove-link
        :nv "SPC f A"   #'org-save-all-org-buffers
        :nv "SPC a t"   #'org-agenda-todo
        :nv "DEL"       #'org-mark-ring-goto
        :nv "M-j"       #'org-metadown
        :nv "M-k"       #'org-metaup
        :nv "M-n"       #'org-next-link
        :nv "M-p"       #'org-previous-link
        :nv "SPC v n"   #'org-narrow-to-subtree
        :nv "SPC v w"   #'widen
        )
  (setq org-src-tab-acts-natively nil)
  ;; hide emphasis markers, display when move to the line (enabled by org-appear)
  (setq org-hide-emphasis-markers nil)
  (setq org-archive-location (concat org-directory "/archive.org::* From %s"))
  ;; make org-archive-location a safe local variable whenever it is a string
  (put 'org-archive-location 'safe-local-variable #'stringp)
  (setq org-default-notes-file "inbox.org")
  (setq org-footnote-auto-adjust t)
  (setq org-tags-column -80)
  (setq org-extend-today-until 4)    ;; end of each day
  (setq org-pretty-entities t
        org-pretty-entities-include-sub-superscripts nil)
  (setq org-clock-persist 'history
        org-clock-idle-time 10
        org-clock-mode-line-total 'current  ; show current clocking time in mode-line
                                            ; 'auto for total; 'today
  )
  (setq org-enforce-todo-checkbox-dependencies t)
  ;; highlight latex environment
  ; (setq org-highlight-latex-and-related '(native script entities))
  ; (setq org-highlight-latex-and-related '(native script))

  ;; agenda
  (setq org-agenda-files (concat org-directory "/org-agenda.org")
        org-agenda-skip-scheduled-if-done 't
        org-agenda-dim-blocked-tasks nil
        org-agenda-inhibit-startup 't
        ; org-log-into-drawer 't
        org-log-done 'time
        org-agenda-use-tag-inheritance '(search timeline agenda)
        org-agenda-window-setup 'reorganize-frame
  )

  ;;; capture templates
  (setq org-capture-templates
        `(
          ("n" "Quick inbox note" plain
                (file+headline "inbox.org" "Inbox")
                "** TODO %u %?" :prepent t) ; Inbox as heading 1
          ("L"   "Language")
          ("Le"  "English")
          ("Lev" "English vocaburary" entry
               (file "english_vocabulary.org")
               "* %? :drill:\n** Examples :ignore:\n** Origin :ignore:")
          ("Lg"  "German" entry)
          ("Lgv" "German vocaburary" entry
               (file "german_vocabulary.org")
               "* %? :drill:\n:PROPERTIES:\n:DRILL_CARD_TYPE: hide1_firstmore\n:END:[||GE]: [||EN,CN]")
          ("l"  "Log")
          ("lj" "Journal" plain
                (file+function "Log/journal.org" my/org-goto-today-node)
                "%<%H:%M> %?")
;          ("lw" "Work" plain
;                (file+function "Log/work.org" my/org-goto-today-node)
;                "%<%H:%M> %?")
         ))
  ;; update TODO/checkbox count before finalizing the capture process
  ;; after-finalize-hook won't work, because the update should be done within the captured buffer
  (add-hook 'org-capture-before-finalize-hook '(lambda () (org-update-statistics-cookies t)))
  ;; org-table related.
  ;; commonly used constants for formulas
  (setq org-table-formula-constants
        '(("pi" . "3.14159265358")
          ("RY" . "13.60569301")
          ("HBAR" . "1.0545718e-34")
          ("EPS0" . "8.8541878128e−12")
          ("FSCA" . "0.0072973525664")
          ("KB" . "1.38064852e-23")
          ("CLIGHT" . "2.99792458e8")
          ("CE" . "1.6021766208e-19") ; electron charge
          ("BOHR2ANG" . "0.5291772")
          ("ANG2M" . "1e-10")
          ("EV2J" . "1.6021766208e-19")
          ("HA2EV" . "27.21138602")
          ("THZ2HA" . "1.519829846e-4") ; 10^12 h in Ha unit
          ))
  ;; default precision of formula results
  (plist-put org-calc-default-modes 'calc-internal-prec 20)
  (plist-put org-calc-default-modes 'calc-float-format '(float 12))

  ;; TODO keywords
  ; each state with ! is recorded as state change, @ require note
  (setq org-todo-keywords '((sequence "TODO(t)" "WIP(i)" "HOLD(h@)" "WAIT(w!)" "REV(r!)" "|" "DONE(d)" "CANCELLED(c!)"))
        org-todo-keyword-faces
          '(("REV" :foreground "#ff9933" :weight bold)
            ("WAIT" :foreground  "#9f7efe" :underline t)
            ("HOLD" :foreground  "black" :box t)
            ("WIP" :foreground "#0098dd" :weight bold)
            ("TODO" :foreground "#8c1400" :weight bold)
            ("DONE" :foreground "#50a14f")
            ("CANCELLED" :foreground "#ff6480" :strike-through t)
             )
  )
  ;; faces for org priority, from https://emacs.stackexchange.com/a/17405
  (setq org-priority-faces '((?A . (:foreground "red" :weight bold))
                             (?B . (:foreground "#C88523"))
                             (?C . (:foreground "blue"))
                             (?D . (:foreground "#4B7A47"))))

  ;; more link abbreviaiton
  (let ((link-abbrev-l `(("ytb" . "https://www.youtube.com/watch?v=%s")
                         ("mp" . "https://next-gen.materialsproject.org/materials/mp-%s")
                         ("isbn" . "http://books.google.com/books?vid=ISBN%s")
                         ("issn" . "http://books.google.com/books?vid=ISSN%s")
                         ("cnwiki" . "https://zh.wikipedia.org/zh-cn/%s")
                         ("zhihu" . "https://zhuanlan.zhihu.com/p/%s")
                         ("rnote" . ,(concat my/read-note-dir "/%s.org"))
                         ("SO" . "https://stackoverflow.com/questions/%s")
                         ("arxiv" . "https://arxiv.org/abs/%s"))))
    (dolist (elem link-abbrev-l) (add-to-list 'org-link-abbrev-alist elem)))

  ;; org-babel related
  (setq org-babel-results-keyword "results")
  (setq org-confirm-babel-evaluate nil)  ;; do not need to confirm when evaluate

  ;; disable some tags from inheriting to descendants
  (dolist (elem '("noter" "Reference" "Book" "bookrev" "drill"))
    (add-to-list 'org-tags-exclude-from-inheritance elem))
)

(after! org
  ;; create ID if there is no CUSTOM_ID
  (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)

  ;; add a CUSTOM_ID according to the name of heading
  ;; adapted from https://writequit.org/articles/emacs-org-mode-generate-ids.html
  ;; the original one use id from org-id-new, which creates a non-human-readable id
  (defun my/org-custom-id-get (&optional pom create)
    "Get the CUSTOM_ID property of the entry at point-or-marker POM.
     If POM is nil, refer to the entry at point. If the entry does
     not have an CUSTOM_ID, the function returns nil. However, when
     CREATE is non nil, create a CUSTOM_ID if none is present.
     In any case, the CUSTOM_ID of the entry is returned."
    (interactive)
    (org-with-point-at pom
      (let ((id (org-entry-get nil "CUSTOM_ID")))
        (cond
         ((and id (stringp id) (string-match "\\S-" id))
          id)
         (create
          ;; naively remove emphasis
          (setq id (replace-regexp-in-string "[~*+]" "" (org-entry-get nil "ITEM")))
          (org-entry-put pom "CUSTOM_ID" id)
          (org-id-add-location id (buffer-file-name (buffer-base-buffer)))
          id)))))
  (map! :leader
        (:prefix-map ("e" . "extra-my")
          (:prefix-map ("p" . "property")
           :desc "Custom ID"  "i"  '(lambda () (interactive) (my/org-custom-id-get nil 'create)))))
)

(use-package! org-cliplink
  :after org
  :bind
  (:map org-mode-map
        ("C-c m l c" . org-cliplink)))

(use-package! org-appear
  :hook
  (org-mode . org-appear-mode)
  :config
  (setq org-appear-autolinks t      ;; not working right now in Doom, which is a bit annoying
        org-appear-autoentities t
        org-appear-inside-latex t)  ;; show original/unprettified symbols in latex envrionment
)

(use-package! org-download
  :init
  (put 'org-download-image-dir 'safe-local-variable #'stringp)
  :custom
  ; do not use subdirectory of heading
  (org-download-heading-lvl nil)
  :config
  ;; org-download-timestamp is changed in modules/lang/org/contrib/dragndrop.el, so :custom didn't work
  (setq org-download-timestamp "%Y%m%d-%H%M%S_")
  (setq org-download-method 'directory)
  ; from https://emacs.stackexchange.com/questions/75983/how-to-format-org-downloads-image-saving-directorys
  (defun my-org-download-set-dir ()
    "Set `org-download-image-dir` to the directory of the current buffer's file."
    (if buffer-file-name
      (setq-local org-download-image-dir (concat "./assets/" (file-name-base buffer-file-name)))))
  (add-hook 'org-mode-hook 'my-org-download-set-dir)
  (add-hook 'dired-mode-hook 'org-download-enable)

  ;; https://www.reddit.com/r/emacs/comments/145a3wk/orgdownload_doesnt_add_file_url_to_image_links
  (setq org-download-link-format "[[file:%s]]\n" org-download-abbreviate-filename-function #'file-relative-name)
  (setq org-download-link-format-function #'org-download-link-format-function-default)

  ;; TODO advice the org-download-yank to check if current kill is a file name
  (defadvice org-download-yank (around check-kill-filename activate)
    ;; copy current kill
    (let ((k (current-kill 0))
          (is-kill-filename nil))
      ;; check if current kill is a file path
      ad-do-it
      ;; restore the original kill-ring
      (if is-kill-filename
        ())
    )
  )

  ;; TODO convert download link to file link
  ;; see https://vxlabs.com/2020/07/25/emacs-lisp-function-convert-attachment-to-file/
)

(after! org-archive
  (setq org-archive-mark-done t) ; change subtree state to DONE when archived
)

(use-package! org-recur
  :hook ((org-mode . org-recur-mode)
         (org-agenda-mode . org-recur-agenda-mode))
  :config
  (define-key org-recur-mode-map (kbd "C-c d") 'org-recur-finish)

  ;; Rebind the 'd' key in org-agenda (default: `org-agenda-day-view').
  (define-key org-recur-agenda-mode-map (kbd "d") 'org-recur-finish)
  (define-key org-recur-agenda-mode-map (kbd "C-c d") 'org-recur-finish)

  (setq org-recur-finish-done t
        org-recur-finish-archive t))


(use-package! org-bars
  :after org
  :commands org-bars-mode
  ;; disabled due to blinking when using magit
  ; :hook
  ; (org-mode . org-bars-mode)
)

(use-package! org-drill
  :after org
  :commands org-drill org-drill-resume
  :config
  (setq org-drill-scope 'directory)
  (setq org-drill-spaced-repetition-algorithm 'sm2)
  (setq org-drill-adjust-intervals-for-early-and-late-repetitions-p t)
  (setq org-drill-add-random-noise-to-intervals-p t)
  ;; reduce space, repeat more
  (setq org-drill-learn-fraction 0.30)
)

(use-package! org-tree-slide
  :after org
  :bind
  (:map org-tree-slide-mode-map
        ("<f9>" . 'org-tree-slide-move-previous-tree)
        ("<f10>" . 'org-tree-slide-move-next-tree)
        ("<f11>" . 'org-tree-slide-content))
  (:map org-mode-map
        ("<f8>" . 'org-tree-slide-mode)
        ("S-<f8>" . 'org-tree-slide-skip-done-toggle))
  :config
  (setq org-tree-slide-skip-outline-level 4)
  (org-tree-slide-narrowing-control-profile)
  (setq org-tree-slide-skip-done nil)
)

(use-package! org-roam
  :after org-roam
  :init
  (setq org-roam-directory org-directory
        org-roam-db-location (expand-file-name "org-roam.db" org-directory)
        org-roam-index-file "index.org"
        org-roam-graph-extra-config '(("overlap" . "false")) ; man dot for attributes setup
        )
  :bind
  (:map org-mode-map
        (("C-c r R" . org-roam-buffer-toggle)
         ("C-c r ." . org-roam-node-find)
         ("C-c r L" . org-roam-store-link)
         ("C-c r a" . org-roam-alias-add)
         ("C-c r u" . org-roam-unlinked-references)
         ("C-c r r" . org-roam-find-ref)
         ("C-c r d" . org-roam-find-directory)
         ("C-c r j" . org-roam-jump-to-index)
         ("C-c r b" . org-roam-switch-to-buffer)
         ("C-c r n" . orb-note-actions)
         ("C-c r i" . org-roam-node-insert)
         )
  )
  :config
  (add-to-list 'display-buffer-alist
                '("\\*org-roam\\*"
                  (display-buffer-in-direction)
                  (direction . right)
                  (window-width . 0.33)
                  (window-height . fit-window-to-buffer)))
  (setq org-roam-extract-new-file-path "${slug}.org")
  (org-roam-db-autosync-mode)
  (setq org-roam-node-display-template
        ; (format "${doom-hierarchy:*} %s %s"
        ;         (propertize "${doom-type:12}" 'face 'font-lock-keyword-face)
        ;         (propertize "${doom-tags:32}" 'face 'org-tag)))
        (format "${doom-hierarchy:*} %s"
                (propertize "${doom-tags:50}" 'face 'org-tag)))
  (setq org-roam-capture-templates
        `(
          ("d" "default quick note" plain "%?"
           :if-new (file+head ,(expand-file-name "${slug}.org" my/org-roam-inbox)
                              "#+title: ${title}\n#+startup: content\n#+created: %U\n")
           :unnarrowed t)
          ("l" "Note for latex export" plain "%?"
           :if-new (file+head ,(expand-file-name "${slug}.org" my/org-roam-inbox)
                               "#+title: ${title}
#+startup: content
#+created: %U\n
#+latex_compiler: pdflatex
#+latex_class: article
#+latex_header: \\usepackage[hmargin=1.0in, top=1.0in, bottom=0.7in]{geometry}
#+latex_header: \\usepackage[maxnames=3,style=nature,date=year,url=false,isbn=false,articletitle=false]{biblatex}
#+latex_header: \\addbibresource{etc/bibliography.bib}
# include commands preset
#+setupfile: lh_symbols.org
#+setupfile: lh_biblatex.org
#+options: toc:nil tags:nil title:t email:nil author:t date:t

# a link-colored toc
#+latex: {\\hypersetup{linkcolor=Blue}\\tableofcontents}
#+latex: \\clearpage\n\n* References\n#+latex: \\printbibliography[heading=none]")
           :unnarrowed t)
          ("b" "non-STEM book note" plain "%?"
           :if-new (file+head ,(expand-file-name "${slug}.org" my/read-note-dir)
           "#+title: ${title}\n#+startup: overview\n#+created: %U\n#+options: toc:nil email:t f:t\n")
           :unnarrowed t)
          ("t" "talk note" plain "%?"
           :if-new (file+head ,(expand-file-name "%<%Y%m%d>-${slug}.org" my/talk-note-dir)
           "#+title: ${title}\n#+startup: overview\n#+created: %U\n#+options: toc:nil email:t f:t\n")
           :unnarrowed t)
         ))
)

(use-package! org-roam-bibtex
  :after org
  :hook
  (org-mode . org-roam-bibtex-mode)
  :config
  (require 'org-ref)
  ; (setq org-ref-notes-function 'orb-edit-notes)
  (setq orb-preformat-keywords
    '("citekey"  "title" "author-or-editor" "date" "doi" "file" "journaltitle" "volume" "pages"))
  ; (setq orb-roam-ref-format "org-ref-v3")
  (advice-add 'bibtex-completion-candidates :filter-return 'reverse)
  ; anystyle-related
  (setq orb-autokey-format "%A*[1]%y*"
        orb-pdf-scrapper-export-fields '("author" "journal" "date" "volume" "pages" "title"))
  (add-to-list 'org-roam-capture-templates
          `("r" "reference" plain "%?"
           :if-new (file+head
           ,(expand-file-name "note-${citekey}.org" my/literature-note-dir)
           ":PROPERTIES:
:TITLE: ${title}
:AUTHOR: ${author-or-editor}
:JOURNAL: ${journaltitle}
:DATE: ${date}
:VOLUME: ${volume}
:PAGES: ${pages}
:DOI: [[doi:%(replace-regexp-in-string \" \" \"\" \"${doi}\")]]
:END:
#+title: ${citekey}: ${title}
#+startup: content
#+created: %U

* Summary

* Notes :noter:
:PROPERTIES:
:NOTER_DOCUMENT: ${file}
:END:
")
           :unnarrowed t))
  ;;; connect to citar by citar-org-roam
  (require 'citar-org-roam)
  (citar-register-notes-source
   'orb-citar-source (list :name "Org-Roam Notes"
          :category 'org-roam-node
          :items #'citar-org-roam--get-candidates
          :hasitems #'citar-org-roam-has-notes
          :open #'citar-org-roam-open-note
          :create #'orb-citar-edit-note
          :annotate #'citar-org-roam--annotate))
  (setq citar-notes-source 'orb-citar-source)
  (setq citar-org-roam-capture-template-key "r")
)

(use-package! org-noter
  :after (:any org pdf-view)
  :bind
  ("C-c n N" . org-noter)
  ("C-c n s" . my-org-noter-extract-annotation-pdf)
  :config
  (setq
   ;; The WM can handle splits
   ;; org-noter-notes-window-location 'other-frame
   ;; Please stop opening frames
   org-noter-always-create-frame nil
   ;; I want to see the whole file
   org-noter-hide-other nil
   ;; Everything is relative to the main notes file
   org-noter-notes-search-path (list org-directory)
   ;org-noter-set-notes-window-behavior 'scroll
   ;; split fraction. default (0.5 . 0.5). slightly larger on vertical
   ;; TODO: may adapt according to the size of display
   org-noter-doc-split-fraction '(0.58 . 0.5)
  )
  (defun my-org-noter-extract-annotation-pdf ()
    "Create notes skeleton with the PDF annotations.
  Only available with PDF Tools."
    (interactive)
    (org-noter--with-valid-session
     (cond
     ( (eq (org-noter--session-doc-mode session) 'pdf-view-mode)
       (let* ((ast (org-noter--parse-root))
              (top-level (org-element-property :level ast))
              output-data)
         (with-current-buffer (org-noter--session-doc-buffer session)
             (let ((chosen-annots (list 'highlight
                                        'underline
                                        'squiggly
                                        'text
                                        'strike-out
                                       ))
                   insert-contents pages-with-links)

               (setq insert-contents t)

               (dolist (item (pdf-info-getannots))
                 (let* ((type  (alist-get 'type item))
                        (page  (alist-get 'page item))
                        (edges (or (org-noter--pdf-tools-edges-to-region (alist-get 'markup-edges item))
                                   (alist-get 'edges item)))
                        (top (nth 1 edges))
                        (item-subject (alist-get 'subject item))
                        (item-contents (alist-get 'contents item))
                        name contents)
                   (when (and (memq type chosen-annots) (> page 0))
                     (if (eq type 'link)
                         (cl-pushnew page pages-with-links)
                       (setq name (cond ((eq type 'highlight)  "Highlight")
                                        ((eq type 'underline)  "Underline")
                                        ((eq type 'squiggly)   "Squiggly")
                                        ((eq type 'text)       "Text note")
                                        ((eq type 'strike-out) "Strikeout")))
                       (when insert-contents
                         (setq contents (cons (pdf-info-gettext page edges)
                                              (and (or (and item-subject (> (length item-subject) 0))
                                                       (and item-contents (> (length item-contents) 0)))
                                                   (concat (or item-subject "")
                                                           (if (and item-subject item-contents) "\n" "")
                                                           (or item-contents ""))))))

                       (push (vector (format "%s on page %d" name page) (cons page top) 'inside contents)
                             output-data)
             )
             )))
         )

           (when output-data
             (setq output-data
                   (sort output-data
                         (lambda (e1 e2)
                           (or (not (aref e1 1))
                               (and (aref e2 1)
                                    (org-noter--compare-locations '< (aref e1 1) (aref e2 1)))))))
       )
       )
       ; print out
         (with-current-buffer (org-noter--session-notes-buffer session)
           ;; NOTE(nox): org-with-wide-buffer can't be used because we want to reset the
           ;; narrow region to include the new headings
           (widen)
           (save-excursion
             (goto-char (org-element-property :end ast))

             (let (last-absolute-level
                   title location relative-level contents
                   level)
               (dolist (data output-data)
                 (setq title          (aref data 0)
                       location       (aref data 1)
                       ;relative-level (aref data 2)
                       contents       (aref data 3))
                 ;(if (symbolp relative-level)
                 ;    (setq level (1+ last-absolute-level))
                 ;  (setq last-absolute-level (+ top-level relative-level)
                 ;        level last-absolute-level))
                 (setq level (1+ top-level))

                 ;;; add by minyez
                 ;(org-noter--insert-heading level title)
                 (when (car contents)
                   (org-noter--insert-heading level (car contents)))
                 ;; end add by minyez

                 (when location
                   (org-entry-put nil org-noter-property-note-location (org-noter--pretty-print-location location)))

                 (when org-noter-doc-property-in-notes
                   (org-entry-put nil org-noter-property-doc-file (org-noter--session-property-text session))
                   (org-entry-put nil org-noter--property-auto-save-last-location "nil"))

                 (when title
                   (org-noter--insert-heading (1+ level) title))
                 (when (cdr contents)
                   (org-noter--insert-heading (1+ level) "Comment")
                   (insert (cdr contents)))
          ))

             (setq ast (org-noter--parse-root))
             (org-noter--narrow-to-root ast)
             (goto-char (org-element-property :begin ast))
             (outline-hide-subtree)
             (org-show-children 1)
       )
       )
     )
     )
     (t (user-error "This command is only supported on PDF Tools.")))))
)

;;; org-roam-ui
;; (use-package! websocket
;;     :after org-roam)
;; (use-package! org-roam-ui
;;     :after org-roam ;; or :after org
;; ;;         normally we'd recommend hooking orui after org-roam, but since org-roam does not have
;; ;;         a hookable mode anymore, you're advised to pick something yourself
;; ;;         if you don't care about startup time, use
;; ;;  :hook (after-init . org-roam-ui-mode)
;;     :config
;;     (setq org-roam-ui-sync-theme t
;;           org-roam-ui-follow t
;;           org-roam-ui-update-on-save t
;;           org-roam-ui-open-on-start t)
;;     (setq org-roam-ui-ref-title-template "%^{citekey} %^{title}"))

;; ================================================
;; References and bibliography
;;
;; In my case there are used in combination with org and org-roam
;;
;; - bibtex-completion
;; - org-ref
;; - org-cite (oc)
;; - citar
;; - citeproc
;; ================================================

;; for org-ref completion
(use-package! bibtex-completion
  :config
  (setq bibtex-completion-notes-path my/literature-note-dir
        bibtex-completion-bibliography (list my/bibtex-file)
        bibtex-completion-pdf-field "file"
        bibtex-completion-additional-search-fields '(keywords journaltitle)
  )
)

(use-package! oc
  :after org
  :custom
  (org-cite-global-bibliography (list my/bibtex-file)))

(use-package! org-ref
  :config
  (define-key org-mode-map (kbd "C-c ]") 'org-ref-insert-link)
)

(use-package! citar
  :custom
  (citar-bibliography (list my/bibtex-file))
  (citar-notes-paths (list my/literature-note-dir))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :config
  ;; use citeproc to generate in-text reference
  (setq citar-format-reference-function 'citar-citeproc-format-reference
        citar-citeproc-csl-styles-dir (expand-file-name "csl" doom-private-dir)
        citar-citeproc-csl-style "aps-modified.csl")
)

(use-package! citeproc)


;; ================================================
;; org export setup
;; - ox
;; - ox-pandoc
;; - ox-extra
;; - ox-latex
;; - ox-beamer
;; - ox-gfm
;; - ox-hugo
;; - ox-icalendar
;; ================================================

(after! ox
  (setq org-export-with-broken-links t)  ; t/'mark/nil
  ; don't ask if treat compile-command safe if it is a string
  (put 'compile-command 'safe-local-variable #'stringp)
  ; 'mark leads to a marker in exported content with broken links
  ; nil will abort export with broken links
  ; t will continue anyway


  (defun my/org-pandoc-convert-org-ref-link-to-org-cite (BACKEND &optional subtreep)
    "Hook function to convert org-ref cite link to org-cite cite link.

Currently it uses a naive implementation by `re-search-forward' for the conversion.
Caveats:
- only work with pandoc backend
- only handle cite and fullcite
- cannot handle notes"
    (if (not (equal BACKEND 'pandoc)) ()
      (goto-char (point-min))
      (while (re-search-forward
               "\\([=\~]\\)?\\[?\\[?\\(cite\\|fullcite\\):&?\\([^] @\t\r\n]+\\)\\]?\\]?\\([=\~]\\)?"
               nil t)
        ; do not convert those in a source code block or inline code
        (unless (or (org-in-src-block-p)
                    (string= (match-string 1) "=") (string= (match-string 1) "~")
                    (string= (match-string 4) "=") (string= (match-string 4) "~"))
          (let ((keys  ; handle multiple keys
                  (replace-regexp-in-string "[,;]&?" ";@" (match-string 3))))
            (cl-case (intern (match-string 2))
                     (fullcite
                       ; bare to remove the bracket
                       ; https://www.miskatonic.org/2024/01/08/org-citations-basic
                       (replace-match (format "[cite/bibentry/bare:@%s]" keys)))
                     (t
                       (replace-match (format "[\\2:@%s]" keys)))))))))

  (defun my/expand-org-id-link-to-relative-path (BACKEND &optional subtreep)
    "Expand all org-id links and replace with relative file path"
    (goto-char (point-min))
    (while (re-search-forward org-link-bracket-re nil t)
      (unless (org-in-src-block-p)
        (let* ((link (match-string-no-properties 1))
               (desc (match-string-no-properties 2))
               (prefix (downcase (substring link 0 3)))
               (id (substring link 3))
               )
          (when (and (equal prefix "id:"))
            (let* ((fn-loc
                     ; required to use save-match-data macro, since
                     ; org-id-find-id-in-file will use string-match;
                     ; also split-string.
                     (save-match-data
                       (org-id-find-id-in-file id (org-id-find-id-file id))))
                   (rela-fn (if fn-loc
                                (file-relative-name
                                  (car fn-loc)
                                  (file-name-directory (buffer-file-name)))
                              nil)))
              (if rela-fn
                  (replace-match (org-link-make-string (format "file:%s" rela-fn) desc))
                (warn "fail to find id %s" id)
                (if desc
                    (replace-match desc)
                  (replace-match (format "=broken-id:%s=" id))))))))))

  (defun my/replace-org-file-org-with-export-output (BACKEND &optional subtreep)
    "Replace org-mode link to org-mode file by export file name

  For exampel, [[file:filename.org][description]] becomes [[file:filename.EXPORT_BACKEND][description]]"
    (let ((output-ext
            (if (equal BACKEND 'pandoc)
                (symbol-name (or (assoc-default org-pandoc-format org-pandoc-extensions)
                                 org-pandoc-format))
              (symbol-name BACKEND))))
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (unless (org-in-src-block-p)
          (let* ((link (match-string-no-properties 1))
                 (desc (match-string-no-properties 2))
                 (prefix (downcase (substring link 0 5)))
                 (target (substring link 5))
                 (ext (file-name-extension target))
                 )
            (when (and (equal prefix "file:") (if ext (equal (downcase ext) "org")))
              (let ((output-path
                      (save-match-data
                        (if (find-file-noselect target)
                            (with-current-buffer (find-file-noselect target)
                              (org-export-output-file-name (format ".%s" output-ext) subtreep))
                            ))))
              ;; (message (match-string-no-properties 0))
                (if output-path
                  (replace-match (org-link-make-string (format "file:%s" output-path) desc))))))))))

  (defun my/trim-jekyll-root-in-file-link (BACKEND &optional subtreep)
    "trim Jekyll root directory in file link"
    (when (and (equal BACKEND 'pandoc) my/jekyll-root-directory)
      (let ((root (expand-file-name my/jekyll-root-directory)))
        (goto-char (point-min))
        (while (re-search-forward org-link-bracket-re nil t)
          (unless (org-in-src-block-p)
            (let* ((link (match-string-no-properties 1))
                   (prefix (downcase (substring link 0 5))))
              (when (equal prefix "file:")
                (let* ((target (substring link 5))
                       (desc (match-string-no-properties 2))
                       (full (expand-file-name target))
                       (trimed (save-match-data
                                 (if (string-match root full)
                                     (replace-match "" t t full)
                                   full))))
                  (replace-match (org-link-make-string (format "file:%s" trimed) desc))))))))))

  (add-to-list 'org-export-before-parsing-functions 'my/trim-jekyll-root-in-file-link)
  (add-to-list 'org-export-before-parsing-functions 'my/replace-org-file-org-with-export-output)
  (add-to-list 'org-export-before-parsing-functions 'my/expand-org-id-link-to-relative-path)
  (add-to-list 'org-export-before-parsing-functions 'my/org-pandoc-convert-org-ref-link-to-org-cite)
)

(use-package! ox-pandoc
  :init
  (setq org-pandoc-options-for-markdown '((standalone . t) (preserve-tabs . t)))
  (setq org-pandoc-format-extensions
        '(markdown-link_attributes-bracketed_spans-simple_tables-raw_tex-raw_attribute)))

(use-package! ox-extra
  :after ox
  :config
  (ox-extras-activate '(ignore-headlines)))

;; (use-package! ox-gfm
;;   :after ox)

;; (use-package! ox-hugo
;;   :after ox)

(use-package! ox-latex
  :bind
  ("C-c x l" . org-latex-export-to-latex)
  ("C-c x o" . org-latex-export-to-pdf)
  :config

  ;; use latexmk to automate toolchain
  (setq org-latex-pdf-process '("latexmk -latexoption=\"-interaction=nonstopmode -shell-escape -file-line-error -synctex=1\" -pdf -pdflatex=%latex -bibtex -f %f"))

  ;; prefer custom label
  (setq org-latex-prefer-user-labels t)
  (put 'org-latex-compiler 'safe-local-variable #'stringp)

  ;; remove default hyperset with author names included
  ;; for local variable setup, use for each file
  ;; # -*- org-latex-hyperref-template: nil; -*-

  ;; default packages to load right after documentclass at first
  (setq org-latex-default-packages-alist
    '(
      ("" "amsmath" t) ; to avoid iint and iiint error
      ("" "amssymb" t)
      ("" "wasysym" t) ; last to avoid iint and iint error
      ("AUTO" "inputenc"  t ("pdflatex"))
      ("T1"   "fontenc"   t ("pdflatex"))
      (""     "CJKutf8"   t ("pdflatex"))
      (""     "ifxetex"   nil)
      (""     "xeCJK"     nil ("xelatex", "xetex"))
      (""     "fontspec"  nil ("xelatex", "xetex", "lualatex", "luatex"))
      (""     "microtype"  nil) ; for typographic refinements
      (""     "graphicx"  t)
      (""     "xcolor"  t)
      ; ("nottoc,numbib"     "tocbibind" nil)
      ; corresponding to "setq org-latex-listings t"
      ; (""           "listings"   nil)
      ; but minted is better to use
      ("newfloat,cache=true"   "minted"   nil)
      (""     "grffile"   t)
      ; (""     "longtable" nil)
      (""     "mdframed" nil)   ; for creating blockquote
      (""     "float" nil)
      (""     "wrapfig"   nil)
      (""     "subfig"    nil)
      (""     "rotating"  nil)
      ("normalem" "ulem"  t)    ; strikeout
      (""     "textcomp"  t)
      (""     "capt-of"   nil)
      ("font={small},skip=1pt"     "caption"   nil)
      (""     "parskip"   nil)  ; better paragraph spacing
      (""     "booktabs"   nil) ; better table
	 )
  )

  ; packages to load at last
  (setq org-latex-packages-alist
    '(
      ; hyperref and cleverf should be the last packages to load
      ("" "hyperref"  nil)
      ("" "cleveref"  nil)
     )
  )

  ;; use minted as the default code block renderer
  (setq org-latex-listings 'minted
        org-latex-minted-options '(
          ("bgcolor" "bg")
          ("breaklines" "true")
          ("autogobble" "true")
          ;; ("fontsize" "\\small") ;; adjust by header \setminted{fontsize=\small}
          )
  )

  ; customized classes for latex export
  (setq org-latex-classes
               `(
                ("article"
                  ,(s-join "\n"
                      `(
                         ,my/org-latex-classes-common-header-passoptions
                         "\\documentclass[11pt,a4paper]{article}"
                         "[DEFAULT-PACKAGES]"
                         ,my/org-latex-classes-common-header-after-default-pkgs
                         "[EXTRA]"
                         "[PACKAGES]"))
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                ("book"
                  ,(s-join "\n"
                      `(
                         ,my/org-latex-classes-common-header-passoptions
                         "\\documentclass[11pt,a4paper,titlepage]{book}\n"
                         "[DEFAULT-PACKAGES]"
                         ,my/org-latex-classes-common-header-after-default-pkgs
                         "[EXTRA]"
                         "[PACKAGES]"))
                  ("\\chapter{%s}" . "\\chaper*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                ("report"
                  ,(s-join "\n"
                      `(
                         ,my/org-latex-classes-common-header-passoptions
                         "\\documentclass[11pt]{report}\n"
                         "[DEFAULT-PACKAGES]"
                         ,my/org-latex-classes-common-header-after-default-pkgs
                         "[EXTRA]"
                         "[PACKAGES]"))
                  ("\\chapter{%s}" . "\\chaper*{%s}")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                ("beamer"
                  ,(s-join "\n"
                      `(
                         ,my/org-latex-classes-common-header-passoptions
                         "\\documentclass[ignorenonframetext,presentation]{beamer}"
                         "\\usepackage{beamerseminar}"
                         "[DEFAULT-PACKAGES]"
                         "% space between caption and table"
                         "\\captionsetup[table]{belowskip=-6pt}"
                         ,my/org-latex-classes-common-header-after-default-pkgs
                         "[EXTRA]"
                         "[PACKAGES]"))
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}"))
                ("beamerarticle"
                  ,(s-join "\n"
                      `(
                         ,my/org-latex-classes-common-header-passoptions
                         "\\documentclass[11pt,a4paper]{article}"
                         "\\usepackage{beamerarticle}"
                         "\\usepackage{beamerseminar}"
                         "[DEFAULT-PACKAGES]"
                         "\\usepackage{physics}"
                         ,my/org-latex-classes-common-header-after-default-pkgs
                         "[EXTRA]"
                         "[PACKAGES]"))
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                  ("\\paragraph{%s}" . "\\paragraph*{%s}")
                  ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                )
  )
)

(use-package! ox-beamer
  :bind
  ("C-c x b" . org-beamer-export-to-latex)
  ("C-c x O" . org-beamer-export-to-pdf)
  :config
  ; default 3rd level heading as frame.
  ; 1st and 2nd are sec and subsec
  (setq org-beamer-frame-level 3)
)

(use-package! ox-icalendar
  :after ox
  :custom
  ; calendar name of org-agenda combined export
  (org-icalendar-combined-name        "MYZ org agenda")
  (org-icalendar-combined-agenda-file (expand-file-name "org.ics" org-directory))
  (org-icalendar-combined-description "Calendar entries from Emacs org-mode")
  ; before finding the way to set alarm per entry, use a global alarm time
  (org-icalendar-alarm-time 5)
  ; honor noexport tag when exporting
  (org-icalendar-exclude-tags (list "noexport"))
  ; NOTE: Timestamp is also added to the summary, which is redundant in icalendar
  ;       A hook might be useful to remove before the export.
  )

;; ================================================
;; PDF tools configuration
;;
;; - pdf-view
;; - pdf-tools
;; ================================================
(after! pdf-view
  (map! :map pdf-view-mode-map
        :nv "z g"        #'pdf-view-goto-page
        :nv "z r"        #'image-rotate       ;; rotate the page (defined in image.el)
        :nv "SPC a a l"  #'pdf-annot-list-annotations
        :nv "SPC a a h"  #'pdf-annot-add-highlight-markup-annotation
        :nv "SPC a a u"  #'pdf-annot-add-underline-markup-annotation
        )
)

(after! pdf-tools
  :config
  ;; activate when opening pdf, otherwise the evil keybindings will not work
  (pdf-loader-install)
)

;; ================================================
;; RSS
;;
;; - elfeed
;; - elfeed-org
;; ================================================
(after! elfeed
  ;; override default 2-week-ago filter by doom emacs
  (setq elfeed-search-filter "")
)

(use-package! elfeed-org
  :after org
  :preface
  (setq rmh-elfeed-org-files `(,(expand-file-name "etc/elfeed.org" org-directory)))
)


;; ================================================
;; Project management and version control
;;
;; - magit
;; - git-gutter
;; - projectile
;; ================================================
(use-package! magit
  :config
  ;; resolve "cannot switch buffers in a dedicated window" when opening the diff of a commit
  ;; while treemac is active, from https://github.com/magit/magit/issues/4034
  (defun my/magit-display-buffer (buffer)
    (if (and git-commit-mode
             (with-current-buffer buffer
               (derived-mode-p 'magit-diff-mode)))
        (display-buffer buffer '((display-buffer-pop-up-window
                                  display-buffer-use-some-window
                                  display-buffer-below-selected)
                                 (inhibit-same-window . t)))
      (magit-display-buffer-traditional buffer)))

  (setq magit-display-buffer-function #'my/magit-display-buffer)
)

;; do not ask confirm for stage/revert
(after! git-gutter
  (setq git-gutter:ask-p nil))

(use-package! projectile
  :custom
  (projectile-project-search-path (list "~/projects"))
  ;; :config
  ;; (map! :leader
  ;;       (:prefix-map "p"
  ;;        :desc "Switch to project"  "p"  'projectile-switch-project))
)


;; ================================================
;; Blogging
;;
;; - easy-hugo
;; ================================================
;; (use-package! easy-hugo
;;   :config
;;   (easy-hugo-enable-menu)
;; )
