;;; config-ui.el -*- lexical-binding: t; -*-

(display-time-mode 1) ;; always show time
;; relative visual line, usually better than relative with folded blocks
(setq display-line-numbers-type 'visual)
;; calculate necessary width to correctly show all line numbers, since 28.1
(setq display-line-numbers-width-start t)

(let ((font "Sarasa Fixed Slab SC Nerd Font")
      (size 16))
  (setq doom-font (font-spec :family font :size size)
        doom-variable-pitch-font (font-spec :family font :size size)
        ;; disable unicode font size set to resolve too large icon in dashboard/treemacs
        ;;doom-unicode-font (font-spec :family font :size size)
        doom-big-font (font-spec :family font :size (+ size 4))))

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

(provide 'config-ui)
;;; config-ui.el ends here
