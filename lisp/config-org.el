;;; config-org.el -*- lexical-binding: t; -*-

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

(map!
  :nv "SPC m u" #'outline-up-heading)

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

(provide 'config-org)
;;; config-org.el ends here
