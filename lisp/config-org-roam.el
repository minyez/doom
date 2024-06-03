;;; config-org-roam.el -*- lexical-binding: t; -*-

(use-package! org-roam
  :after org-roam
  :init
  (setq org-roam-directory org-directory
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
#+latex_header: \\addbibresource{~/database/bibliography.bib}
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
"
           )
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

(provide 'config-org-roam)
;;; config-org-roam.el ends here
