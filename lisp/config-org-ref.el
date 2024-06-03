;;; config-org-ref.el -*- lexical-binding: t; -*-
(use-package! bibtex-completion
  :config
  (setq bibtex-completion-notes-path my/literature-note-dir
        bibtex-completion-bibliography (list my/bibtex-file)
        bibtex-completion-pdf-field "file"
        bibtex-completion-additional-search-fields '(keywords journaltitle)
  )
)

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

(provide 'config-org-ref)
;;; config-org-ref.el ends here
