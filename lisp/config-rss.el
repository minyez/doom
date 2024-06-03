;;; config-rss.el -*- lexical-binding: t; -*-

(after! elfeed
  ;; override default 2-week-ago filter by doom emacs
  (setq elfeed-search-filter "")
)

(use-package! elfeed-org
  :after org
  :preface
  (setq rmh-elfeed-org-files `(,(expand-file-name "elfeed.org" org-directory)))
)

(provide 'config-rss)
;;; config-rss.el ends here
