;;; config-org-drill.el -*- lexical-binding: t; -*-
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

(provide 'config-org-drill)
;;; config-org-drill.el ends here
