;;; config-org-bars.el -*- lexical-binding: t; -*-
(use-package! org-bars
  :after org
  :commands org-bars-mode
  ;; disabled due to blinking when using magit
  ; :hook
  ; (org-mode . org-bars-mode)
)

(provide 'config-org-bars)
;;; config-org-bars.el ends here
