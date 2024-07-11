;;; config-projectile.el -*- lexical-binding: t; -*-
(use-package! projectile
  :custom
  (projectile-project-search-path (list "~/projects"))
  ;; :config
  ;; (map! :leader
  ;;       (:prefix-map "p"
  ;;        :desc "Switch to project"  "p"  'projectile-switch-project))
)

(provide 'config-projectile)
;;; config-projectile.el ends here
