;;; config-org-present.el -*- lexical-binding: t; -*-
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

(provide 'config-org-present)
;;; config-org-present.el ends here
