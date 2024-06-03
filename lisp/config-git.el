;;; config-git.el -*- lexical-binding: t; -*-

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

(provide 'config-git)
;;; config-git.el ends here
