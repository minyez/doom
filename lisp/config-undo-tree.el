;;; config-undo-tree.el -*- lexical-binding: t; -*-

(after! undo-tree
  (defun radian--undo-tree-suppress-undo-history-saved-message
        (undo-tree-save-history &rest args)
      (let ((inhibit-message t))
        (apply undo-tree-save-history args)))

  (defun radian--undo-tree-suppress-buffer-modified-message
      (undo-tree-load-history &rest args)
    (let ((inhibit-message t))
      (apply undo-tree-load-history args)))

  (advice-add #'undo-tree-load-history :around
              #'radian--undo-tree-suppress-buffer-modified-message))

(provide 'config-undo-tree)
;;; config-undo-tree.el ends here
