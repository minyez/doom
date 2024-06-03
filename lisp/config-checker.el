;;; config-checker.el -*- lexical-binding: t; -*-

(when (modulep! :checkers spell)
  (after! ispell
    (setq ispell-personal-dictionary (concat doom-private-dir "words"))
  )
)

(provide 'config-checker)
;;; config-checker.el ends here
