;;; config-dictionary.el -*- lexical-binding: t; -*-
(if IS-MAC
  (progn
    (use-package! osx-dictionary
      :bind
      (:map global-map
            ("C-c d" . osx-dictionary-search-word-at-point))
      :config
    )
  )
)

(provide 'config-dictionary)
;;; config-dictionary.el ends here
