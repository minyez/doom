;;; config-evil.el -*- lexical-binding: t; -*-

(use-package! evil
  :config
  (evil-select-search-module 'evil-search-module 'evil-search)
  (evil-set-initial-state 'dired-mode 'normal)
  (evil-set-initial-state 'elpaca-ui-mode 'motion)
  (dolist (mode '(delve-mode
                  elfeed-search-mode
                  easy-hugo-mode
                  eshell-mode
                  git-rebase-mode
                  pyim-dict-manager-mode
                  vterm-mode
                  term-mode
                  calc-mode))
    (evil-set-initial-state mode 'emacs)))

(provide 'config-evil)
;;; config-evil.el ends here
