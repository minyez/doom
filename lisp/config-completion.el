;;; config-completion.el -*- lexical-binding: t; -*-

(use-package! corfu
  :config
  ; disable tab
  ; https://new.reddit.com/r/emacs/comments/xqk1xj/disable_tab_completion_in_corfu/
  (setq tab-always-indent t)
  (define-key corfu-map [tab] nil)
  (define-key corfu-map "\t" nil)
)

(provide 'config-completion)
;;; config-completion.el ends here
