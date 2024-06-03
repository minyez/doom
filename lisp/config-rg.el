;;; config-rg.el -*- lexical-binding: t; -*-

(use-package! rg
  :config
  (setq rg-keymap-prefix "\C-cg")
  (setq rg-ignore-case 'smart)
  (rg-enable-default-bindings)
  (map! :leader
        (:prefix-map "s"
          (:prefix-map ("g" . "ripgrep")
           :desc "rg"                  "g"  #'rg
           :desc "rg-menu"             "m"  #'rg-menu
           :desc "Cursor word"         "w"  #'rg-dwim
           :desc "Cursor Word (file)"  "f"  #'rg-dwim-current-file)))
)

(provide 'config-rg)
;;; config-rg.el ends here
