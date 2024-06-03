;;; my-keybindings.el -*- lexical-binding: t; -*-
(use-package! key-chord
  :config
  (key-chord-mode 1)
  ;; https://discourse.doomemacs.org/t/typing-jk-deletes-j-and-returns-to-normal-mode/59
  ;; commented out for double pinyin
  ;(with-eval-after-load 'evil-escape
  ;  (key-chord-define-global "jk" 'evil-escape))
)

(map! :leader
      (:prefix-map ("e" . "extra-my")
        (:prefix-map ("D" . "drill")
          :desc "org-drill"          "d"   #'org-drill
          :desc "org-drill-resume"   "r"   #'org-drill-resume)
        (:prefix-map ("d" . "download")
          :desc "rename"                    "r"   #'org-download-rename-at-point
          :desc "image from clipboard"      "p"   #'org-download-clipboard
          :desc "download from kill URL"    "d"   #'org-download-yank)
        :desc "org-roam-node-find"      "."   #'org-roam-node-find
        :desc "Find file Other Window"  "f"   #'find-file-other-window
        :desc "org-agenda-list"         "a"   #'org-agenda-list
        :desc "Sort entries by todo"    "S"   '(lambda () (interactive) (org-sort-entries t ?o))
        :desc "org-schedule"            "s"   #'org-schedule)

      ;; supplement existing map
      (:prefix-map ("t" . "toggle")
        :desc "truncate-lines"       "L"   #'toggle-truncate-lines)

      ;; supplement existing map
      (:prefix-map ("r" . "reference")
        :desc "citar-copy-reference"   "r"   #'citar-copy-reference
        :desc "citar-open"             "o"   #'citar-open
        :desc "citar-insert-citation"  "i"   #'citar-insert-citation
        :desc "citar-insert-keys"      "k"   #'citar-insert-keys)

      (:prefix-map ("g" . "git")
       (:when (modulep! :tools magit)
        ; "SPC g a" for stage file, just as "git add"
        :desc "stage current buffer file"    "a"    #'magit-stage-buffer-file))
)

(provide 'my-keybindings)
;;; my-keybindings.el ends here
