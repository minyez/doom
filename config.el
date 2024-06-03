;;; config.el -*- lexical-binding: t; -*-

(setq user-full-name "Minye Zhang"
      user-mail-address "minyez.physchem@gmail.com")

(add-to-list 'load-path (concat doom-private-dir "lisp"))

(if (getenv "EMACS_PROF")
    (setq use-package-verbose t
          use-package-expand-minimally nil
          use-package-compute-statistics t)
  (setq use-package-verbose nil
        use-package-expand-minimally t))

(setq confirm-kill-emacs nil)     ; do not ask if I would like to go

(require 'my-variables)

(require 'my-keybindings)

(require 'config-ui)

(require 'config-evil)

(require 'config-cjk)

(require 'config-completion)

(require 'config-undo-tree)

(require 'config-checker)

(require 'config-org)

(require 'config-org-recur)

(require 'config-org-ref)

(require 'config-org-bars)

(require 'config-org-drill)

(require 'config-ox)

(require 'config-org-present)

(require 'config-org-roam)

(require 'config-org-noter)

(require 'config-projectile)

(require 'config-command-log-mode)

(require 'config-dictionary)

(require 'config-rg)

(require 'config-pdf)

(require 'config-rss)

(require 'config-git)

(require 'config-blog)
