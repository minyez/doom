;;; autoload/bibliography.el -*- lexical-binding: t; -*-

;;;###autoload
(progn
  ;; Org-roam optionally requires full org-ref during every database sync and
  ;; file update.  Suppress only that optional require: this disables legacy
  ;; org-ref citation extraction, while Org Cite extraction and explicit
  ;; loading of org-ref continue to work normally.
  (defvar my/org-roam-db-suppress-org-ref nil)

  (defun my/org-roam-db-require-a (fn feature &optional filename noerror)
    (if (and my/org-roam-db-suppress-org-ref (eq feature 'org-ref))
        t
      (funcall fn feature filename noerror)))

  (defun my/org-roam-db-without-org-ref-a (fn &rest args)
    (let ((my/org-roam-db-suppress-org-ref t))
      (apply fn args)))

  (advice-add 'require :around #'my/org-roam-db-require-a)

  ;; This setting must exist before bibtex-completion is ever loaded.
  (setq bibtex-completion-watch-bibliography nil)

  (with-eval-after-load 'org-roam-db
    (advice-add 'org-roam-db-sync :around
                #'my/org-roam-db-without-org-ref-a)
    (advice-add 'org-roam-db-update-file :around
                #'my/org-roam-db-without-org-ref-a)))
