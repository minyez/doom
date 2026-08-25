;;; autoload/bibliography.el -*- lexical-binding: t; -*-

(require 'seq)

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

(defun my/org--markdown-to-org (markdown)
  "Convert MARKDOWN to Org syntax with Pandoc."
  (unless (executable-find "pandoc")
    (user-error "Pandoc is required to paste Zotero annotations"))
  (with-temp-buffer
    (insert markdown)
    (let ((status (call-process-region
                   (point-min) (point-max) "pandoc" t '(t t) nil
                   "--from=markdown" "--to=org" "--wrap=none")))
      (unless (and (integerp status) (zerop status))
        (user-error "Pandoc failed: %s" (string-trim (buffer-string))))
      (buffer-string))))

;;;###autoload
(defun my/org-yank-zotero-annotation ()
  "Paste a Zotero PDF annotation as Org, preserving its source link."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command only works in Org buffers"))
  (let ((markdown
         (seq-find
          (lambda (text)
            (and (stringp text)
                 (string-match-p "zotero://open-pdf/" text)))
          (list (and (fboundp 'gui-get-selection)
                     (ignore-errors
                       (gui-get-selection 'CLIPBOARD 'UTF8_STRING)))
                (and (fboundp 'gui-get-selection)
                     (ignore-errors
                       (gui-get-selection 'CLIPBOARD 'STRING)))
                (car-safe kill-ring)))))
    (unless markdown
      (user-error "Clipboard has no Zotero PDF annotation link"))
    (setq markdown
          (replace-regexp-in-string
           "(\\[\\([^]\n]+\\)\\](zotero://select/[^)\n]+))[[:space:]]+(\\[pdf\\](\\(zotero://open-pdf/[^)\n]+\\)))"
           "[\\1](\\2)" markdown))
    (insert (my/org--markdown-to-org markdown))))
