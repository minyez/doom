;;; my-variables.el -*- lexical-binding: t; -*-

(defvar my/org-dir
  (expand-file-name "org-roam" "~/Library/CloudStorage/Dropbox")
  "org directory")

(defvar my/org-roam-inbox
  (expand-file-name "0_Scratch" my/org-dir)
  "location where usual org-roam-capture goes")

(defvar my/read-note-dir
  (expand-file-name "Review" (concat my/org-dir))
  "directory for reading notes")

(defvar my/bibtex-file
  (expand-file-name "etc/bibliography.bib" my/org-dir)
  "all-in-one bibtex file for referecnes")

(defvar my/literature-note-dir
  (expand-file-name "Paper" my/org-dir)
  "Directory to store the notes of literature")

(defvar my/talk-note-dir
  (expand-file-name "Talk" my/org-dir)
  "Directory to store the notes of talks, conferences")

(provide 'my-variables)
;;; my-variables.el ends here
