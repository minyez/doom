;;; test/habit-stats-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'org)
(load-file
 (expand-file-name "../autoload/habit-stats.el"
                   (file-name-directory load-file-name)))

(ert-deftest my/org-habit-stats-daily-dblock ()
  (let* ((today (current-time))
         (tomorrow (time-add today (days-to-time 1)))
         (file (make-temp-file "habit-stats-" nil ".org"))
         (org-agenda-files (list file)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             (format
              "* TODO Nightly review\nSCHEDULED: <%s ++1d>\n:PROPERTIES:\n:STYLE: habit\n:END:\n:LOGBOOK:\n- State \"DONE\"       from \"TODO\"       [%s]\n:END:\n"
              (format-time-string "%Y-%m-%d %a 22:30" tomorrow)
              (format-time-string "%Y-%m-%d %a 22:30" today))))
          (with-temp-buffer
            (org-mode)
            (insert "#+BEGIN: habit-stats :scope agenda :block today\n#+END:\n")
            (goto-char (point-min))
            (org-update-dblock)
            (should (string-match-p "Nightly review" (buffer-string)))
            (should (string-match-p
                     (regexp-quote (format-time-string "%Y-%m-%d" today))
                     (buffer-string)))
            (should (string-match-p
                     "|[ \t]+1[ \t]+|[ \t]+0[ \t]+|[ \t]+0[ \t]+|[ \t]+100%"
                     (buffer-string)))
            (should (string-match-p "\\*Total\\*" (buffer-string)))
            (should-not (string-match-p "\n\n#\\+END:" (buffer-string)))))
      (when-let ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (delete-file file))))

(ert-deftest my/org-habit-stats-ng-dblock ()
  (let* ((today (current-time))
         (yesterday (time-subtract today (days-to-time 1)))
         (file (make-temp-file "habit-stats-ng-" nil ".org"))
         (org-agenda-files (list file)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert
             (format
              "* TODO Flexible review\nSCHEDULED: <%s .+1d>\n:PROPERTIES:\n:STYLE: habit\n:RECURRENCE: FREQ=DAILY;INTERVAL=1\n:ADDED: [%s]\n:END:\n:LOGBOOK:\n- State \"DONE\"       from \"TODO\"       [%s]\n:END:\n"
              (format-time-string "%Y-%m-%d %a" today)
              (format-time-string "%Y-%m-%d %a" yesterday)
              (format-time-string "%Y-%m-%d %a 22:30" yesterday))))
          (with-temp-buffer
            (org-mode)
            (insert "#+BEGIN: habit-stats :scope agenda :block thisweek :total nil\n#+END:\n")
            (goto-char (point-min))
            (org-update-dblock)
            (should (string-match-p "Flexible review" (buffer-string)))
            (should (string-match-p
                     "|[ \t]+1[ \t]+|[ \t]+0[ \t]+|[ \t]+1[ \t]+|[ \t]+100%[ \t]+|[ \t]+1"
                     (buffer-string)))
            (should-not (string-match-p "\\*Total\\*" (buffer-string)))
            (should-not (string-match-p "\n\n#\\+END:" (buffer-string)))))
      (when-let ((buffer (get-file-buffer file)))
        (kill-buffer buffer))
      (delete-file file))))
