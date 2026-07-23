;;; autoload/habit-stats.el -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'calendar)
(require 'org)
(require 'org-agenda)
(require 'org-clock)
(require 'org-habit)
(require 'org-table)
(require 'seq)
(require 'subr-x)

(declare-function org-habit-ng-instances
                  "org-habit-ng"
                  (rule anchor-day completions start-day end-day today
                        &optional skip-events vacation-ranges))
(declare-function org-habit-ng-parse-rrule "org-habit-ng" (rrule-string))
(declare-function org-habit-ng-score
                  "org-habit-ng"
                  (rule anchor-day completions start-day end-day today
                        &optional skip-events vacation-ranges))

(defun my/org-habit-stats--completion-days (state)
  "Return unique completion days logged for STATE in the current entry."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (org-entry-end-position))
          (regexp (format "^[ \t]*-[ \t]+State \"%s\".*\\(%s\\)"
                          (regexp-quote state)
                          org-ts-regexp-inactive))
          days)
      (while (re-search-forward regexp end t)
        (push (time-to-days
               (org-time-string-to-time
                (match-string-no-properties 1)))
              days))
      (sort (delete-dups days) #'<))))

(defun my/org-habit-stats--property-day (property)
  "Return absolute day stored in timestamp PROPERTY, or nil."
  (when-let* ((value (org-entry-get nil property))
              ((string-match org-ts-regexp value)))
    (time-to-days
     (org-time-string-to-time (match-string-no-properties 0 value)))))

(defun my/org-habit-stats--range (params)
  "Return the day range selected by clock-style PARAMS.
The result is (START END DESCRIPTION), with END exclusive."
  (let* ((block (or (plist-get params :block) 'thismonth))
         (range (org-clock-special-range block)))
    (unless (and (car range) (cadr range))
      (user-error "habit-stats requires a bounded :block"))
    (list (time-to-days (car range))
          (time-to-days (cadr range))
          (nth 2 range))))

(defun my/org-habit-stats--streak (statuses)
  "Return the trailing satisfied count in STATUSES."
  (cl-loop for status in (reverse statuses)
           while (eq status 'satisfied)
           count t))

(defun my/org-habit-stats--standard-statuses
    (scheduled interval completions start end today)
  "Evaluate an ordinary Org habit over [START, END).
SCHEDULED anchors recurrence slots of INTERVAL days.  COMPLETIONS is
a list of absolute days, and TODAY separates missed from pending."
  (let ((first (+ start (mod (- scheduled start) interval)))
        statuses)
    (cl-loop for due from first below end by interval
             when (<= due today)
             do
             (push
              (if (seq-some
                   (lambda (done)
                     (and (>= done due)
                          (< done (+ due interval))))
                   completions)
                  'satisfied
                (if (= due today) 'pending 'missed))
              statuses))
    (nreverse statuses)))

(defun my/org-habit-stats--summarize-statuses (statuses)
  "Return summary plist for occurrence STATUSES."
  (let* ((done (cl-count 'satisfied statuses))
         (missed (cl-count 'missed statuses))
         (pending (cl-count 'pending statuses))
         (scored (+ done missed)))
    (list :done done
          :missed missed
          :pending pending
          :rate (and (> scored 0) (round (* 100.0 (/ (float done) scored))))
          :streak (my/org-habit-stats--streak
                   (seq-filter (lambda (status)
                                 (memq status '(satisfied missed)))
                               statuses)))))

(defun my/org-habit-stats--entry-row (file state start end today)
  "Return one statistics row for the habit at point."
  (let* ((title (org-get-heading t t t t))
         (scheduled-time (org-get-scheduled-time (point)))
         (scheduled-text (org-entry-get nil "SCHEDULED"))
         (recurrence (org-entry-get nil "RECURRENCE"))
         (all-completions (my/org-habit-stats--completion-days state))
         (scheduled-day
          (if scheduled-time
              (time-to-days scheduled-time)
            (user-error "Habit %s has no SCHEDULED timestamp" title)))
         (history-start
          (or (my/org-habit-stats--property-day "ADDED")
              (my/org-habit-stats--property-day "CREATED")
              (car all-completions)
              scheduled-day))
         (start (max start history-start))
         (completions (seq-filter
                       (lambda (day) (and (>= day start) (< day end)))
                       all-completions))
         summary)
    (setq
     summary
     (if (and recurrence (not (string-empty-p recurrence)))
         (progn
           (require 'org-habit-ng)
           (let* ((rule (org-habit-ng-parse-rrule recurrence))
                  (anchor scheduled-day)
                  (instances
                   (org-habit-ng-instances
                    rule anchor all-completions start (1- end) today))
                  (statuses (mapcar (lambda (instance)
                                      (plist-get instance :status))
                                    instances))
                  (result (my/org-habit-stats--summarize-statuses statuses))
                  (score (org-habit-ng-score
                          rule anchor all-completions start (1- end) today)))
             (plist-put result :streak (plist-get score :streak))))
       (let* ((repeat (org-get-repeat scheduled-text))
              (interval (and repeat (org-habit-duration-to-days repeat))))
         (unless (and interval (> interval 0))
           (user-error "Habit %s has no supported scheduled repeater" title))
         (my/org-habit-stats--summarize-statuses
          (my/org-habit-stats--standard-statuses
           scheduled-day interval completions start end today)))))
    (append
     (list :file (if file (file-name-nondirectory file) "")
           :title title
           :last (car (last all-completions)))
     summary)))

(defun my/org-habit-stats--collect-buffer
    (buffer state start end today match)
  "Collect habit rows from BUFFER."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (let (rows)
       (org-map-entries
        (lambda ()
          (when (org-is-habit-p)
            (condition-case err
                (push (my/org-habit-stats--entry-row
                       (buffer-file-name) state start end today)
                      rows)
              (error
               (message "habit-stats: skipping %s: %s"
                        (org-get-heading t t t t)
                        (error-message-string err))))))
        match 'file)
       rows))))

(defun my/org-habit-stats--collect (scope state start end today match)
  "Collect habit rows from SCOPE."
  (pcase scope
    ((or `agenda "agenda")
     (let ((files (org-agenda-files t))
           rows)
       (org-agenda-prepare-buffers files)
       (dolist (file files)
         (setq rows
               (nconc
                (my/org-habit-stats--collect-buffer
                 (org-get-agenda-file-buffer file)
                 state start end today match)
                rows)))
       rows))
    ((or `file "file" `nil)
     (my/org-habit-stats--collect-buffer
      (org-base-buffer (current-buffer))
      state start end today match))
    (_ (user-error "Unknown habit-stats scope: %S" scope))))

(defun my/org-habit-stats--cell (value)
  "Return VALUE as safe Org table text."
  (replace-regexp-in-string "|" "¦" (format "%s" value) t t))

(defun my/org-habit-stats--date (day)
  "Format absolute DAY for the report."
  (if day
      (pcase-let ((`(,month ,date ,year)
                    (calendar-gregorian-from-absolute day)))
        (format "%04d-%02d-%02d" year month date))
    "-"))

;;;###autoload
(defun org-dblock-write:habit-stats (params)
  "Write an Org table summarizing habits.

PARAMS accepts clocktable-like `:scope' and `:block' values.  `:scope'
is `agenda' by default, and `:block' defaults to `thismonth'.  Optional
`:match' is an Org tags/properties match and `:state' selects the logged
completion state, defaulting to DONE.  Set `:total nil' to omit the
total row.

Example:

  #+BEGIN: habit-stats :scope agenda :block thismonth
  #+END:"
  (pcase-let* ((`(,start ,requested-end ,description)
                 (my/org-habit-stats--range params))
                (today (time-to-days nil))
                (end (min requested-end (1+ today)))
                (scope (or (plist-get params :scope) 'agenda))
                (state (format "%s" (or (plist-get params :state) "DONE")))
                (match (plist-get params :match))
                (show-total (if (plist-member params :total)
                                (plist-get params :total)
                              t))
                (rows (my/org-habit-stats--collect
                       scope state start end today match))
                (rows (sort rows
                            (lambda (a b)
                              (string-lessp
                               (concat (plist-get a :file)
                                       (plist-get a :title))
                               (concat (plist-get b :file)
                                       (plist-get b :title))))))
                (total-done (cl-loop for row in rows
                                     sum (plist-get row :done)))
                (total-missed (cl-loop for row in rows
                                       sum (plist-get row :missed)))
                (total-pending (cl-loop for row in rows
                                        sum (plist-get row :pending)))
                (total-scored (+ total-done total-missed))
                (total-rate (and (> total-scored 0)
                                 (round (* 100.0
                                           (/ (float total-done)
                                              total-scored)))))
                (table-start nil))
    (insert (format "#+CAPTION: Habit statistics — %s (%s to %s)\n"
                    description
                    (my/org-habit-stats--date start)
                    (my/org-habit-stats--date (1- end))))
    (setq table-start (point))
    (insert "| File | Habit | Done | Missed | Pending | Rate | Streak | Last done |\n")
    (insert "|-\n")
    (dolist (row rows)
      (insert
       (format "| %s | %s | %d | %d | %d | %s | %d | %s |\n"
               (my/org-habit-stats--cell (plist-get row :file))
               (my/org-habit-stats--cell (plist-get row :title))
               (plist-get row :done)
               (plist-get row :missed)
               (plist-get row :pending)
               (if-let ((rate (plist-get row :rate)))
                   (format "%d%%" rate)
                 "-")
               (plist-get row :streak)
               (my/org-habit-stats--date (plist-get row :last)))))
    (when show-total
      (insert
       (format "| *Total* |  | %d | %d | %d | %s |  |  |\n"
               total-done total-missed total-pending
               (if total-rate (format "%d%%" total-rate) "-"))))
    (delete-char -1)
    (save-excursion
      (goto-char table-start)
      (org-table-align))))

;;;###autoload
(defun my/org-insert-habit-stats ()
  "Insert and update a monthly agenda-wide habit statistics block."
  (interactive)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command must be used in an Org buffer"))
  (let ((start (point)))
    (insert "#+BEGIN: habit-stats :scope agenda :block thismonth\n#+END:\n")
    (goto-char start)
    (org-update-dblock)))

(provide 'habit-stats)
