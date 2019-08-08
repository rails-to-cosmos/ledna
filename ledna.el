;;; ledna.el --- try to make edna more flexible and familiar to emacs-lisp developers

;; Copyright (C) 2019 Dmitry Akatov

;; Author: Dmitry Akatov <akatovda@yandex.com>
;; Created: 17 Feb 2019
;; Version: 0.1

;; Keywords: org babel
;; Homepage: https://github.com/rails-to-cosmos/org-literate-devtools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; This package allows you to manage bookmarks and travel around the
;; digital world with an org-mode power behind your shoulders.

;;; Code:

(defun ledna/eval-forms (forms)
  (unwind-protect
      (mapc #'(lambda (f) (when f (eval (read f)))) forms)
    t))

(defun ledna/get-entry-values-by-keys (pom keys)
  (remove 'nil (mapcar #'(lambda (f) (org-entry-get pom f org-edna-use-inheritance)) keys)))

(defun ledna/apply-magic-tag (tag keys)
  (let* ((params (alist-get tag ledna/magic-tags))
         (priority (cadr params))
         (props (car params)))
    (loop for key in keys
          do (when-let (magic-form (car (alist-get (intern key) props)))
               (eval magic-form)))
    priority))

(defun ledna--apply-magic-tag-consider-priority (tag priority keys vals &optional entry-props-evaled-p)
  (when (and vals
             (not entry-props-evaled-p)
             (>= priority 100))
    (ledna/eval-forms vals)
    (setq entry-props-evaled-p t))
  (ledna/apply-magic-tag tag keys)
  entry-props-evaled-p)

(defun apply-ledna-forms (entry-keys pom)
  (save-excursion
    (org-goto-marker-or-bmk pom)

    (let (entry-props-evaled-p
          (magic-tags (ledna/magic-tags-list))
          (complex-tags (ledna/complex-tags-list))
          (entry-vals (ledna/get-entry-values-by-keys pom entry-keys))
          (src-org-tags (mapcar #'intern (org-get-tags))))

      ;; Process complex tags
      (dolist (src-tag src-org-tags)
        (when (member src-tag complex-tags)
          (let* ((sm-tags (car (alist-get src-tag ledna/complex-tags))))
            (loop for (tag priority)
                  in (ledna/tags-prioritized magic-tags)
                  when (member tag sm-tags)
                  do (setq entry-props-evaled-p
                           (ledna--apply-magic-tag-consider-priority
                            tag priority
                            entry-keys entry-vals
                            entry-props-evaled-p))))))

      ;; Process simple tags (minor copy-paste)
      (loop for (tag priority)
            in (ledna/tags-prioritized magic-tags)
            when (member tag src-org-tags)
            do (setq entry-props-evaled-p
                     (ledna--apply-magic-tag-consider-priority
                      tag priority
                      entry-keys entry-vals
                      entry-props-evaled-p)))

      ;; Process user properties
      (when (and entry-vals (not entry-props-evaled-p))
        (ledna/eval-forms entry-vals)))))

(defun ledna-trigger-function-emacs-lisp (change-plist)
  "Trigger function work-horse.

See `org-edna-run' for CHANGE-PLIST explanation.

This shouldn't be run from outside of `org-trigger-hook'."
  (let* ((pos (plist-get change-plist :position))

         (to* (or (plist-get change-plist :to) ""))
         (from* (or (plist-get change-plist :from) ""))

         (to (cond ((symbolp to*) (symbol-name to*))
                   ((stringp to*) (substring-no-properties to*))))
         (from (cond ((symbolp from*) (symbol-name from*))
                     ((stringp from*) (substring-no-properties from*))))

         (prop-templates
          (list (format "%s->%s" from to)
                (format "%s->*" from)
                (format "*->%s" to)
                "*"
                "*->*")))

    (apply-ledna-forms prop-templates pos)))

(defun ledna-blocker-function-emacs-lisp (change-plist)
  "Trigger function work-horse.

See `org-edna-run' for CHANGE-PLIST explanation.

This shouldn't be run from outside of `org-trigger-hook'."
  (let* ((pos (plist-get change-plist :position))
         (type (plist-get change-plist :type))
         (to* (or (plist-get change-plist :to) ""))
         (from* (or (plist-get change-plist :from) ""))
         (to (cond ((symbolp to*) (symbol-name to*))
                   ((stringp to*) (substring-no-properties to*))))
         (from (cond ((symbolp from*) (symbol-name from*))
                     ((stringp from*) (substring-no-properties from*))))

         (prop-templates
          (list (format "#%s->%s" from to)
                (format "#%s->*" from)
                (format "#*->%s" to)
                "#*" "#*->*")))

    (ledna-run change-plist
      (if-let ((forms (remove 'nil (mapcar #'(lambda (tpl) (org-entry-get pos tpl org-edna-use-inheritance)) prop-templates))))
          (not (setq org-block-entry-blocking (not (some 'null (mapcar #'(lambda (form) (eval (read form))) forms)))))
        t))))


(defmacro ledna-run (change-plist &rest body)
  "Run a TODO state change.

The state information is held in CHANGE-PLIST.

If the TODO state is changing from a TODO state to a DONE state, run BODY."
  (declare (indent 1))
  `(if (eq (plist-get ,change-plist :type) 'todo-state-change)
       (condition-case-unless-debug err
           ,@body
         (error
          (if (eq (car err) 'invalid-read-syntax)
              (org-edna--print-syntax-error (cdr err))
            (message "Edna Error at heading %s: %s" (org-get-heading t t t) (error-message-string err)))
          (setq org-block-entry-blocking (org-get-heading))
          ;; Block
          nil))
     t))

(defun ledna-dsl-init ()
  (advice-add 'org-edna-trigger-function :around #'ledna-dsl-specifier-trigger)
  (advice-add 'org-edna-blocker-function :around #'ledna-dsl-specifier-blocker))

(defun ledna-dsl-specifier-trigger (orig-fun &rest args)
  "Wrap edna's triggers.

ORIG-FUN is a trigger function called with ARGS."
  (apply #'ledna-trigger-function-emacs-lisp args))

(defun ledna-dsl-specifier-blocker (orig-fun &rest args)
  "Wrap edna's blockers.

ORIG-FUN is a blocker function called with ARGS."
  (apply #'ledna-blocker-function-emacs-lisp args))

;; one or many
(defun ledna/oom (items)
  (if (and (listp items) (= (length items) 1))
      (car items)
    items))

;; marker or self
(defun ledna/mos (&optional marker-or-markers)
  (or marker-or-markers (ledna/$self)))

(defun ledna/markers (&optional marker-or-markers)
  (let* ((marker (ledna/mos marker-or-markers))
         (markers (if (markerp marker) (list marker) marker)))
    markers))

(defun ledna/defer (handler &optional marker timeout)
  (run-with-idle-timer (or timeout 2) nil
                       #'(lambda (h s) (ledna/map h s))
                       handler (ledna/mos marker)))

(defun ledna/map (handler &optional marker)
  (save-window-excursion
    (save-excursion
      (loop for mark in (ledna/markers marker)
            collect (progn
                      (org-goto-marker-or-bmk mark)
                      (funcall handler))
            finally (progn
                      (org-align-tags t)
                      (org-update-checkbox-count))))))

(defun string-is-numeric-p (string)
  "Return non-nil if STRING is a valid numeric string.

Examples of valid numeric strings are \"1\", \"-3\", or \"123\"."
  ;; Can't use string-to-number, because it returns 0 if STRING isn't a
  ;; number, which is ambiguous.
  (numberp (car (read-from-string string))))

(defun ledna/org-kill-subtree ()
  (kill-region (org-entry-beginning-position) (org-entry-end-position)))

(defun ledna/rename (title &optional marker)
  (cl-flet ((rename ()
                 (search-forward " ")
                 (org-kill-line)
                 (insert title)))
    (ledna/map #'rename marker)))

(defun ledna-entry-name-from-template ()
  (when-let ((template (or (ledna/get-property ledna-props-template) (cdr (assoc-string "ITEM" (org-entry-properties))))))
    (org-back-to-heading)
    (org-beginning-of-line)
    (org-kill-line)

    (let ((entry-name-format template)
          (entry-name-fmt-args  (org-entry-properties)))
      (insert (s-format entry-name-format 'aget entry-name-fmt-args)))))

(require 's)

(defun ledna-clone (&rest args)
  (save-window-excursion
    (save-excursion
      (org-back-to-heading)

      (let* ((src-entry             (or (plist-get args :source)       (ledna/$self)))
             (src-props             (org-entry-properties))
             (src-props-std         (org-entry-properties nil 'standard))
             (src-props-std-keys    (mapcar #'car src-props-std))
             (src-tags-string       (org-make-tag-string (org-get-tags nil t)))
             (todo-state            (or (plist-get args :todo-state)   "TODO"))
             (target-props          (or (plist-get args :properties)   src-props-std-keys)))

        (org-insert-heading-respect-content)
        (insert (cdr (assoc-string "ITEM" src-props)) " " src-tags-string)

        ;; Copy properties
        (mapc #'(lambda (prop)
                  (when-let (p (assoc-string prop src-props))
                    (condition-case nil
                        (ledna/set-property (car p) (cdr p))
                      (error nil))))
              target-props)

        (ledna/set-todo-state todo-state))
      (org-align-tags t)
      (org-update-checkbox-count))))

(defun ledna/set-property (property value &optional marker)
  (cl-flet ((set-current-prop () (org-entry-put marker property
                                             (cond ((numberp value) (number-to-string value))
                                                   ((stringp value) value)
                                                   (t "Unknown value type")))))
    (ledna/map #'set-current-prop marker)))

(defun ledna/get-property (property &optional marker default)
  (ledna/oom (loop for mark in (ledna/markers marker)
                   for property-value = (or (org-entry-get mark property) default)
                   when (not (eq property-value nil))
                   collect property-value)))

(defun ledna/get-property-read (property &optional marker default)
  (if-let ((pval (ledna/get-property property marker default)))
      (eval (read pval))))

(defun ledna/get-title (&optional target default)
  (ledna/get-property "ITEM" target default))

(defun -ledna/next-value (allowed &optional current)
  (loop for item in allowed with a = -1
        if (or (string= current item)
               (> a -1))
        do (setq a (1+ a))
        if (= a 1) return item
        finally (return (car allowed))))

(defun ledna/switch-to-next-allowed-value (property &optional marker)
  (loop for mark in (ledna/markers marker)
        with current = (ledna/get-property property mark)
        with allowed = (org-property-get-allowed-values mark property)
        when allowed
        do (ledna/set-property property (-ledna/next-value allowed current) mark)))

(defun ledna/cycle-props ()
  (let ((props (ledna/get-property-read ledna-props-cycle)))
    (ledna/map #'(lambda () (mapc 'ledna/switch-to-next-allowed-value props)))))

(defun ledna/inc-property (property &optional val units marker)
  (loop for mark in (ledna/markers marker)
        with result-value
        do (let* ((full-prop-value (ledna/get-property property mark "0"))
                  (inc-value (cond ((and (stringp val) (string-is-numeric-p val)) (string-to-number val))
                                   ((numberp val) val)
                                   (t 1)))
                  (prop-number (string-to-number (car (split-string full-prop-value))))
                  (prop-label (or units (key-description (cdr (split-string full-prop-value))))))
             (setq result-value (s-trim (concat (number-to-string (+ inc-value prop-number)) " " prop-label)))
             (ledna/set-property property result-value mark))
        collect result-value))

(defun ledna/inc-property-get (property &rest args)
  (apply #'ledna/inc-property (append (list property) args))
  (ledna/get-property property))

(defun ledna/get-todo-state (&optional marker)
  (ledna/oom
   (mapcar 'substring-no-properties
           (remove nil (ledna/map 'org-get-todo-state marker)))))

(defun ledna/set-todo-state (state &optional marker)
  (ledna/map #'(lambda () (org-todo state)) marker))

(defun ledna/$children (&optional marker)
  (-flatten (ledna/map 'org-edna-finder/children marker)))

(defun ledna/$parent ()
  (org-edna-finder/parent))

(defun ledna/$self ()
  (save-window-excursion
    (save-excursion
      (org-back-to-heading)
      (list (point-marker)))))

(defun ids (&rest ids)
  "Find a list of headings with given IDS.

Edna Syntax: ids(ID1 ID2 ...)

Each ID is a UUID as understood by `org-id-find'.

Note that in the edna syntax, the IDs don't need to be quoted."
  (mapcar (lambda (id) (org-id-find id 'marker)) ids))

(defun ledna/search (match-spec &optional scope skip)
  "Find entries using Org matching.

Edna Syntax: ledna/search(\"MATCH-SPEC\" SCOPE SKIP)

MATCH-SPEC may be any valid match string; it is passed straight
into `org-map-entries'.

SCOPE and SKIP are their counterparts in `org-map-entries'.
SCOPE defaults to agenda, and SKIP defaults to nil."
  (when match-spec
    (setq scope (or scope 'agenda))
    (org-map-entries
     ;; Find all entries in the agenda files that match the given tag.
     (lambda nil (point-marker))
     match-spec scope skip)))

(defun select (&rest markers)
  (apply #'append markers))
;; (select (ids "test-pass-purchased-p") (tags "test_tag"))
;; TODO (select :ids '(test-pass-purchased-p) :tags '(test_tag))

(defun clocktime-from-timestamp-or-effort ()
  (let* ((timestamp (save-excursion
                      (org-back-to-heading)
                      (re-search-forward org-element--timestamp-regexp
                                         (org-entry-end-position))
                      (goto-char (match-beginning 0))
                      (org-element-timestamp-parser)))
         (start-time (org-timestamp-to-time timestamp))
         (end-time (org-timestamp-to-time timestamp t))
         (effort (/ (float-time (time-subtract end-time start-time)) 60)))

    (unless (> effort 0)
      (setq effort (org-duration-to-minutes
                    (or (ledna/get-property "EFFORT")
                        (let ((user-effort (org-read-property-value "EFFORT")))
                          (org-set-property "EFFORT" user-effort)
                          user-effort))))
      (setq end-time (seconds-to-time (+ (time-to-seconds start-time) (* effort 60)))))

    (with-temp-buffer
      (org-insert-time-stamp start-time 'with-hm 'inactive (concat org-clock-string " "))
      (org-insert-time-stamp end-time 'with-hm 'inactive "--")
      (org-clock-update-time-maybe)
      (buffer-string))))

(defun ledna/consider-effort-as-clocktime ()
  (save-excursion
    (let ((clocktime (clocktime-from-timestamp-or-effort)))
      (org-clock-find-position t)
      (insert-before-markers "\n")
      (backward-char 1)
      (org-indent-line)
      (when (and (save-excursion (end-of-line 0) (org-in-item-p)))
        (beginning-of-line 1)
        (indent-line-to (- (current-indentation) 2)))
      (insert (clocktime-from-timestamp-or-effort))
      (org-back-to-heading)
      (org-set-tags (remove "Effort_Clock" (org-get-tags))))))

(defun ledna/advanced-schedule (&optional target)
  (when-let (schedule (ledna/get-property-read ledna-props-schedule))
    (let ((next-time (ledna/get-nearest-date schedule))
          (org-last-state (ledna/get-todo-state target))
          (todo-word "TODO")
          (done-word "DONE")
          (end (copy-marker (org-entry-end-position))))

      (when (or org-log-repeat
		(catch :clock
		  (save-excursion
		    (while (re-search-forward org-clock-line-re end t)
		      (when (org-at-clock-log-p) (throw :clock t))))))
	(org-entry-put nil "LAST_REPEAT" (format-time-string
					  (org-time-stamp-format t t))))

      (when org-log-repeat
	(if (or (memq 'org-add-log-note (default-value 'post-command-hook))
		(memq 'org-add-log-note post-command-hook))
	    ;; We are already setup for some record.
	    (when (eq org-log-repeat 'note)
	      ;; Make sure we take a note, not only a time stamp.
	      (setq org-log-note-how 'note))
	  ;; Set up for taking a record.
	  (org-add-log-setup 'state
			     (or done-word (car org-done-keywords))
			     org-last-state
			     org-log-repeat)))

      (when org-log-repeat
	(if (or (memq 'org-add-log-note (default-value 'post-command-hook))
		(memq 'org-add-log-note post-command-hook))
	    ;; We are already setup for some record.
	    (when (eq org-log-repeat 'note)
	      ;; Make sure we take a note, not only a time stamp.
	      (setq org-log-note-how 'note))
	  ;; Set up for taking a record.
	  (org-add-log-setup 'state
			     (or done-word (car org-done-keywords))
			     org-last-state
			     org-log-repeat)))

      ;; Time-stamps without a repeater are usually skipped.  However,
      ;; a SCHEDULED time-stamp without one is removed, as they are no
      ;; longer relevant.
      (save-excursion
	(let ((scheduled (org-entry-get (point) "SCHEDULED")))
	  (when (and scheduled (not (string-match-p org-repeat-re scheduled)))
	    (org-remove-timestamp-with-keyword org-scheduled-string))))

      (ledna/set-scheduled next-time target)
      (ledna/set-todo-state todo-word target))))

(defun ledna/get-nearest-date (times)
  (let ((current-sec (time-to-seconds (org-current-time))))
    (cl-flet* ((diff (time)
                     (let* ((target-sec (org-time-string-to-seconds (active-timestamp time)))
                            (diff-sec (- target-sec current-sec)))
                       (cond ((and (> diff-sec 0) (< diff-sec 604800)) diff-sec)
                             ((< diff-sec 0) (+ diff-sec 604800))
                             ((> diff-sec 604800) (- diff-sec 604800)))))
               (comparator (a b) (< (diff a) (diff b))))
      (elt (sort times #'comparator) 0))))

(defun active-timestamp (str)
  (let* ((default-time (org-current-time))
         (decoded-time (decode-time default-time nil))
         (analyzed-time (org-read-date-analyze str default-time decoded-time))
         (encoded-time (apply #'encode-time analyzed-time)))
    (format-time-string (org-time-stamp-format t) encoded-time)))

(defun inactive-timestamp (str)
  (let* ((default-time (org-current-time))
         (decoded-time (decode-time default-time nil))
         (analyzed-time (org-read-date-analyze str default-time decoded-time))
         (encoded-time (apply #'encode-time analyzed-time)))
    (format-time-string (org-time-stamp-format t t) encoded-time)))

(defun ledna/set-scheduled (timestamp &optional marker)
  (let ((mark (or marker (ledna/$self))))
    (save-mark-and-excursion
     (cl-labels
      ((set-scheduled-on (mts)
                         (let ((pom (car mts)) (ts (cdr mts)))
                           (with-current-buffer
                               (marker-buffer pom)
                             (goto-char pom)
                             (org-add-planning-info 'scheduled ts)
                             ts))))
    (mapcar #'set-scheduled-on (-zip mark (-repeat (length mark) timestamp)))))))

(defun ledna/set-deadline (timestamp &optional marker)
  (let ((mark (or marker (ledna/$self))))
    (save-mark-and-excursion
     (cl-labels
      ((set-scheduled-on (mts)
                         (let ((pom (car mts)) (ts (cdr mts)))
                           (with-current-buffer
                               (marker-buffer pom)
                             (goto-char pom)
                             (org-add-planning-info 'deadline ts)
                             ts))))
      (mapcar #'set-scheduled-on (-zip mark (-repeat (length mark) timestamp)))))))

(let ((ledna-reserved-properties (quote (("ledna-props-count" "DONE_COUNT" "int" "Default counter property" 1) ("ledna-props-schedule" "ADVANCED_SCHEDULE" "list<string>" "Describe repeated scheduling" "[\"Mon 15:00\" \"Wed\" \"Fri 18:00\"]") ("ledna-props-template" "HEADLINE_TEMPLATE" "string" "Header prototype template" "${ledna-times} English class") ("ledna-props-archive" "ARCHIVE_ENTRY_P" "bool" "Archive entry if t" "t") ("ledna-props-kill" "KILL_ENTRY_P" "bool" "Kill entry if t" "t") ("ledna-props-cleanup" "CLEANUP_ENTRY_PROPS" "bool or list<string>" "Delete entry props if t or props specified" "'(\"_PRICE\" \"_PASSED\" \"_COUNT\")") ("ledna-props-cycle" "CYCLE_ENTRY_PROPS" "list<string>" "Cycle prop values over allowed in PROP_ALL header" "'(\"MONTH\" \"TRAIN_TYPE\")")))))
(loop for (symbol name type descr example) in ledna-reserved-properties
      do (eval (macroexpand (list 'defconst (intern symbol) name
                                  (format "%s. Type = %s." descr type)))))
)

(defun ledna/ensure-prop (prop)
  (unless (org-entry-get nil prop)
    (org-set-property prop (read-string (format "Set %s: " prop)))))

;; priority list of magic tags
;; greater priorities mean latter execution
(setq ledna/magic-tags
      '(;; Tag                Status       Handler                               Priority

        ;; (  Pending_Inherit   ((*->PENDING (ledna/set-todo-state "PENDING" (ledna/$parent)))
        ;;                       (PENDING->* (ledna/set-todo-state "TODO"    (ledna/$parent)))) 1)

        ;; (  *                  ((*->DONE     (ledna/consider-effort-as-clocktime)))           5)

        ;; Constructors
        (  Cycle_Props       ((->TODO       (ledna/cycle-props)))                            1)
        (  Rename            ((->TODO       (ledna-entry-name-from-template)))               1)

        ;; Destructors
        (  Effort_Clock      ((*->DONE (ledna/consider-effort-as-clocktime)))                5)
        (  Advanced_Schedule ((*->DONE      (ledna/advanced-schedule))
                              (*->CANCELLED (ledna/advanced-schedule))
                              (+Advanced_Schedule (ledna/ensure-prop "ADVANCED_SCHEDULE")))  10)

        ;; Uncertain destructors
        (  Cleanup_Maybe      ((*->DONE      (ledna/cleanup-maybe-defer))
                               (*->CANCELLED (ledna/cleanup-maybe-defer)))                   1)
        (  Kill_Maybe         ((*->DONE      (ledna/kill-subtree-maybe-defer))
                               (*->CANCELLED (ledna/kill-subtree-maybe-defer)))              1)
        (  Forget_Unnecessary ((*->CANCELLED (ledna/kill-subtree-maybe-defer)))              1)
        (  Archive_Maybe      ((*->DONE      (ledna/archive-subtree-maybe-defer))
                               (*->CANCELLED (ledna/archive-subtree-maybe-defer)))           1)

        ;; User-defined properties are executed with priority = 100

        ;; So do not confuse yourself:
        ;; use tags that change properties after user-defined triggers.
        (  Counter           ((*->DONE      (ledna/inc-property ledna-props-count)))         110)

        (  Clone             ((*->DONE      (ledna-clone))
                              (*->CANCELLED (ledna-clone)))                                  120)

        ;; Removing entry properties
        ;; Warning! Tags with priority > 1000 don't have access to special properties
        (  Cleanup           ((*->DONE      (ledna/cleanup-properties))
                              (*->CANCELLED (ledna/cleanup-properties)))                     1000)

        ;; Deferred destructors
        (  Kill              ((*->DONE      (ledna/defer 'ledna/org-kill-subtree))
                              (*->CANCELLED (ledna/defer 'ledna/org-kill-subtree)))          1001)

        (  Archive           ((*->DONE      (ledna/defer 'org-archive-subtree))
                              (*->CANCELLED (ledna/defer 'org-archive-subtree)))             1001)))

(setq ledna/complex-tags
      '(;; Complex tag         Features
        (  Repeated_Task     ( Advanced_Schedule Effort_Clock
                               Rename Forget_Unnecessary Cycle_Props))
        (  Reminder          ( Advanced_Schedule Kill))))

(defun ledna/tags-prioritized (tags)
  (loop for (name (status header) priority)
        in (ledna/magic-tags-sorted)
        when (member name tags)
        collect (list name priority)))

(defun ledna/magic-tag-get-priority (tag)
  (cadr (alist-get 'Cleanup ledna/magic-tags)))

(defun ledna/magic-tags-sorted ()
  (sort ledna/magic-tags #'(lambda (a b) (< (caddr a) (caddr b)))))

(defun ledna/magic-tags-list ()
  (mapcar #'car (ledna/magic-tags-sorted)))

(defun ledna/complex-tags-list ()
  (mapcar #'car ledna/complex-tags))

(defun ledna/cleanup-properties (&optional pom)
  (if-let ((cleanup-prop (ledna/get-property ledna-props-cleanup)))
      (if (listp cleanup-prop)
          (mapc #'(lambda (p) (org-delete-property p))
                cleanup-prop)
        (mapc #'(lambda (p) (let ((pname (car p))) (org-delete-property pname)))
              (org-entry-properties nil 'standard)))))

(defun ledna/cleanup-maybe-defer ()
  (ledna/defer #'ledna/cleanup-properties))

(defun ledna/kill-subtree-maybe-defer ()
  (when (string= (ledna/get-property ledna-props-kill) "t")
    (ledna/defer #'ledna/org-kill-subtree)))

(defun ledna/archive-subtree-maybe-defer ()
  (when (string= (ledna/get-property ledna-props-archive) "t")
    (ledna/defer #'org-archive-subtree)))

(defmacro ledna-counter (countable counter &optional target unit)
  `(when-let (inc (cond ((stringp ,countable) (ledna/get-property ,countable ,target))
                        ((numberp ,countable) ,countable)))
     (ledna/inc-property ,counter inc ,unit ,target)))

(defun ledna-price-counter (&optional target unit)
  (ledna-counter "PRICE" "Money" target unit))

(defun ledna-time-counter (&optional target)
  (ledna-counter "DURATION" "Time" target "hours"))

(defun ledna-times-counter (&optional target)
  (ledna-counter 1 "Times" target "times"))

(defun ledna-touch (&optional target)
  (ledna/set-scheduled (active-timestamp "now") target)
  (ledna/set-todo-state "TODO" target))

(defun ledna-money-time-report (&optional target)
  (ledna-time-counter target)
  (ledna-price-counter target)
  (ledna-times-counter target))

(provide 'ledna)
