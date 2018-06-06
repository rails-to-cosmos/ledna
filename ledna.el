;; priority list of magic tags
;; greater priorities mean latter execution
(setq ledna/magic-tags
      '(;; Tag                Status       Handler                               Priority

        ;; Constructors
        (  Advanced_Schedule ((->TODO     (ledna-advanced-schedule)))            1)
        (  Rename            ((->TODO     (ledna-entry-name-from-template)))     1)

        ;; Destructors
        (  Classwork         ((*->DONE    (set-hometask-deadline)))              1)
        (  Effort_Clock      ((TODO->DONE (ledna/consider-effort-as-clocktime))) 1)
        (  Counter           ((*->DONE    (inc-property "$COUNT")))              1)

        (  Clone             ((*->DONE    (ledna-clone)))                        10)

        ;; User-defined properties are executed with priority = 100

        (  Cleanup           ((*->DONE    (delete-entry-properties)))            1000)))

(setq ledna/complex-tags
      '(;; Complex tag       Features
        (  Repeated_Task     (Advanced_Schedule Clone Cleanup Effort_Clock Counter Rename))))

(defun ledna/magic-tags-sorted ()
  (sort ledna/magic-tags #'(lambda (a b) (< (caddr a) (caddr b)))))

(defun ledna/magic-tags-list ()
  (mapcar #'car (ledna/magic-tags-sorted)))

(defun ledna/complex-tags-list ()
  (mapcar #'car ledna/complex-tags))

(defun ledna/eval-forms (forms)
  (unwind-protect
      (mapc #'(lambda (f) (when f (eval (read f)))) forms)
    t))

(defun ledna/get-entry-values-by-keys (pom keys)
  (remove 'nil (mapcar #'(lambda (f) (org-entry-get pom f org-edna-use-inheritance)) keys)))

(defun ledna/apply-magic-tag (tag keys)
  (let* ((mt-params (alist-get tag ledna/magic-tags))
         (mt-priority (cadr mt-params))
         (mt-props (car mt-params)))
    (loop for key in keys
          do (when-let (magic-form (car (alist-get (intern key) mt-props)))
               (eval magic-form)))
    mt-priority))

(defun apply-ledna-forms (entry-keys pom)
  (ledna-run change-plist

    (let (entry-props-evaled-p
          (mtags-list (ledna/magic-tags-list))
          (cpx-tags-list (ledna/complex-tags-list))
          (entry-vals (ledna/get-entry-values-by-keys pom entry-keys))
          (src-org-tags (mapcar #'intern (org-get-tags))))

      (cl-flet ((apply-magic-tag-consider-priority (tag keys vals)
                    (let ((mt-priority (ledna/apply-magic-tag tag keys)))
                      (when (and entry-vals
                                 (not entry-props-evaled-p)
                                 (>= mt-priority 100))
                        (ledna/eval-forms vals)
                        (setq entry-props-evaled-p t)))))

        ;; Process complex tags
        (dolist (src-tag src-org-tags)
          (when (member src-tag cpx-tags-list)
            (let* ((sm-tags (car (alist-get src-tag ledna/complex-tags))))
              (dolist (tag mtags-list)
                (when (member tag sm-tags)
                  (apply-magic-tag-consider-priority tag entry-keys entry-vals))))))

        ;; Process simple tags
        (dolist (tag mtags-list)
          (when (member tag src-org-tags)
            (apply-magic-tag-consider-priority tag entry-keys entry-vals))))

      (when (and entry-vals (not entry-props-evaled-p))
        (ledna/eval-forms entry-vals)))))

(defun ledna-trigger-function-emacs-lisp (change-plist)
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

(defun ledna-dsl-init (&optional dsl)
  (let ((dsl (or dsl 'ledna)))
    (defvar ledna-dsl dsl "Language that edna uses for triggers and blockers.")
    (defvar ledna-dsl-trigger-handler "Org-edna custom trigger wrapper.")
    (defvar ledna-dsl-blocker-handler "Org-edna custom blocker wrapper.")

    (setq-default ledna-dsl dsl
                  ledna-dsl-trigger-handler (case dsl
                                              ('ledna #'ledna-trigger-function)
                                              ('emacs-lisp #'ledna-trigger-function-emacs-lisp))
                  ledna-dsl-blocker-handler (case dsl
                                              ('ledna #'ledna-blocker-function)
                                              ('emacs-lisp #'ledna-blocker-function-emacs-lisp)))

    (advice-add 'org-edna-trigger-function :around #'ledna-dsl-specifier-trigger)
    (advice-add 'org-edna-blocker-function :around #'ledna-dsl-specifier-blocker)))

(defun ledna-dsl-specifier-trigger (orig-fun &rest args)
  "Wrap edna's triggers.

ORIG-FUN is a trigger function called with ARGS."
  (apply ledna-dsl-trigger-handler args))

(defun ledna-dsl-specifier-blocker (orig-fun &rest args)
  "Wrap edna's blockers.

ORIG-FUN is a blocker function called with ARGS."
  (apply ledna-dsl-blocker-handler args))

(defun string-is-numeric-p (string)
  "Return non-nil if STRING is a valid numeric string.

Examples of valid numeric strings are \"1\", \"-3\", or \"123\"."
  ;; Can't use string-to-number, because it returns 0 if STRING isn't a
  ;; number, which is ambiguous.
  (numberp (car (read-from-string string))))

(defun ledna-entry-name-from-template ()
  (when-let ((template (or (get-property "$TEMPLATE") (cdr (assoc-string "ITEM" (org-entry-properties))))))
    (org-back-to-heading)
    (org-beginning-of-line)
    (org-kill-line)

    (let ((entry-name-format template)
          (entry-name-fmt-args  (list
                                 (cons "ledna-times"
                                       (num-with-ordinal-indicator
                                        (string-to-number
                                         (or (get-property "$COUNT") "1")))))))
      (insert (s-format entry-name-format 'aget entry-name-fmt-args)))))

(require 's)

(defun ledna-clone (&rest args)
  (save-excursion
    (org-back-to-heading)

    (let* ((src-entry             (or (plist-get args :source)       (self)))
           (src-props             (org-entry-properties))
           (src-tags-string       (org-get-tags-string))

           (todo-state            (or (plist-get args :todo-state)   "TODO"))
           (target-props          (or (plist-get args :properties)   (mapcar #'car (org-entry-properties nil 'standard)))))

      (org-insert-heading-respect-content)
      (insert (cdr (assoc-string "ITEM" src-props)) " " src-tags-string)

      ;; Copy properties
      (mapc #'(lambda (prop)
                (when-let (p (assoc-string prop src-props))
                    (condition-case nil
                        (set-property (car p) (cdr p))
                      (error nil))))
            target-props)

      (set-todo-state todo-state))))

(defun set-property (property value &optional target)
  (dolist (mark (or target (self)))
    (org-entry-put
     mark property
     (cond ((numberp value) (number-to-string value))
           ((stringp value) value)
           (t "Unknown value type")))))

(defun get-property (property &optional target default)
  (let ((mark (cond
               (target
                (cond
                 ((listp target) (car target))
                 (t target)))
          (t (car (self))))))
    (or (org-entry-get mark property)
        default)))

(defun inc-property (property &optional val units target)
  (dolist (mark (or target (self)))
    (let* ((full-prop-value (get-property property mark "0"))
           (inc-value (cond ((and (stringp val) (string-is-numeric-p val)) (string-to-number val))
                            ((numberp val) val)
                            (t 1)))
           (prop-number (string-to-number (car (split-string full-prop-value))))
           (prop-label (or units (key-description (cdr (split-string full-prop-value)))))
           (result-value (s-trim (concat (number-to-string (+ inc-value prop-number)) " " prop-label))))
      (set-property property result-value (list mark))
      result-value)))

(defun inc-property-get (property &rest args)
  (apply #'inc-property (append (list property) args))
  (get-property property))

(defun delete-entry-properties (&optional pom)
  (mapc #'(lambda (p) (org-delete-property (car p)))
        (org-entry-properties nil 'standard)))

(defun get-todo-state (&optional marker)
  (let ((mark (car (or marker (self)))))
    (save-excursion
      (with-current-buffer (marker-buffer mark)
        (goto-char mark)
        (substring-no-properties (org-get-todo-state))))))

(defun set-todo-state (state &optional marker)
  (let ((mark (car (or marker (self)))))
    (save-mark-and-excursion
      (with-current-buffer (marker-buffer mark)
        (goto-char mark)
        (org-todo state)))))

(defun self ()
  (save-excursion
    (org-back-to-heading)
    (list (point-marker))))

(defun ids (&rest ids)
  "Find a list of headings with given IDS.

Edna Syntax: ids(ID1 ID2 ...)

Each ID is a UUID as understood by `org-id-find'.

Note that in the edna syntax, the IDs don't need to be quoted."
  (mapcar (lambda (id) (org-id-find id 'marker)) ids))

(defun tags (match-spec &optional scope skip)
  "Find entries using Org matching.

Edna Syntax: match(\"MATCH-SPEC\" SCOPE SKIP)

MATCH-SPEC may be any valid match string; it is passed straight
into `org-map-entries'.

SCOPE and SKIP are their counterparts in `org-map-entries'.
SCOPE defaults to agenda, and SKIP defaults to nil.

* TODO Test
  :PROPERTIES:
  :BLOCKER:  match(\"test&mine\" agenda)
  :END:

\"Test\" will block until all entries tagged \"test\" and
\"mine\" in the agenda files are marked DONE."
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

(defun ledna/consider-effort-as-clocktime ()
  (if-let (entry-effort (get-property "EFFORT"))
      (save-excursion
        (save-restriction
          (org-clock-find-position org-clock-in-resume)
          (insert-before-markers "\n")
          (backward-char 1)
          (org-indent-line)
          (when (and (save-excursion (end-of-line 0) (org-in-item-p)))
            (beginning-of-line 1)
            (indent-line-to (- (org-get-indentation) 2)))
          (insert org-clock-string " ")

          (let ((scheduled-time (org-get-scheduled-time (org-entry-beginning-position))))
            (org-insert-time-stamp scheduled-time 'with-hm 'inactive)
            (insert "--")
            (org-insert-time-stamp (seconds-to-time (+ (time-to-seconds scheduled-time)
                                                       (* (org-duration-to-minutes entry-effort) 60)))
                                   'with-hm 'inactive)
            (org-clock-update-time-maybe))))))

(defun ledna-advanced-schedule (&optional target)
  (when-let (schedule-prop (get-property "$SCHEDULE"))
    (let* ((schedule (cadr (read schedule-prop)))
           (next-time (get-nearest-date schedule)))
      (set-scheduled next-time target)
      (set-todo-state "TODO" target)
      (org-entry-put nil "LAST_REPEAT" (format-time-string
					      (org-time-stamp-format t t)
					      (current-time))))))

(defun get-nearest-date (times)
  (cl-flet* ((diff (time)
                   (let* ((current-sec (time-to-seconds (org-current-time)))
                          (target-sec (org-time-string-to-seconds (active-timestamp time)))
                          (diff-sec (- target-sec current-sec)))
                     (cond ((and (> diff-sec 0) (< diff-sec 604800)) diff-sec)
                           ((< diff-sec 0) (+ diff-sec 604800))
                           ((> diff-sec 604800) (- diff-sec 604800)))))
             (comparator (a b) (< (diff a) (diff b))))
    (let ((nearest-date (car (sort times #'comparator))))
      nearest-date)))

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

;; (set-keyword "SCHEDULED" (active-timestamp (get-nearest-date (cdr (read (get-property "SCHEDULE" (car (ids "test-event"))))))) (select (ids "test-event")))
;; (set-scheduled (get-nearest-date (cdr (read (get-property "SCHEDULE" (car (ids "test-event")))))) (select (ids "test-event")))
;; (active-timestamp (get-nearest-date (cadr (read (get-property "SCHEDULE" (car (ids "test-event")))))))
;; (get-nearest-date (list "Mon 09:00" "Mon 10:00" "Mon 12:00" "Mon 21:00" "Tue 17:00-18:00" "Thu 17:00-18:00" "Sat 13:00-14:00"))
;; (- (org-time-string-to-seconds (active-timestamp "Mon 09:00")) (time-to-seconds (org-current-time)))

(defun set-scheduled (timestamp &optional marker)
  (let ((mark (or marker (self))))
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

(defun set-deadline (timestamp &optional marker)
  (let ((mark (or marker (self))))
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

(defun set-hometask-deadline ()
  (when-let (hometask-entries (select (tags (get-property "$HOMETASK"))))
    (when-let (schedule-prop (get-property "$SCHEDULE"))
      (let* ((schedule (cadr (read schedule-prop)))
             (next-time (get-nearest-date schedule)))
        (set-deadline next-time hometask-entries)))))

(defmacro ledna-counter (countable counter &optional target unit)
  `(when-let (inc (cond ((stringp ,countable) (get-property ,countable ,target))
                        ((numberp ,countable) ,countable)))
     (inc-property ,counter inc ,unit ,target)))

(defun ledna-price-counter (&optional target unit)
  (ledna-counter "PRICE" "Money" target unit))

(defun ledna-time-counter (&optional target)
  (ledna-counter "DURATION" "Time" target "hours"))

(defun ledna-times-counter (&optional target)
  (ledna-counter 1 "Times" target "times"))

(defun ledna-touch (&optional target)
  (set-scheduled (active-timestamp "now") target)
  (set-todo-state "TODO" target))

(defun ledna-money-time-report (&optional target)
  (ledna-time-counter target)
  (ledna-price-counter target)
  (ledna-times-counter target))

(provide 'ledna)
