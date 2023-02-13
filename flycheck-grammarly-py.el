(require 'cl-lib)
(require 'json)
(require 'dom)

(require 'flycheck)

(defgroup flycheck-grammarly-py nil
  "Grammarly support for Flycheck."
  :prefix "flycheck-grammarly-py-"
  :group 'flycheck)

(defcustom flycheck-grammarly-py-active-modes
  '(latex-mode org-mode markdown-mode)
  "List of major mode that work with Grammarly."
  :type 'list
  :group 'flycheck-grammarly-py)

(defcustom flycheck-grammarly-py-check-time 0.8
  "How long do we call request after we done typing."
  :type 'float
  :group 'flycheck-grammarly-py)

(defcustom flycheck-grammarly-py-hostname "localhost"
  "Host to bind the `grammarly-py' service."
  :type 'string
  :group 'flycheck-grammarly-py)

(defcustom flycheck-grammarly-py-port 9292
  "Port where the `grammarly-py' service listens."
  :type 'number
  :group 'flycheck-grammarly-py)

;; For #3
(defconst flycheck-grammarly-py-avoidance-rule
  '((":" . "\n"))
  "Replace character to another character to avoid from Grammarly API.")

(defvar flycheck-grammarly-py-log-level 2
  "Log level for logging.

3 = debug
2 = info
1 = error.")

(defvar flycheck-grammarly-py-log-data nil
  "Log list for `flycheck-grammarly-py'.")

(defvar-local flycheck-grammarly-py-done-checking nil
  "Check if Grammarly API done checking.")

(defvar-local flycheck-grammarly-py-point-data nil
  "List of error/warning JSON data.")

(defvar-local flycheck-grammarly-py-last-buffer-string nil
  "Record the last buffer string.")

(defvar flycheck-grammarly-py-check-strings nil
  "Alist of all check strings.")

(defvar-local flycheck-grammarly-py-request-timer nil
  "Timer that for sending check requests.")

(defvar-local flycheck-grammarly-py-result-timer nil
  "Timer for fetching results and displaying errors.")

(defvar-local flycheck-grammarly-py-flycheck-callback nil
  "Callback for displaying errors.")

(defvar-local flycheck-grammarly-py-flycheck-checker nil
  "Checker context (I guess) for flycheck.")

(defvar-local flycheck-grammarly-py-flycheck-checker-finished nil
  "Variable to check if checker finished for buffer.")

(defvar-local flycheck-grammarly-py-fetch-no-more nil)

(defun flycheck-grammarly-py-log (loglevel fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (push fmt flycheck-grammarly-py-log-data)
  (let ((loglevel (or loglevel flycheck-grammarly-py-log-level)))
    (when (> loglevel 1)
      (apply 'message fmt args))
    (cond ((listp args)
           (push args flycheck-grammarly-py-log-data))
          ((stringp args)
           (push (json-read-from-string args) flycheck-grammarly-py-log-data))
          ((and (listp args) (stringp (cadr args)))
           (push (json-read-from-string (cadr args)) flycheck-grammarly-py-log-data)))))

(defsubst flycheck-grammarly-py-debug (fmt &rest args)
  (flycheck-grammarly-py-log 3 fmt args))

(defsubst flycheck-grammarly-py-info (fmt &rest args)
  (flycheck-grammarly-py-log 2 fmt args))

(defun flycheck-grammarly-py-start-grammarly ()
  (apply #'start-process "grammarly-py" "*grammarly-py*" "python"
         `("/home/joe/lib/grammarly/grammarly.py"
           ,flycheck-grammarly-py-hostname
           ,(format "%s" flycheck-grammarly-py-port))))

(defun flycheck-grammarly-py-post-cmd-hook ()
  (pcase-let ((`(,prev-beg ,prev-end) flycheck-grammarly-py-previous-check-string-bounds)
              (`(,current-beg ,current-end) (flycheck-grammarly-py-check-string-bounds)))
    (unless (and (= prev-beg current-beg) (= prev-end current-end))
      )))

(defun flycheck-grammarly-py-org-text-bounds ()
  "Return bounds of text body if present in org subtree.

Return value is a triple of \\='(beg end has-body) where beg is the
point after metadata, end is the point at end of subtree and
has-body indicates if any text is present."
  (save-excursion
    (let* ((beg (or (org-end-of-meta-data t) (point)))
           (end (progn
                  (unless (org-at-heading-p)
                    (outline-next-heading))
                  (point)))
           (has-body (not (string-empty-p
                           (string-trim
                            (buffer-substring-no-properties beg end))))))
      (list beg end has-body))))

(defun flycheck-grammarly-py-check-string-bounds ()
  "Return the string to check with grammarly.

Passing large buffers to grammarly can cause fewer grammar errors
to be reported and large number of flycheck errors also cause
issues.  Instead we pass only the current paragraph or
org subtree if in `org-mode'."
  (pcase major-mode
    ('org-mode
     (pcase-let ((`(,beg ,end ,has-body) (flycheck-grammarly-py-org-text-bounds)))
       (when has-body (list beg end))))
    (_ (let* ((newlines (looking-at-p "\n\n"))
              (separating (-all? #'looking-at-p `(,paragraph-start ,paragraph-separate)))
              (beg (save-excursion (if (and separating (not newlines))
                                       (progn (re-search-forward "\n" nil t)
                                              (- (point-at-bol) 1))
                                     (re-search-backward "\n\n" nil t)
                                     (+ (point) 1))))
              (end (save-excursion (if (and separating newlines)
                                       (+ (point) 1)
                                     (re-search-forward "\n\n" nil t)
                                     (- (point) 1)))))
         (list beg end)))))

(defun flycheck-grammarly-py-check-string ()
  "Return the string to check with grammarly.

Passing large buffers to grammarly can cause fewer grammar errors
to be reported and large number of flycheck errors also cause
issues.  Instead we pass only the current paragraph or
org subtree if in `org-mode'.

See `flycheck-grammarly-py-check-string-bounds'."
  (pcase-let ((`(,beg ,end) (flycheck-grammarly-py-check-string-bounds)))
    (when (and beg end)
      (buffer-substring-no-properties beg end))))

(defun flycheck-grammarly-py-get-offset ()
  (pcase major-mode
    ('org-mode
     (pcase-let ((`(,beg ,end ,has-body) (ref-man-org-text-bounds)))
       beg))
    (_ (save-excursion (if (and separating (not newlines))
                           (progn (re-search-forward "\n" nil t)
                                  (- (point-at-bol) 1))
                         (re-search-backward "\n\n" nil t)
                         (+ (point) 1))))))

(defun flycheck-grammarly-py-column-at-pos (&optional pt)
  "Column at PT."
  (unless pt (setq pt (point)))
  (save-excursion (goto-char pt) (current-column)))

(defun flycheck-grammarly-py-after-change-functions (&rest _)
  "After change function to check if content change."
  (unless (string=
           (flycheck-grammarly-py-checksum flycheck-grammarly-py-last-buffer-string)
           (flycheck-grammarly-py-checksum (flycheck-grammarly-py-check-string)))
    (flycheck-grammarly-py-kill-timer)
    (setq flycheck-grammarly-py-request-timer
          (run-with-idle-timer flycheck-grammarly-py-check-time nil
                               'flycheck-grammarly-py-reset-request))))

(defun flycheck-grammarly-py-check-all ()
  "Check grammar for buffer document."
  (message "Annotating grammarly region...")
  (let ((offset (flycheck-grammarly-py-get-offset))
        (flycheck-grammarly-py-point-data
         (-filter (lambda (x) (a-get x 'highlightBegin)) flycheck-grammarly-py-point-data))
        check-list)
    (dolist (data flycheck-grammarly-py-point-data)
      (let* ( ; narrowed buffer
             (pt-beg (+ offset (a-get data 'highlightBegin)))
             (pt-end (+ offset (a-get data 'highlightBegin)))
             (ln (line-number-at-pos pt-beg t))
             (col-start (flycheck-grammarly-py-column-at-pos pt-beg))
             (col-end (flycheck-grammarly-py-column-at-pos pt-end))
             (exp (a-get data 'explanation))
             (card-desc (unless exp (a-get* data 'cardLayout 'groupDescription)))
             (desc (flycheck-grammarly-py-html-to-text (or exp card-desc "")))
             (type (if exp (if (string-match-p "error" (json-encode data)) 'error 'warning) 'info))

             (parsed-data data)
             (transforms (a-get parsed-data 'transforms))
             (beg (a-get parsed-data 'highlightBegin))
             (end (a-get parsed-data 'highlightEnd))
             (text (when (and beg end)
                     (substring-no-properties grammarly-text beg end)))
             (context (list (a-get* parsed-data 'cardContext 's)
                            (a-get* parsed-data 'cardContext 'e)))
             (context (when (-all? 'identity context)
                        (apply 'substring-no-properties
                               grammarly-text context)))
             (highlighted (a-get parsed-data 'highlightText))
             (title (a-get parsed-data 'title))
             (splits (when transforms
                       (seq-map (lambda (x) (split-string
                                             (flycheck-grammarly-py-html-to-text x)))
                                transforms)))
             (replacements (a-get parsed-data 'replacements))
             (suggestions (when replacements
                            (if (string-empty-p (a-get parsed-data 'text))
                                (string-join (seq-map (lambda (x)
                                                        (concat x highlighted))
                                                      replacements)
                                             ", ")
                              (string-join (seq-map (lambda (x)
                                                        (replace-regexp-in-string (a-get parsed-data 'text)
                                                                                  x highlighted))
                                                      replacements)
                                           ", "))))
             (desc (if splits (concat
                               ;; (mapconcat
                               ;;  (lambda (x) (format "%s -> %s" (car x) (-last-item x)))
                               ;;  splits "\n")
                               ;; "\n"
                               highlighted " -> " suggestions
                               "\n"
                               (string-replace "\\302\\240" " " title)
                               "\n"
                               desc)
                     desc)))
        (when transforms (push transforms my/flycheck-grammarly-py-transforms))
        (when transforms (push parsed-data my/flycheck-grammarly-py-data))
        (push (list ln col-start type desc :end-column col-end) check-list)))
    check-list))

(defun flycheck-grammarly-py-apply-avoidance-rule (str)
  "Apply avoidance rule to STR."
  (dolist (rule flycheck-grammarly-py-avoidance-rule)
    (setq str (s-replace (car rule) (cdr rule) str)))
  str)

(defsubst flycheck-grammarly-py-url (path &optional opts)
  "Copied from `ref-man-py-url'.

Optional PATH is the path after url to fetch.
Optional OPTS is an alist of additional HTTP args to send."
  (declare (pure t) (side-effect-free t))
  (format "http://%s:%s/%s%s"
          flycheck-grammarly-py-hostname
          flycheck-grammarly-py-port
          (or path "")
          (or (and opts (concat "?"
                                (mapconcat
                                 (lambda (x) (format "%s=%s" (car x) (cdr x)))
                                 opts "&")))
              "")))

(defun flycheck-grammarly-py-post-json-synchronous (url data &optional silent)
  "Copied from `ref-man--post-json-synchronous'.

Send an HTTP POST request to URL with DATA.
DATA should be an alist of key-value pairs.  The request is sent
content-type as application/json and DATA is encoded as json."
  (let ((url-request-extra-headers
         `(("Content-Type" . "application/json")))
        (url-request-method "POST")
        (url-request-data
         (encode-coding-string (json-encode-alist data) 'utf-8)))
    (url-retrieve-synchronously url silent)))

(defun flycheck-grammarly-py-request-buffer (path str-or-num)
  (pcase-let* ((`(,check-args ,type)
                (pcase str-or-num
                  ((pred stringp) `((("text" . ,str-or-num)) str))
                  ((pred numberp) `((("num" . ,str-or-num)) num))
                  (_ (error "Incorrect str-or-num"))))
               (url (pcase type
                      ('num (flycheck-grammarly-py-url path check-args))
                      ('str (flycheck-grammarly-py-url path))
                      (_ nil)))
               (buf (pcase type
                      ('num (url-retrieve-synchronously url))
                      ('str (flycheck-grammarly-py-post-json-synchronous url check-args))
                      (_ nil))))
    buf))

(defun flycheck-grammarly-py-send-check-request (str)
  (unless (member str flycheck-grammarly-py-check-strings)
    (let ((url (flycheck-grammarly-py-url "check"))
          (data `(("text" . ,str))))
      (flycheck-grammarly-py-info "Sending string to check %s %s" (current-buffer))
      (let ((buf (with-current-buffer (flycheck-grammarly-py-post-json-synchronous url data))))
        (message "%s" (with-current-buffer buf
                        (goto-char (point-min))
                        (forward-paragraph)
                        (json-read))))))
  (push str flycheck-grammarly-py-check-strings))

(defun flycheck-grammarly-py-get-result ()
  (flycheck-grammarly-py-info "Getting results in %s" (current-buffer))
  (let ((check-string (flycheck-grammarly-py-check-string))
        (finished flycheck-grammarly-py-flycheck-checker-finished))
    ;; Call the results one last time
    (unless flycheck-grammarly-py-fetch-no-more
      (let* ((buf (flycheck-grammarly-py-request-buffer "results" check-string))
             (result (with-current-buffer buf
                       (goto-char (point-min))
                       (forward-paragraph)
                       (json-read)))
             (callback flycheck-grammarly-py-flycheck-callback)
             (checker flycheck-grammarly-py-flycheck-checker))
        (when callback
          (funcall
           callback 'finished
           (flycheck-increment-error-columns
            (mapcar
             (lambda (x)
               (apply #'flycheck-error-new-at `(,@x :checker ,checker)))
             (condition-case err
                 (if flycheck-grammarly-py-done-checking
                     (flycheck-grammarly-py-check-all)
                   (flycheck-stop))
               (error (funcall callback 'errored (error-message-string err))
                      (signal (car err) (cdr err))))))))))
    (when finished
      (setq flycheck-grammarly-py-fetch-no-more t))
    ;; TODO: Annotate buffer with flycheck here
    ))

(defun flycheck-grammarly-py-finished-p (arg)
  (let ((buf (flycheck-grammarly-py-request-buffer "check_finished" arg))
        (finished (with-current-buffer buf
                    (goto-char (point-min))
                    (forward-paragraph)
                    (json-read))))
    (setq flycheck-grammarly-py-flycheck-checker-finished
          (pcase finished
            (t t)
            (_ nil)))))

(defun flycheck-grammarly-py-grammar-check ()
  "Grammar check with `grammarly-py' once."
  (flycheck-grammarly-py-info "Initializing Grammar Check in %s" (current-buffer))
  (let ((check-string (flycheck-grammarly-py-check-string)))
    (flycheck-grammarly-py-send-check-request check-string)))

(defun flycheck-grammarly-py-start (checker callback)
  "Flycheck start function for CHECKER, invoking CALLBACK."
  (when (memq major-mode flycheck-grammarly-py-active-modes)
    (flycheck-grammarly-py-info "Starting flycheck-grammarly-py in %s" (current-buffer))
    (setq flycheck-grammarly-py-request-timer
          (run-with-timer 0 5 'flycheck-grammarly-py-grammar-check))
    (setq flycheck-grammarly-py-result-timer
          (run-with-timer 0 5 'flycheck-grammarly-py-get-result))
    (setq flycheck-grammarly-py-flycheck-callback callback)
    (setq flycheck-grammarly-py-flycheck-checker checker)))

(flycheck-define-generic-checker 'grammarly-py
  "Grammarly flycheck definition with python process."
  :start #'flycheck-grammarly-py-start
  :modes flycheck-grammarly-py-active-modes)

(defun flycheck-grammarly-py-setup ()
  "Setup flycheck-package."
  (interactive)
  (add-to-list 'flycheck-checkers 'grammarly-py))
