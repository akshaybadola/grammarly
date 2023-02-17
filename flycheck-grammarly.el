;;; flycheck-grammarly.el --- Grammarly support for Flycheck  -*- lexical-binding: t; -*-

;; Copyright (C) 2019-2023  Shen, Jen-Chieh
;; Created date 2019-11-06 18:08:01

;; Author: Shen, Jen-Chieh <jcs090218@gmail.com>
;; URL: https://github.com/emacs-grammarly/flycheck-grammarly
;; Package-Version: 20221231.1654
;; Package-Commit: d4a788acc3875a1ffdd7460ab3377a887413c582
;; Version: 0.2.3
;; Package-Requires: ((emacs "25.1") (flycheck "0.14") (grammarly "0.3.0") (s "1.12.0"))
;; Keywords: convenience grammar check

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Grammarly support for Flycheck.
;;

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'dom)

(require 'flycheck)
(require 'grammarly)
(require 's)

(defgroup flycheck-grammarly nil
  "Grammarly support for Flycheck."
  :prefix "flycheck-grammarly-"
  :group 'flycheck
  :link '(url-link :tag "Github" "https://github.com/emacs-grammarly/flycheck-grammarly"))

(defcustom flycheck-grammarly-active-modes
  '(text-mode latex-mode org-mode markdown-mode)
  "List of major mode that work with Grammarly."
  :type 'list
  :group 'flycheck-grammarly)

(defcustom flycheck-grammarly-check-time 0.8
  "How long do we call request after we done typing."
  :type 'float
  :group 'flycheck-grammarly)

;; For #3
(defconst flycheck-grammarly--avoidance-rule
  '((":" . "\n"))
  "Replace character to another character to avoid from Grammarly API.")

(defvar flycheck-grammarly--show-debug-message nil
  "Show the debug message from this package.")

(defvar-local flycheck-grammarly--done-checking nil
  "Check if Grammarly API done checking.")

(defvar-local flycheck-grammarly--point-data nil
  "List of error/warning JSON data.")

(defvar-local flycheck-grammarly--last-buffer-string nil
  "Record the last buffer string.")

(defvar flycheck-grammarly-check-strings nil
  "Alist of all check strings.")

(defvar-local flycheck-grammarly--request-timer nil
  "Timer that will tell to do the request.")

(defun flycheck-grammarly-post-cmd-hook ()
  (pcase-let ((`(,prev-beg ,prev-end) flycheck-grammarly-previous-check-string-bounds)
              (`(,current-beg ,current-end) (flycheck-grammarly-check-string-bounds)))
    (unless (and (= prev-beg current-beg) (= prev-end current-end))
      )))

(defun flycheck-grammarly-check-string-bounds ()
  "Return the string to check with grammarly.

Passing large buffers to grammarly can cause fewer grammar errors
to be reported and large number of flycheck errors also cause
issues.  Instead we pass only the current paragraph or
org subtree if in `org-mode'."
  (pcase major-mode
    ('org-mode
     (pcase-let ((`(,beg ,end ,has-body) (ref-man-org-text-bounds)))
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

(defun flycheck-grammarly-check-string ()
  "Return the string to check with grammarly.

Passing large buffers to grammarly can cause fewer grammar errors
to be reported and large number of flycheck errors also cause
issues.  Instead we pass only the current paragraph or
org subtree if in `org-mode'.

See `flycheck-grammarly-check-string-bounds'."
  (pcase-let ((`(,beg ,end) (flycheck-grammarly-check-string-bounds)))
    (when (and beg end)
      (buffer-substring-no-properties beg end))))

(defun flycheck-grammarly-get-offset ()
  (pcase major-mode
    ('org-mode
     (pcase-let ((`(,beg ,end ,has-body) (ref-man-org-text-bounds)))
       beg))
    (_ (save-excursion (if (and separating (not newlines))
                           (progn (re-search-forward "\n" nil t)
                                  (- (point-at-bol) 1))
                         (re-search-backward "\n\n" nil t)
                         (+ (point) 1))))))

(defun flycheck-grammarly--column-at-pos (&optional pt)
  "Column at PT."
  (unless pt (setq pt (point)))
  (save-excursion (goto-char pt) (current-column)))

(defun flycheck-grammarly--debug-message (fmt &rest args)
  "Debug message like function `message' with same argument FMT and ARGS."
  (when flycheck-grammarly--show-debug-message
    (apply 'message fmt args)
    (cond ((stringp args)
           (push (json-read-from-string args) my/grammarly-data))
          ((and (listp args) (stringp (cadr args)))
           (push (json-read-from-string (cadr args)) my/grammarly-data)))))

(defun flycheck-grammarly--on-open ()
  "On open Grammarly API."
  (when flycheck-mode
    (flycheck-grammarly--debug-message "[INFO] Start connecting to Grammarly API...")))

(defun flycheck-grammarly--on-message (data)
  "Received DATA from Grammarly API."
  (when flycheck-mode
    (flycheck-grammarly--debug-message
     "[INFO] Receiving data from grammarly, level (%s) : %s"
     (length flycheck-grammarly--point-data) data)
    (push (json-read-from-string data) flycheck-grammarly--point-data)
    ;; (when (string-match-p "\"highlightBegin\":" data)
    ;;   (push data flycheck-grammarly--point-data))
    ))

(defun flycheck-grammarly--on-close ()
  "On close Grammarly API."
  (when flycheck-mode
    (setq flycheck-grammarly--done-checking t)
    (flycheck-buffer-automatically)))

(defun flycheck-grammarly-checksum (str)
  "Minify the STR to check if any text changed."
  (declare (side-effect-free t))
  (md5 (replace-regexp-in-string "[[:space:]\n]+" " " str)))

(defun flycheck-grammarly--kill-timer ()
  "Kill the timer."
  (when (timerp flycheck-grammarly--request-timer)
    (cancel-timer flycheck-grammarly--request-timer)
    (setq flycheck-grammarly--request-timer nil)))

(defun flycheck-grammarly--reset-request ()
  "Reset some variables so the next time the user done typing can reuse."
  (flycheck-grammarly--debug-message "[INFO] Reset grammarly requests!")
  (setq flycheck-grammarly--last-buffer-string (flycheck-grammarly-check-string)
        flycheck-grammarly--point-data nil
        flycheck-grammarly--done-checking nil))

(defun flycheck-grammarly--after-change-functions (&rest _)
  "After change function to check if content change."
  (when (and flycheck-grammarly--last-buffer-string
             (not (string=
                   (flycheck-grammarly-checksum flycheck-grammarly--last-buffer-string)
                   (flycheck-grammarly-checksum (flycheck-grammarly-check-string)))))
    (flycheck-grammarly--kill-timer)
    (setq flycheck-grammarly--request-timer
          (run-with-idle-timer flycheck-grammarly-check-time nil
                               'flycheck-grammarly--reset-request))))

(defun flycheck-grammarly--encode-char (char-code)
  "Turn CHAR-CODE to character string."
  (cl-case char-code
    (4194208 (cons " " 2))
    (4194201 (cons "'" 3))))

(defun flycheck-grammarly--html-to-text (html)
  "Turn HTML to text."
  (let* ((buf (get-buffer-create " *flycheck-grammarly-temp-buf*"))
         (str (with-current-buffer buf
                (erase-buffer)
                (goto-char (point-min))
                (insert (string-replace "\302\240" " " html))
                (let ((pt (point)))
                  (shr-insert-document (libxml-parse-html-region (point-min) (point-max)))
                  (goto-char pt)
                  (buffer-substring pt (point-max))))))
    (replace-regexp-in-string "\n" "" str))
  ;; (with-temp-buffer
  ;;   (insert (string-replace "\\302\\240" " " html))
  ;;   (goto-char (point-min))
  ;;   (while (not (= (point) (point-max)))
  ;;     (let ((replace-data (flycheck-grammarly--encode-char (char-before))))
  ;;       (when replace-data
  ;;         (backward-delete-char (cdr replace-data))
  ;;         (insert (car replace-data))))
  ;;     (forward-char 1))
  ;;   (dom-texts (libxml-parse-html-region (point-min) (point-max))))
  )

(defun flycheck-grammarly--grab-info (data attr)
  "Grab value through ATTR key with DATA."
  (let* ((attrs (split-string attr " "))
         (json-object-type 'hash-table)
         (json-array-type 'list)
         (json-key-type 'string)
         (target-val (json-read-from-string data)))
    (while (< 0 (length attrs))
      (setq target-val (gethash (nth 0 attrs) target-val))
      (pop attrs))
    target-val))

(defun flycheck-grammarly--valid-description (desc)
  "Convert DESC to valid description."
  (setq desc (replace-regexp-in-string "\n" "" desc)
        desc (replace-regexp-in-string "[ ]+" " " desc))
  desc)

(setq my/flycheck-grammarly-transforms nil)
(setq my/flycheck-grammarly-data nil)

(defun flycheck-grammarly--check-all ()
  "Check grammar for buffer document."
  (message "Annotating grammarly region...")
  (let ((offset (flycheck-grammarly-get-offset))
        (flycheck-grammarly--point-data
         (-filter (lambda (x) (a-get x 'highlightBegin)) flycheck-grammarly--point-data))
        check-list)
    (if flycheck-grammarly--point-data
        (dolist (data flycheck-grammarly--point-data)
          (let* ( ; narrowed buffer
                 (pt-beg (+ offset (a-get data 'highlightBegin)))
                 (pt-end (+ offset (a-get data 'highlightBegin)))
                 (ln (line-number-at-pos pt-beg t))
                 (col-start (flycheck-grammarly--column-at-pos pt-beg))
                 (col-end (flycheck-grammarly--column-at-pos pt-end))
                 (exp (a-get data 'explanation))
                 (card-desc (unless exp (a-get* data 'cardLayout 'groupDescription)))
                 (desc (flycheck-grammarly--html-to-text (or exp card-desc "")))
                 (type (if exp (if (string-match-p "error" (json-encode data)) 'error 'warning) 'info))

                 (parsed-data data)
                 (transforms (a-get parsed-data 'transforms))
                 (beg (a-get parsed-data 'highlightBegin))
                 (end (a-get parsed-data 'highlightEnd))
                 (text (when (and beg end)
                         (substring-no-properties grammarly--text beg end)))
                 (context (list (a-get* parsed-data 'cardContext 's)
                                (a-get* parsed-data 'cardContext 'e)))
                 (context (when (-all? 'identity context)
                            (apply 'substring-no-properties
                                   grammarly--text context)))
                 (highlighted (a-get parsed-data 'highlightText))
                 (title (a-get parsed-data 'title))
                 (splits (when transforms
                           (seq-map (lambda (x) (split-string
                                                 (flycheck-grammarly--html-to-text x)))
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
            (when transforms (push transforms my/flycheck-grammarly-transforms))
            (when transforms (push parsed-data my/flycheck-grammarly-data))
            (push (list ln col-start type desc :end-column col-end) check-list)))
      (message "No errors in current text"))
    check-list))

(defun flycheck-grammarly--apply-avoidance-rule (str)
  "Apply avoidance rule to STR."
  (dolist (rule flycheck-grammarly--avoidance-rule)
    (setq str (s-replace (car rule) (cdr rule) str)))
  str)

(defun flycheck-grammarly--grammar-check ()
  "Grammar check once."
  (message "Sending grammarly check request...")
  (flycheck-grammarly--debug-message "[Initializing Grammar Check]")
  (unless (and flycheck-grammarly--done-checking
               (string= (flycheck-grammarly-checksum flycheck-grammarly--last-buffer-string)
                        (flycheck-grammarly-checksum (flycheck-grammarly-check-string))))
    (flycheck-grammarly--reset-request)
    (grammarly-check-text (flycheck-grammarly--apply-avoidance-rule
                           (flycheck-grammarly-check-string)))))

(defun flycheck-grammarly--start (checker callback)
  "Flycheck start function for CHECKER, invoking CALLBACK."
  (add-hook 'after-change-functions #'flycheck-grammarly--after-change-functions nil t)
  (flycheck-grammarly--grammar-check)
  (funcall
   callback 'finished
   (flycheck-increment-error-columns
    (mapcar
     (lambda (x)
       (apply #'flycheck-error-new-at `(,@x :checker ,checker)))
     (condition-case err
         (if flycheck-grammarly--done-checking
             (flycheck-grammarly--check-all)
           (flycheck-stop))
       (error (funcall callback 'errored (error-message-string err))
              (signal (car err) (cdr err))))))))

(flycheck-define-generic-checker 'grammarly
  "Grammarly flycheck definition."
  :start #'flycheck-grammarly--start
  :modes flycheck-grammarly-active-modes)

;;;###autoload
(defun flycheck-grammarly-setup ()
  "Setup flycheck-package."
  (interactive)
  (add-to-list 'flycheck-checkers 'grammarly)
  (add-to-list 'grammarly-on-open-function-list 'flycheck-grammarly--on-open)
  (add-to-list 'grammarly-on-message-function-list 'flycheck-grammarly--on-message)
  (add-to-list 'grammarly-on-close-function-list 'flycheck-grammarly--on-close))

(provide 'flycheck-grammarly)
;;; flycheck-grammarly.el ends here
