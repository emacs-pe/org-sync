;;; org-sync.el --- Synchronize issues with org       -*- lexical-binding: t -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/org-sync
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (magit "2.8.0"))

;; This file is NOT part of GNU Emacs.

;;; License:

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

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'ob)
(require 'org)
(require 'json)
(require 'eieio)
(require 'magit-git)
(require 'url-parse)
(require 'parse-time)
(require 'format-spec)
(require 'org-element)


(defgroup org-sync nil
  "Syncronice org with issues"
  :prefix "org-sync-"
  :group 'outlines)

(defcustom org-sync-backends nil
  "List of available backends for `org-sync'."
  :type  'list
  :group 'org-sync)

(defcustom org-sync-metadata-title-format "Orgsync %b"
  "Format metadata heading.

%b: backend name"
  :type 'string
  :safe #'stringp
  :group 'org-sync)

(defcustom org-sync-issue-title-format "Bug #%i: %t"
  "Format string for tagged slots.
The following format codes are supported:

%i:  Issue id
%t:  Issue title"
  :type 'string
  :safe #'stringp
  :group 'org-sync)

(defcustom org-sync-org-todo-keyword "TODO"
  "Keyword used on todo issues."
  :type 'string
  :safe #'stringp
  :group 'org-sync)

(defcustom org-sync-org-done-keyword "DONE"
  "Keyword used on done issues."
  :type 'string
  :safe #'stringp
  :group 'org-sync)

(defcustom org-sync-headline-level 1
  "Default org headline level.."
  :type 'integer
  :safe #'integerp
  :group 'org-sync)

(defcustom org-sync-metadata-prefix "OS_"
  "Prefix used in org properties to hold the metadata."
  :type 'string
  :safe #'stringp
  :group 'org-sync)

(defcustom org-sync-process-commented-entries nil
  "Default filename used by `org-sync' to fetch issues."
  :type 'boolean
  :safe #'booleanp
  :group 'org-sync)

(defcustom org-sync-file-name "orgsync.org"
  "Default filename used by `org-sync' to fetch issues."
  :type 'string
  :safe #'stringp
  :group 'org-sync)

(defcustom org-sync-export-backend 'md
  "Backend to export body when creating an issue.

See `org-export-registered-backends' for available backends."
  :type 'symbol
  :safe #'symbolp
  :group 'org-sync)

(defvar org-sync-persistent-dir "org-sync"
  "Directory used by `org-persistent' to save `org-sync-issue'.

This directory is relative to git dir.")

(defvar org-sync-issues-hook '(delete-trailing-whitespace)
  "Hook executed after fetching issues.")

;; TODO(marsam): Replace with a minor-mode(?)
(defvar-local org-sync nil
  "Local variable to check if a buffer is synced with")
(put 'org-sync 'permanent-local t)
;;;###autoload
(put 'org-sync 'safe-local-variable 'booleanp)

(defvar-local org-sync-backend nil
  "Default `org-sync' backend used in current buffer.")
(put 'org-sync-backend 'permanent-local t)
;;;###autoload
(put 'org-sync-backend 'safe-local-variable 'org-sync-registered-backend-p)

(defconst org-sync-version "0.1"
  "Org-sync version.")

;;; Helpers
(defsubst org-sync-as-symbol (string-or-symbol)
  "If STRING-OR-SYMBOL is already a symbol, return it.  Otherwise convert it to a symbol and return that."
  (if (symbolp string-or-symbol) string-or-symbol (intern string-or-symbol)))

(defsubst org-sync-as-string (string-or-symbol)
  "If STRING-OR-SYMBOL is already a string, return it.  Otherwise convert it to a string and return that."
  (cl-etypecase string-or-symbol
    (stringp string-or-symbol)
    (numberp (number-to-string string-or-symbol))
    (symbolp (symbol-name string-or-symbol))))

(defsubst org-sync-as-keyword (string-or-symbol)
  "Convert STRING-OR-SYMBOL into a keyword symbol."
  (let ((string (org-sync-as-string string-or-symbol)))
    (intern (if (eq (aref string 0) ?:) string (concat ":" string)))))

(defsubst org-sync-serialize (data)
  "Serialize DATA.  The returned data can be restored with `org-sync-unserialize'."
  (let (print-length) (prin1-to-string data)))

(defsubst org-sync-unserialize (data)
  "Read DATA serialized by `org-sync-serialize'."
  (read data))

(defsubst org-sync-remove-square-brackets (string)
  "Remove angle brackets from STRING."
  (string-remove-suffix "]" (string-remove-prefix "[" string)))

(defsubst org-sync-remove-angle-brackets (string)
  "Remove angle brackets from STRING."
  (string-remove-suffix ">" (string-remove-prefix "<" string)))

(defsubst org-sync-registered-backend-p (backend)
  "Determine whether `org-sync' BACKEND is registered."
  (memq backend org-sync-backends))

(defun org-sync-backend ()
  "Select an appropriate backend."
  (or org-sync-backend
      (and (derived-mode-p 'org-mode)
           (or (org-entry-get nil "OS_BACKEND" 'inherit)
               (org-sync-document-keyword "OS_BACKEND")))))

(defun org-sync-clean-tag (tag)
  "Clean TAG with valid characters."
  (replace-regexp-in-string "[[:space:][:punct:]]" "_" tag))

(defun org-sync-json-truthy (keyword)
  "Convert a decoded `json' KEYWORD to elisp truthy."
  (cond ((eq keyword t)          t)
        ((eq keyword json-null)  nil)   ; XXX: let's be lenient
        ((eq keyword json-false) nil)))

(defun org-sync-plist-to-node-properties (alist)
  "Return an org property drawer node properties from an ALIST."
  (cl-loop for (key . value) in alist
           collect (list 'node-property (list :key key :value value))))

(defun org-sync-find-entry-with-id (ident)
  "Locate the entry that contain the ID property with exact value IDENT.

IDENT can be a string, a symbol or a number, this function will search for
the string representation of it.
Return the position where this entry starts, or nil if there is no such entry."
  (interactive "sID: ")
  (org-with-wide-buffer (org-find-property (concat org-sync-metadata-prefix "ID") (org-sync-as-string ident))))

(defun org-sync-read-multiple-choice (prompt choices)
  "Ask user a multiple choice question with PROMPT and CHOICES."
  (if (fboundp 'read-multiple-choice)
      (read-multiple-choice prompt choices) ;; Available since Emacs26.1
    (when-let (choice-chars (mapcar #'car choices))
      (assq (read-char-choice
             (format "%s(%s): "
                     (substring prompt 0 (string-match ": $" prompt))
                     (mapconcat (lambda (ch)
                                  (format "[%c] - %s" (car ch) (cadr ch)))
                                choices "; "))
             choice-chars)
            choices))))

(defun org-sync-json-read (&optional coding-system)
  "Bind `json-array-type' to `list' and call `json-read' with CODING-SYSTEM."
  (let ((json-array-type 'list))
    (json-read-from-string (decode-coding-string (buffer-substring-no-properties (point) (point-max)) (or coding-system 'utf-8)))))

(defun org-sync-json-encode (object &optional coding-system)
  "Return a JSON representation of OBJECT as a string with CODING-SYSTEM."
  (encode-coding-string (json-encode object) (or coding-system 'utf-8)))

(defun org-sync-parse-date (date-string)
  "Parse DATE-STRING and return a time value."
  (condition-case nil
      (parse-iso8601-time-string date-string)
    (error (apply 'encode-time (org-parse-time-string date-string))))) ;; XXX: Don't use `date-to-time' because is buggy

(defsubst org-sync-parse-date-maybe (date-string)
  "Return a internal time for DATE-STRING if is not nil."
  (and date-string (org-sync-parse-date date-string)))

(defun org-sync-random-color ()
  "Return a random hex triplet color."
  (format "%02x%02x%02x" (random 256) (random 256) (random 256)))

(defun org-sync-assoc-default (key alist)
  "Get value associate with KEY from ALIST."
  (cdr-safe (assoc-string key alist)))

(defun org-sync-assoc-list (key list-of-alists)
  "Return a list with the values of KEY associated in each alist of LIST-OF-ALISTS."
  (mapcar (lambda (alist) (org-sync-assoc-default key alist)) list-of-alists))

(defun org-sync-git-parse-url (url)
  "Parse an git remote URL, and return an url struct."
  (url-generic-parse-url (if (and (not (string-match-p "^[a-zA-Z_-]+://" url))
                                  (string-match-p ":" url)
                                  (not (string-match-p "\\\\\\\\" url))) ;; Two literal backlashes
                             (concat "ssh://" (subst-char-in-string ?: ?/ url))
                           url)))

(defun org-sync-parse-remote (url)
  "Return a list \(hostname slug\) from an git URL.

Where slug is stripped from its \".git\" suffix and \"/\" prefix."
  (let ((urlobj (org-sync-git-parse-url url)))
    (list (url-host urlobj) (string-remove-prefix "/" (string-remove-suffix ".git" (url-filename urlobj))))))

(defun org-sync-parse-remote-or-error (url)
  "Return an parsed remote URL or error."
  (or (and url (org-sync-parse-remote url)) (error "Invalid url: %S" url)))

(defsubst org-sync-timestamp-active-p (timestamp)
  "Check if an org TIMESTAMP string is active."
  (eq (aref timestamp 0) ?<))

(defun org-sync-format-time (time &optional with-hm inactive)
  "Return a date stamp for the date given by the internal TIME.

See `format-time-string' for the format of TIME.  WITH-HM means
use the stamp format that includes the time of the day.  INACTIVE
means use square brackets instead of angular ones, so that the
stamp will not contribute to the agenda."
  (let ((fmt (funcall (if with-hm 'cdr 'car) org-time-stamp-formats)))
    (and inactive (setq fmt (concat "[" (substring fmt 1 -1) "]")))
    (format-time-string fmt time)))

(defsubst org-sync-format-time-maybe (time &optional with-hm inactive)
  "Check if TIME is not nil and return a date stamp for internal TIME.

WITH-HM and INACTIVE are passed to `org-sync-format-time'."
  (and time (org-sync-format-time time with-hm inactive)))

(defsubst org-sync-date-to-timestamp (date-string &optional with-hm inactive)
  "Parse a DATE-STRING and format it as org timestamp.

WITH-HM and INACTIVE are passed to `org-sync-format-time'."
  (org-sync-format-time (org-sync-parse-date date-string) with-hm inactive))

(defsubst org-sync-property-prefix (slot)
  "Generate from SLOT."
  (concat org-sync-metadata-prefix (upcase (org-sync-as-string slot))))

(defun org-sync-object-slots (object)
  "Return list of slot names available in OBJECT."
  (mapcar #'eieio-slot-descriptor-name (eieio-class-slots (eieio--object-class object))))

(defun org-sync-object-property-alist (object)
  "Generate an alist from OBJECT slots."
  (cl-loop for slot in (org-sync-object-slots object)
           unless (eq slot 'description) ; XXX: description is stored separately
           collect (cons (org-sync-property-prefix slot) (org-sync-serialize (eieio-oref object slot)))))

(defun org-sync-document-keywords ()
  "Return an alist of org document keywords."
  (org-with-wide-buffer
   (goto-char (point-min))
   (cl-loop while (re-search-forward "^[ \t]*#\\+\\([^\t\n]+\\):[ \t]*\\([^\t\n]+\\)" nil 'noerror)
            collect (cons (upcase (match-string-no-properties 1)) (match-string-no-properties 2)))))

(defun org-sync-document-keyword (keyword)
  "Return the value of a org document KEYWORD."
  (let ((case-fold-search t)
        (regexp (format "^[ \t]*#\\+%s:[ \t]*\\([^\t\n]+\\)" keyword)))
    (org-with-wide-buffer
     (goto-char (point-min))
     (and (re-search-forward regexp nil 'noerror) (match-string-no-properties 1)))))

;; XXX: Maybe look for a better way to parse a subtree
(defun org-sync-parse-subtree (&optional granularity visible-only)
  "Parse current org sub tree narrowing it first.

GRANULARITY and VISIBLE-ONLY arguments are passed to `org-element-parse-buffer'."
  (cl-assert (derived-mode-p 'org-mode) nil "Cannot parse subtree in not an Org mode: %s" major-mode)
  (save-restriction
    (org-narrow-to-subtree)
    (cl-caddr (org-element-parse-buffer granularity visible-only))))

(defun org-sync-collect-metadata (element)
  "Collect org-sync metadata from an org ELEMENT into a plist.

A metadata is represented in the ELEMENT property drawer prefixed
by `org-sync-metadata-prefix'."
  (cl-loop for prop in (cddr (assq 'property-drawer (assq 'section element)))
           for plist = (cadr prop)
           when (string-prefix-p org-sync-metadata-prefix (plist-get plist :key))
           nconc (list (org-sync-as-keyword (downcase (string-remove-prefix org-sync-metadata-prefix (plist-get plist :key))))
                       (org-sync-unserialize (plist-get plist :value)))))

(defun org-sync-data-new-issue (element)
  "Return metadata plist from an ELEMENT to create a new issue."
  (list :title       (org-no-properties (car (org-element-property :title element)))
        :tags        (mapcar #'org-no-properties (org-element-property :tags element))
        :status      (org-element-property :todo-type element)
        :deadline    (org-sync-remove-angle-brackets (org-element-property :raw-value (org-element-property :scheduled element)))
        :closed-at   (org-sync-remove-square-brackets (org-element-property :raw-value (org-element-property :closed element)))))

(defun org-sync-issue-title-regexp ()
  "Return a regex for title from `org-sync-issue-title-format'."
  (with-temp-buffer
    (insert org-sync-issue-title-format)
    (goto-char (point-min))
    (while (search-forward "%" nil 'noerror)
      (cond
       ((eq (char-after) ?%)
        (delete-char 1))
       ((looking-at "\\([-0-9.]*\\)\\([a-zA-Z]\\)")
        (let* ((num (match-string 1))
               (spec (string-to-char (match-string 2)))
               (val (cl-ecase spec
                      (?i  (rx (+ num)))
                      (?t  (rx (group (+ not-newline)))))))
          (let ((text (format (concat "%" num "s") val)))
            (insert-and-inherit text)
            (delete-region (+ (match-beginning 0) (length text))
                           (+ (match-end 0) (length text)))
            (delete-region (1- (match-beginning 0)) (match-beginning 0)))))))
    (buffer-string)))

(defun org-sync-issue-title (string)
  "Return the title value from a STRING based on `org-sync-issue-title-format'."
  (and (string-match (org-sync-issue-title-regexp) string) (match-string 1 string)))

(defun org-sync-interpret-description (element)
  "Interpret data from description ELEMENT."
  (org-export-string-as (org-element-interpret-data element) org-sync-export-backend))

(defun org-sync-desc-element (element)
  "Return a valid description element for a headline ELEMENT."
  (cl-remove-if (lambda (e) (memq (org-element-type e) '(planning property-drawer))) (assq 'section element)))

(defun org-sync-metadata-valid-p (metadata)
  "Check if an issue METADATA is valid."
  (plist-member metadata :id))

(defclass org-sync-issue ()
  ((id           :initarg :id           :initform nil) ; String (required)
   (url          :initarg :url          :initform nil) ; String
   (tags         :initarg :tags         :initform nil) ; [String]
   (title        :initarg :title        :initform nil) ; String
   (status       :initarg :status       :initform nil) ; 'todo | 'done
   (comments     :initarg :comments     :initform nil) ; Integer
   (deadline     :initarg :deadline     :initform nil) ; DateString
   (milestone    :initarg :milestone    :initform nil) ; nil | String
   (created-at   :initarg :created-at   :initform nil) ; DateString
   (created-by   :initarg :created-by   :initform nil) ; nil | String
   (closed-at    :initarg :closed-at    :initform nil) ; nil | DateString
   (closed-by    :initarg :closed-by    :initform nil) ; nil | String
   (updated-at   :initarg :updated-at   :initform nil) ; DateString
   (description  :initarg :description  :initform nil) ; String
   )
  :abstract t)

(cl-defgeneric org-sync/metadata (backend)
  (:documentation "Metadata to identify a backend"))

(cl-defgeneric org-sync/valid-p (backend metadata)
  (:documentation "Check if backend is correctly configured with metadata"))

(cl-defgeneric org-sync/issues (backend)
  (:documentation "Get all issues from backend"))

(cl-defgeneric org-sync/issue-new (backend metadata issue-data)
  (:documentation "Create an issue in backend from an `org-sync-issue'"))

(cl-defgeneric org-sync/issue-get (backend &rest args)
  (:documentation "Get an `org-sync-issue' from backend and id"))

(cl-defgeneric org-sync/issue-update (backend &rest args)
  (:documentation "Update an `org-sync-issue' from backend and id"))

(cl-defgeneric org-sync/issue-delete (backend &rest args)
  (:documentation "Import an `org-sync-issue' from backend and id"))


(cl-defmethod org-sync/issue->org-title ((issue org-sync-issue))
  (format-spec org-sync-issue-title-format `((?i . ,(oref issue id))
                                             (?t . ,(oref issue title)))))

(cl-defmethod org-sync/issue->org-keyword ((issue org-sync-issue))
  (cl-case (oref issue status)
    (done      org-sync-org-done-keyword)
    (todo      org-sync-org-todo-keyword)
    (otherwise org-sync-org-todo-keyword)))

(cl-defmethod org-sync/issue->org-tag ((issue org-sync-issue))
  (mapcar #'org-sync-clean-tag (oref issue tags)))

(cl-defmethod org-sync/issue->org-planning ((issue org-sync-issue))
  (nconc (and (oref issue deadline) `(:deadline (timestamp (:raw-value ,(org-sync-date-to-timestamp (oref issue deadline) 'with-hm)))))
         (and (oref issue closed-at) `(:closed (timestamp (:raw-value ,(org-sync-date-to-timestamp (oref issue closed-at) 'with-hm 'inactive)))))))

(cl-defmethod org-sync/metadata->org-element (backend)
  "Return a org element from an `org-sync' ISSUE."
  (org-element-interpret-data `(headline
                                (:title ,(format-spec org-sync-metadata-title-format `((?b . ,backend)))
                                        :level ,org-sync-headline-level)
                                (section
                                 nil
                                 (property-drawer (:post-blank 1) ,(org-sync-plist-to-node-properties (org-sync/metadata backend)))))))

(cl-defmethod org-sync/issue->org-element ((issue org-sync-issue))
  "Return a org element from an `org-sync' ISSUE."
  `(headline
    (:title ,(org-sync/issue->org-title issue)
            :level        ,(1+ org-sync-headline-level)
            :tags         ,(org-sync/issue->org-tag issue)
            :todo-type    ,(oref issue status)
            :todo-keyword ,(org-sync/issue->org-keyword issue))
    (section
     nil
     (planning ,(org-sync/issue->org-planning issue))
     (property-drawer (:post-blank 1) ,(org-sync-plist-to-node-properties (org-sync-object-property-alist issue)))
     (fixed-width (:value ,(oref issue description))))))

(cl-defmethod org-sync/issue->org ((issue org-sync-issue))
  "Return a org element from an `org-sync' ISSUE."
  (org-element-interpret-data (org-sync/issue->org-element issue)))


;;; Public
;;;###autoload
(defun org-sync (&optional arg)
  "Sync the state from an item.

With \\[universal-argument] prefix ARG, use completion to
determine the new state."
  (interactive "P")
  (cl-assert (derived-mode-p 'org-mode) nil "Cannot execute `%s' in not an Org mode: %s" this-command major-mode)
  (if (and (org-region-active-p) org-loop-over-headlines-in-active-region)
      (let ((cl (if (eq org-loop-over-headlines-in-active-region 'start-level)
                    'region-start-level 'region))
            org-loop-over-headlines-in-active-region)
        (org-map-entries
         `(org-sync ,arg)
         org-loop-over-headlines-in-active-region
         cl (if (outline-invisible-p) (org-end-of-subtree nil t))))
    (if (equal arg '(16)) (setq arg 'nextset))
    (if (and (org-in-commented-heading-p t) (not org-sync-process-commented-entries))
        (message "Not syncing commented entry")
      (save-excursion
        (when-let (backend (org-sync-as-symbol (org-sync-backend)))
          (cl-assert (org-sync-registered-backend-p backend) nil "Unregistered org-sync backend: %S" backend)
          (let* ((class (org-sync-as-symbol (format "org-sync-issue-%s" backend)))
                 (element (org-sync-parse-subtree))
                 (metadata (org-sync-collect-metadata element))
                 (desc-element (org-sync-desc-element element))
                 (begin (org-element-property :begin element))
                 (end (or (org-element-property :end desc-element)
                          (org-element-property :end element))))
            (if (org-sync-metadata-valid-p metadata)
                (let ((issue (apply class metadata)))
                  (setf (oref issue title) (car (org-element-property :title element)))
                  (delete-region begin end)
                  (insert (org-sync/issue->org issue)))
              ;; XXX: Issue doesn't exists create it
              (let* ((metadata (org-sync/metadata backend))
                     (issue-data (plist-put (org-sync-data-new-issue element) :description (org-sync-interpret-description desc-element)))
                     (issue (org-sync/issue-new backend metadata issue-data)))
                (delete-region begin end)
                (insert (org-sync/issue->org issue))))))))))

;;;###autoload
(cl-defun org-sync-issues (&optional (backend (org-sync-backend)))
  "Show issues from BACKEND."
  (interactive (list (or (and (not current-prefix-arg) (org-sync-backend))
                         (completing-read "Backend: " org-sync-backends nil t nil nil (org-sync-backend)))))
  (setq backend (org-sync-as-symbol backend))
  (cl-assert (org-sync-registered-backend-p backend) nil "Unregistered org-sync backend: %S" backend)
  (with-current-buffer (find-file-noselect org-sync-file-name)
    (dolist (issue (org-sync/issues backend))
      (if-let (pom (org-sync-find-entry-with-id (oref issue id)))
          (with-demoted-errors "Error while syncing issue: %S"
            (org-with-point-at pom
              (message "Syncing issue %s..." (oref issue id))
              (call-interactively #'org-sync)))
        (org-with-point-at (point-max)
          (insert (org-sync/issue->org issue)))))
    (org-mode)
    (run-hooks 'org-sync-issues-hook)
    (pop-to-buffer (current-buffer) nil t)))

(provide 'org-sync)
;;; org-sync.el ends here
