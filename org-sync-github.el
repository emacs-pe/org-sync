;;; org-sync-github.el --- Synchronize GitHub issues  -*- lexical-binding: t -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL:
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (ghub "1.2"))

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

;; Synchronize GitHub issues.

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'ghub)
(require 'eieio)
(require 'org-sync)
(require 'magit-git)

;;; Customize
(defgroup org-sync-github nil
  "Syncronice org with GitHub issues."
  :prefix "org-sync-"
  :group 'org-sync)

(defcustom org-sync-github-issues-params
  '((state    . "all")
    (per_page . "100") ; XXX: max allowed https://developer.github.com/v3/#pagination
    )
  "Default params to call GitHub API to fetch all issues."
  :type 'list
  :safe #'listp
  :group 'org-sync-github)

(defcustom org-sync-github-unpaginate nil
  "Whether to use an un-paginated call when fetching issues."
  :type 'boolean
  :safe 'booleanp
  :group 'org-sync-github)

(defclass org-sync-issue-github (org-sync-issue)
  ((locked     :initarg :locked     :initform nil)
   (assignees  :initarg :assignees  :initform nil)))

(defun org-sync-github-read-endpoint ()
  "Read an endpoint to be used to fetch the issues."
  (cl-multiple-value-bind (context _name)
      (org-sync-read-multiple-choice "GitHub Issues by?"
                                     '((?u "user issues")
                                       (?o "organization issues")
                                       (?e "enter project")
                                       (?r "git remote")))
    (cl-ecase context
      (?u "/issues")
      (?o (format "/orgs/%s/issues" (magit-read-string-ns "Organization")))
      (?e (format "/repos/%s/issues" (magit-read-string-ns "Project (owner/repo)")))
      (?r (cl-multiple-value-bind (_host slug)
              (org-sync-parse-remote (magit-read-string-ns "Remote Url" (magit-get "remote" (magit-read-remote "Remote" nil t) "url")))
            (format "/repos/%s/issues" slug))))))

(defun org-sync-github-issue (data)
  "Given a DATA response from GitHub issue api return an `org-sync-issue'."
  (let-alist data
    (make-instance 'org-sync-issue-github
                   ;; NOTE: Don't use .id, because is global
                   :id          .number
                   :url         .html_url
                   :tags        (mapcar (lambda (label)
                                          (assoc-default "name" label))
                                        .labels)
                   :title       .title
                   :status      (pcase .state
                                  ("open"   'todo)
                                  ("closed" 'done)
                                  ;; Default to todo for unknown .status
                                  (_        'todo))
                   :comments    .comments
                   :locked      (org-sync-json-truthy .locked)
                   :milestone   .milestone.number
                   :assignees   (mapcar (lambda (assignee)
                                          (assoc-default "login" assignee))
                                        .assignees)
                   :closed-at   .closed_at
                   :closed-by   .closed_by.login
                   :created-at  .created_at
                   :created-by  .user.login
                   :updated-at  .updated_at
                   :description .body)))

(cl-defmethod org-sync/issues ((_backend (eql github)))
  "Get issues from GitHub backend."
  (let ((endpoint (or (org-sync-document-keyword "ORG_SYNC_ISSUES")
                      (org-sync-returning-it (org-sync-github-read-endpoint)
                        (org-sync-document-keyword-insert "ORG_SYNC_ISSUES" it))))
        (ghub-unpaginate org-sync-github-unpaginate))
    (mapcar #'org-sync-github-issue (ghub-get endpoint org-sync-github-issues-params))))

;;;###autoload
(with-eval-after-load 'org-sync
  (add-to-list 'org-sync-backends 'github))

(provide 'org-sync-github)
;;; org-sync-github.el ends here
