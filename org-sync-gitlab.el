;;; org-sync-gitlab.el --- Synchronize Gitlab issues  -*- lexical-binding: t -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL:
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (glab "1.1"))

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

;; TODO:
;;
;; + Gitlab offers "time tracking" support, check is could be posible to
;;   integrate with "org-clock": https://gitlab.com/help/workflow/time_tracking.md

;;; Code:
(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

(require 'glab)
(require 'eieio)
(require 'org-sync)
(require 'magit-git)

(defgroup org-sync-gitlab nil
  "Syncronice org with gitlab issues."
  :prefix "org-sync-"
  :group 'org-sync)

(defcustom org-sync-gitlab-issues-params
  '((per_page . "100") ; XXX: max allowed https://docs.gitlab.com/ce/api/#pagination
    )
  "Default params to call the Gitlab API when fetching the issues."
  :type 'list
  :safe 'listp
  :group 'org-sync-gitlab)

(defcustom org-sync-gitlab-unpaginate nil
  "Whether to use an un-paginated call when fetching issues."
  :type 'boolean
  :safe 'booleanp
  :group 'org-sync-gitlab)

(defclass org-sync-issue-gitlab (org-sync-issue)
  ((assignee      :initarg :assignee      :initform nil)
   (subscribed    :initarg :subscribed    :initform nil)
   (confidential  :initarg :confidential  :initform nil)))

;; NB: URL encode "user/repo" to avoid to know first hand the project_id.
;;     https://docs.gitlab.com/ee/api/projects.html#get-single-project
(defun org-sync-gitlab-read-endpoint ()
  "Read an endpoint to be used to fetch the issues."
  (cl-multiple-value-bind (context _name)
      (org-sync-read-multiple-choice "GitLab issues by?"
                                     '((?u "user issues")
                                       (?g "group issues")
                                       (?e "enter project")
                                       (?r "git remote")))
    (cl-ecase context
      (?u "/issues")
      (?g (format "/groups/%s/issues" (magit-read-string-ns "Group ID")))
      (?e (format "/projects/%s/issues" (url-hexify-string (magit-read-string-ns "Project (owner/repo)"))))
      (?r (cl-multiple-value-bind (_host slug)
              (org-sync-parse-remote (magit-read-string-ns "Remote Url" (magit-get "remote" (magit-read-remote "Remote" nil t) "url")))
            (format "/projects/%s/issues" (url-hexify-string slug)))))))

(defun org-sync-gitlab-issue (data)
  "Given a DATA response from Gitlab issue api return an `org-sync-issue'."
  (let-alist data
    (make-instance 'org-sync-issue-gitlab
                   ;; NOTE: iid is more meaningful in this scenario
                   :id           .iid
                   :url          .web_url
                   :tags         .labels
                   :title        .title
                   :status       (pcase .state
                                   ("opened" 'todo)
                                   ("closed" 'done)
                                   ;; Default to todo for unknown .state
                                   (_        'todo))
                   :comments     .user_notes_count
                   :deadline     .due_date
                   :milestone    .milestone.id
                   :assignee     .assignee.username
                   ;; XXX: Gitlab issues api doesn't send have a "closed_at" field https://gitlab.com/gitlab-org/gitlab-ce/issues/5935
                   :closed-at    (and (string= .state "closed") .updated_at)
                   :created-at   .created_at
                   :created-by   .author.username
                   :updated-at   .updated_at
                   :description  .description
                   :subscribed   (org-sync-json-truthy .subscribed)
                   :confidential (org-sync-json-truthy .confidential))))

(cl-defmethod org-sync/issue-new ((_backend (eql gitlab)) data)
  (let ((endpoint (org-sync-document-keyword "ORG_SYNC_ISSUES")))
    (org-sync-gitlab-issue (glab-post endpoint nil `((title       . ,(plist-get data :title))
                                                     (labels      . ,(string-join (plist-get data :tags) ","))
                                                     (description . ,(plist-get data :description)))))))

(cl-defmethod org-sync/issues ((_backend (eql gitlab)))
  (let ((endpoint (or (org-sync-document-keyword "ORG_SYNC_ISSUES")
                      (org-sync-returning-it (org-sync-gitlab-read-endpoint)
                        (org-sync-document-keyword-insert "ORG_SYNC_ISSUES" it))))
        (glab-unpaginate org-sync-gitlab-unpaginate))
    (mapcar #'org-sync-gitlab-issue (glab-get endpoint org-sync-gitlab-issues-params))))

;;;###autoload
(with-eval-after-load 'org-sync
  (add-to-list 'org-sync-backends 'gitlab))

(provide 'org-sync-gitlab)
;;; org-sync-gitlab.el ends here
