;;; org-sync-gitlab.el --- Synchronize Gitlab issues  -*- lexical-binding: t -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL: https://github.com/emacs-pe/org-sync
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
(defun org-sync-gitlab-endpoint-project ()
  "Read a GitLab project id to be used by this backend."
  (cl-multiple-value-bind (context _name)
      (org-sync-read-multiple-choice "GitLab project by?"
                                     '((?r "git remote")
                                       (?s "project slug")))
    (cl-ecase context
      (?s (url-hexify-string (magit-read-string-ns "Project (owner/repo)")))
      (?r (cl-multiple-value-bind (_host slug)
              (org-sync-parse-remote (magit-read-string-ns "Remote Url" (magit-get "remote" (magit-read-remote "Remote" nil t) "url")))
            (url-hexify-string slug))))))

(defun org-sync-gitlab-collect-tags (raw-tags)
  "Collect tags from RAW-TAGS string."
  (string-join (mapcar #'org-sync-as-string (org-sync-unserialize raw-tags)) ","))

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
                   :closed_at    (and (string= .state "closed") .updated_at)
                   :created_at   .created_at
                   :created_by   .author.username
                   :updated_at   .updated_at
                   :description  .description
                   :subscribed   (org-sync-json-truthy .subscribed)
                   :confidential (org-sync-json-truthy .confidential))))

(cl-defmethod org-sync/issue-new ((_backend (eql gitlab)) issue)
  (let* ((project-id (org-sync-keyword-get-or-set "ORG_SYNC_GITLAB_PROJECT" #'org-sync-gitlab-endpoint-project))
         (endpoint (format "/projects/%s/issues" project-id)))
    (org-sync-gitlab-issue (glab-post endpoint nil `((title       . ,(oref issue title))
                                                     (labels      . ,(string-join (oref issue tags) ","))
                                                     (description . ,(oref issue description)))))))

(cl-defmethod org-sync/issue ((_backend (eql gitlab)) id)
  (let ((project-id (org-sync-keyword-get-or-set "ORG_SYNC_GITLAB_PROJECT" #'org-sync-gitlab-endpoint-project)))
    (org-sync-gitlab-issue (glab-get (format "/projects/%s/issues/%s" project-id id)))))

(cl-defmethod org-sync/issue-sync ((backend (eql gitlab)) local-issue)
  (let* ((project-id (org-sync-keyword-get-or-set "ORG_SYNC_GITLAB_PROJECT" #'org-sync-gitlab-endpoint-project))
         (remote-issue (org-sync/issue backend (oref local-issue id)))
         (is-outdated-p (time-less-p (org-sync-parse-date (oref local-issue updated_at))
                                     (org-sync-parse-date (oref remote-issue updated_at)))))
    (if is-outdated-p
        remote-issue
      (let ((data (list
                   :title        (oref local-issue title)
                   :description  (oref local-issue description)
                   :confidential (oref local-issue confidential)
                   :milestone_id (oref local-issue milestone)
                   :due_date     (oref local-issue deadline)
                   :updated_at   (oref local-issue updated_at) ;; XXX: necessary?
                   :labels       (string-join (oref local-issue tags) ","))))
        (unless (eq (oref local-issue status) (oref remote-issue status))
          (plist-put data :state_event (cl-ecase (oref local-issue status)
                                         (done "close")
                                         (todo "reopen"))))
        (org-sync-gitlab-issue (glab-put (format "/projects/%s/issues/%s" project-id (oref local-issue id)) nil data))))))

(cl-defmethod org-sync/issues ((_backend (eql gitlab)))
  (let ((project-id (org-sync-keyword-get-or-set "ORG_SYNC_GITLAB_PROJECT" #'org-sync-gitlab-endpoint-project))
        (glab-unpaginate org-sync-gitlab-unpaginate))
    (mapcar #'org-sync-gitlab-issue (glab-get (format "/projects/%s/issues" project-id) org-sync-gitlab-issues-params))))

;;;###autoload
(with-eval-after-load 'org-sync
  (add-to-list 'org-sync-backends 'gitlab))

(provide 'org-sync-gitlab)
;;; org-sync-gitlab.el ends here
