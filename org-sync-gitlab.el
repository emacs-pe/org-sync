;;; org-sync-gitlab.el --- Synchronize Gitlab issues  -*- lexical-binding: t -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL:
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (request "0.2.0"))

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
  (require 'cl-lib))

(require 'eieio)
(require 'subr-x)
(require 'request)
(require 'org-sync)
(require 'magit-git)

;;; Customize
(defgroup org-sync-gitlab nil
  "Syncronice org with gitlab issues."
  :prefix "org-sync-"
  :group 'org-sync)

(defcustom org-sync-gitlab-api-baseurl (or (getenv "GITLAB_API_ENDPOINT")
                                           "https://gitlab.com/api/v3/")
  "Base url for Gitlab API."
  :type 'string
  :safe 'stringp
  :group 'org-sync-gitlab)

(defcustom org-sync-gitlab-issues-params
  '(("per_page" . "100") ; XXX: max allowed https://docs.gitlab.com/ce/api/#pagination
    ("order_by" . "updated_at")         ; better than the default "created_at"
    )
  "Default params to call the Gitlab API."
  :type 'list
  :safe 'listp
  :group 'org-sync-gitlab)

(defcustom org-sync-gitlab-api-auth-method nil
  "Default authentication method used to make request to Gitlab api."
  :type '(choice (string :tag "Oauth token"     oauth-token)
                 (const  :tag "Private token"   private-token)
                 (const  :tag "Unauthenticated" nil))
  :safe 'symbolp
  :group 'org-sync-gitlab)

(defcustom org-sync-gitlab-token nil
  "Default access token to call the Gitlab API.

See: `https://docs.gitlab.com/ce/api/oauth2.html'"
  :type 'string
  :safe 'stringp
  :group 'org-sync-gitlab)

(defcustom org-sync-gitlab-log nil
  "Whether to save the log the last request to gitlab API."
  :type 'boolean
  :safe 'booleanp
  :group 'org-sync-gitlab)

(defcustom org-sync-gitlab-private-token nil
  "User private token.

See: `https://gitlab.com/profile/personal_access_tokens'"
  :type 'string
  :safe 'stringp
  :group 'org-sync-gitlab)

(defvar org-sync-gitlab-default-headers
  `(("Content-Type" . "application/json")
    ("User-Agent"   . ,(format "org-sync.el/%s" org-sync-version))))

(defvar org-sync-gitlab-log-buffer "*org-sync-log[gitlab]*")


(defun org-sync-gitlab-token ()
  "Get user gitlab access token."
  (or org-sync-gitlab-token
      (getenv "GITLAB_TOKEN")
      (magit-get "gitlab" "oauth-token")
      (user-error "You need to generate a gitlab oauth token")))

(defun org-sync-gitlab-private-token ()
  "Get user private token."
  (or org-sync-gitlab-private-token
      (getenv "GITLAB_API_PRIVATE_TOKEN") ; used by Gitlab ruby client
      (magit-get "gitlab" "private-token")
      (user-error "You need to generate a gitlab private token.  https://gitlab.com/profile/personal_access_tokens")))

(defun org-sync-gitlab-request-headers ()
  "Return an alist with http headers used to call the Gitlab api."
  (cons (cl-case org-sync-gitlab-api-auth-method
          (oauth-token   `("Authorization" . ,(format "Bearer %s" (org-sync-gitlab-token))))
          (private-token `("Private-Token" . ,(org-sync-gitlab-private-token))))
        org-sync-gitlab-default-headers))

(cl-defun org-sync-gitlab-request-log-callback (&key data response error-thrown &allow-other-keys)
  "Callback to log the last request to travis-ci api."
  (and error-thrown (message (error-message-string error-thrown)))
  (and org-sync-gitlab-log
       (with-current-buffer (get-buffer-create org-sync-gitlab-log-buffer)
         (erase-buffer)
         (insert (propertize "DATA:\n" 'face font-lock-constant-face))
         (insert (format "%S\n\n" data))
         (insert (propertize "ERROR:\n" 'face font-lock-constant-face))
         (insert (format "%S\n\n" error-thrown))
         (insert (propertize "HEADERS:\n" 'face font-lock-constant-face))
         (insert (format "%S" (request-response--raw-header response))))))

(cl-defun org-sync-gitlab-request (endpoint
                                   &key
                                   (type "GET")
                                   (data nil)
                                   (sync nil)
                                   (params nil)
                                   (parser 'org-sync-json-read)
                                   )
  "Request to Gitlab api ENDPOINT."
  (request (url-expand-file-name endpoint org-sync-gitlab-api-baseurl)
           :type type
           :data data
           :sync sync
           :params params
           :headers (org-sync-gitlab-request-headers)
           :complete 'org-sync-gitlab-request-log-callback
           :parser parser))

(defun org-sync-gitlab-read-endpoint ()
  "Read an endpoint to be used to fetch the issues."
  (cl-multiple-value-bind (context name)
      (org-sync-read-multiple-choice "Issues by?"
                                     '((?u "user issues")
                                       (?g "group issues")
                                       (?e "enter project")
                                       (?r "git remote")))
    (message "Reading Gitlab endpoint by %s" name)
    (cl-ecase context
      (?u "issues")
      (?g (let ((group-id (read-string "Group ID: ")))
            (cl-assert (not (string-blank-p group-id)) nil "Organization must not be an empty string")
            (format "groups/%s/issues" group-id)))
      (?e (let ((project (read-string "Project (owner/repo): ")))
            (cl-assert (not (string-blank-p project)) nil "Organization must not be an empty string")
            (format "projects/%s/issues" (url-hexify-string project))))
      (?r (cl-multiple-value-bind (host slug)
              (org-sync-parse-remote-or-error (magit-get "remote" (magit-read-remote "Remote") "url"))
            (or (string= host "gitlab.com") (lwarn 'org-sync :warning (format "Repository \"%s\" at \"%s\" host might not be a Gitlab repository" slug host)))
            ;; XXX: url encode "user/repo" to avoid to know first hand the project_id.
            ;;      See: https://docs.gitlab.com/ee/api/projects.html#get-single-project
            (if (y-or-n-p (format "Detected repository as \"%s\", is this correct? " slug))
                (format "projects/%s/issues" (url-hexify-string slug))
              (user-error "Could not guess repository from remote")))))))

(defclass org-sync-issue-gitlab (org-sync-issue)
  ((assignee      :initarg :assignee      :initform nil)
   (subscribed    :initarg :subscribed    :initform nil)
   (confidential  :initarg :confidential  :initform nil)))


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

(cl-defmethod org-sync/valid-p ((_backend (eql gitlab)) metadata)
  (org-sync-assoc-default "OS_GITLAB_ENDPOINT" metadata))

(cl-defmethod org-sync/metadata ((_backend (eql gitlab)))
  `(("OS_GITLAB_APIURL"     . ,(or (org-entry-get nil "OS_GITLAB_APIURL")
                                   org-sync-gitlab-api-baseurl))
    ("OS_GITLAB_PROJECT_ID" . ,(or (org-entry-get nil "OS_GITLAB_PROJECT_ID" t)
                                   (user-error "Incorrectly configured")))))

(cl-defmethod org-sync/issue-new ((_backend (eql gitlab)) metadata issue-data)
  (let* ((object `((title       . ,(plist-get issue-data :title))
                   (labels      . ,(string-join (plist-get issue-data :tags) ","))
                   (description . ,(plist-get issue-data :description))))
         (response (org-sync-gitlab-request (format "projects/%s/issues" (org-sync-assoc-default "OS_GITLAB_PROJECT_ID" metadata))
                                            :type "POST"
                                            :data (org-sync-json-encode object)
                                            :sync t)))
    (if-let (err (request-response-error-thrown response))
        (signal (car err) (cdr err))
      (sit-for .5)                      ; FIXME: wait for response be parsed
      (org-sync-gitlab-issue (request-response-data response)))))

(cl-defmethod org-sync/issues ((_backend (eql gitlab)))
  "Get issues from Gitlab backend with params with CONTEXT."
  (let ((response (org-sync-gitlab-request (org-sync-gitlab-read-endpoint) :params org-sync-gitlab-issues-params :sync t)))
    (if-let (err (request-response-error-thrown response))
        (signal (car err) (cdr err))
      (sit-for .5)                      ; FIXME: wait for response be parsed
      (mapcar #'org-sync-gitlab-issue (request-response-data response)))))

;;;###autoload
(with-eval-after-load 'org-sync
  (add-to-list 'org-sync-backends 'gitlab))

(provide 'org-sync-gitlab)
;;; org-sync-gitlab.el ends here
