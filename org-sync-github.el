;;; org-sync-github.el --- Synchronize GitHub issues  -*- lexical-binding: t -*-

;; Copyright (c) 2015 Mario Rodas <marsam@users.noreply.github.com>

;; Author: Mario Rodas <marsam@users.noreply.github.com>
;; URL:
;; Keywords: convenience
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (pkg-info "0.6") (request "0.2.0"))

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
  (require 'cl-lib))

(require 'eieio)
(require 'subr-x)
(require 'request)
(require 'org-sync)
(require 'magit-git)

(declare-function pkg-info-version-info "pkg-info" (library))

;;; Customize
(defgroup org-sync-github nil
  "Syncronice org with GitHub issues."
  :prefix "org-sync-"
  :group 'org-sync)

(defcustom org-sync-github-api-baseurl "https://api.github.com"
  "Base url for Github API."
  :type 'string
  :safe #'stringp
  :group 'org-sync-github)

(defcustom org-sync-github-api-auth-method nil
  "Default authentication method used to make request to GitHub API."
  :type '(choice (string :tag "Oauth token"     oauth-token)
                 (const  :tag "Unauthenticated" nil))
  :safe #'symbolp
  :group 'org-sync-github)

(defcustom org-sync-github-issues-params
  '(("per_page" . "100") ; XXX: max allowed https://developer.github.com/v3/#pagination
    ("state"    . "all"))
  "Default params to call GitHub API to fetch all issues."
  :type 'list
  :safe #'listp
  :group 'org-sync-github)

(defcustom org-sync-github-token nil
  "Default access token to call the GitHub API."
  :type 'string
  :safe #'stringp
  :group 'org-sync-github)

(defvar org-sync-github-default-headers
  ;; XXX: https://developer.github.com/changes/2016-05-12-reactions-api-preview/
  `(("Accept"     . "application/vnd.github.squirrel-girl-preview")
    ("User-Agent" . ,(format "org-sync.el/%s" org-sync-version))))


(defclass org-sync-issue-github (org-sync-issue)
  ((locked     :initarg :locked     :initform nil)
   (assignees  :initarg :assignees  :initform nil)))

(defun org-sync-github-token ()
  "Get github token."
  (or org-sync-github-token
      (getenv "GITHUB_TOKEN")           ; used by "hub"
      (magit-get "github" "oauth-token")
      (user-error "You need to generate a personal access token.  https://github.com/settings/tokens")))

(defun org-sync-github-request-headers ()
  "Return an alist with http headers used to call the GitHub api."
  (cons (cl-case org-sync-github-api-auth-method
          (oauth-token   `("Authorization" . ,(format "Bearer %s" (org-sync-github-token)))))
        org-sync-github-default-headers))

(defun org-sync-github-read-endpoint ()
  "Read an endpoint to be used to fetch the issues."
  (cl-multiple-value-bind (context name)
      (org-sync-read-multiple-choice "Issues by?"
                                     '((?u "user issues")
                                       (?o "organization issues")
                                       (?e "enter project")
                                       (?r "git remote")))
    (message "Reading GitHub endpoint by %s" name)
    (cl-ecase context
      (?u "/issues")
      (?o (let ((organization (read-string "Organization: ")))
            (cl-assert (not (string-blank-p organization)) nil "Organization must not be an empty string")
            (format "/orgs/%s/issues" organization)))
      (?e (let ((project (read-string "Project (owner/repo): ")))
            (cl-assert (not (string-blank-p project)) nil "Project must not be an empty string")
            (format "/repos/%s/issues" project)))
      (?r (cl-multiple-value-bind (host slug)
              (org-sync-parse-remote-or-error (magit-get "remote" (magit-read-remote "Remote") "url"))
            (or (string= host "github.com") (lwarn 'org-sync :warning (format "Repository \"%s\" at \"%s\" host might not be a GitHub repository" slug host)))
            (if (y-or-n-p (format "Detected repository as \"%s\", is this correct? " slug))
                (format "/repos/%s/issues" slug)
              (user-error "Could not guess repository from remote")))))))

(cl-defun org-sync-github-request (endpoint
                                   &key
                                   (type "GET")
                                   (data nil)
                                   (sync nil)
                                   (params nil)
                                   (parser 'org-sync-json-read)
                                   )
  "Request to GitHub api ENDPOINT."
  (request (url-expand-file-name endpoint org-sync-github-api-baseurl)
           :type type
           :data data
           :sync sync
           :params params
           :headers (org-sync-github-request-headers)
           :error (lambda (&rest _)          ; TODO: Add proper error handling
                    (message "Something bad happened..."))
           :parser parser))

(defun org-sync-github-issue (data)
  "Given a DATA response from GitHub issue api return an `org-sync-issue'."
  (let-alist data
    (make-instance 'org-sync-issue-github
                   ;; NOTE: Don't use .id, because is global
                   :id          .number
                   :url         .html_url
                   :tags        (org-sync-assoc-list "name" .labels)
                   :title       .title
                   :status      (pcase .state
                                  ("open"   'todo)
                                  ("closed" 'done)
                                  ;; Default to todo for unknown .status
                                  (_        'todo))
                   :comments    .comments
                   :locked      (org-sync-json-truthy .locked)
                   :milestone   .milestone.number
                   :assignees   (org-sync-assoc-list "login" .assignees)
                   :closed-at   .closed_at
                   :closed-by   .closed_by.login
                   :created-at  .created_at
                   :created-by  .user.login
                   :updated-at  .updated_at
                   :description .body)))

(cl-defmethod org-sync/issues ((_backend (eql github)))
  "Get issues from GitHub backend."
  (let ((response (org-sync-github-request (org-sync-github-read-endpoint) :params org-sync-github-issues-params :sync t)))
    (if-let (err (request-response-error-thrown response))
        (signal (car err) (cdr err))
      (sit-for .5)                      ; FIXME: wait for response be parsed
      (mapcar #'org-sync-github-issue (request-response-data response)))))

;;;###autoload
(with-eval-after-load 'org-sync
  (add-to-list 'org-sync-backends 'github))

(provide 'org-sync-github)
;;; org-sync-github.el ends here
