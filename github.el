;;; github --- interact with github pull requests

;; Copyright (C) 2021 Brian Kubisiak <brian@kubisiak.com>
;;
;; Author: Brian Kubisiak <brian@kubisiak.com>

;;; Commentary:

;; Github code reviews in Emacs

;;; Code:

(defvar gh-owner
  "Skydio"
  "Owner of the github repo to query for reviews.")

(defvar gh-repo
  "aircam"
  "Github repo to query for reviews.")

(defvar gh-user
  "brian-kubisiak-skydio"
  "Github account username.")

(defun gh-prs-query ()
  "Retrieve the graphql query to use for getting the pr overview."
  ;; TODO: get test results too
  (format
   "query {
     search(last:100, query:\"is:pr is:open involves:@me repo:%s/%s\", type:ISSUE) {
      edges {
       node {
       ... on PullRequest {
        author {
         login
        }
        title
        labels(last:4) {
         edges {
          node {
           name
          }
         }
        }
        number
        reviews(last:100) {
         edges {
          node {
           state
          }
         }
        }
       }
       }
      }
     }
    }" gh-owner gh-repo))

(defun gh--parse-all-prs-query (query)
  "Parse the QUERY data from a graphql pr request."
  (let ((data (gethash "edges" (gethash "search" (gethash "data" query)))))
    (seq-map 'gh--parse-pr-queryline data)))

(defun gh--parse-pr-queryline (queryline)
  "Convert QUERYLINE into a readable line for the output buffer."
  (let* ((data (gethash "node" queryline))
         (number (gethash "number" data))
         (title (gethash "title" data))
         (author (gethash "login" (gethash "author" data)))
         (reviews (gethash "edges" (gethash "reviews" data)))
         (labels
          (seq-map
           (lambda (label)
             (gethash "name" (gethash "node" label)))
           (gethash "edges" (gethash "labels" data)))))
    (list
     number
     title
     author
     (gh--count-approvals reviews)
     (gh--in-merge-queue labels)
     (gh--pr-size labels)
     (gh--filter-labels labels))))

(defun gh-pr-query (number)
  "Retrieve the graphql query for getting an overview of a specific pr NUMBER."
  (format
   "query {
     repository(owner:\"%s\", name:\"%s\") {
      pullRequest(number:%d) {
       title
       body
       headRefName
       headRefOid
       author {
        login
       }
       files(first:100) {
        edges {
         node {
          path
          additions
          deletions
         }
        }
       }
      }
     }
    }" gh-owner gh-repo number))

(defun gh--parse-single-pr-query (query)
  "Parse the QUERY result for a single pr summary."
  (let* ((data (gethash "pullRequest"
                        (gethash "repository"
                                 (gethash "data" query))))
         (title (gethash "title" data))
         (body (gethash "body" data))
         (branch-name (gethash "headRefName" data))
         (commit (gethash "headRefOid" data))
         (author (gethash "login" (gethash "author" data)))
         (files (gethash "edges" (gethash "files" data))))
    (list
     commit
     branch-name
     author
     title
     body
     (seq-map (lambda (file)
                (let ((data (gethash "node" file)))
                  (list (gethash "path" data)
                        (gethash "additions" data)
                        (gethash "deletions" data))))
              files))))

(defun gh--filter-labels (labels)
  "Remove merge-queue and size from LABELS."
  (seq-filter
   (lambda (label)
     (not (or (equal "merge-queue" label)
              (string-prefix-p "size/" label))))
   labels))

(defun gh--in-merge-queue (labels)
  "Check if LABELS contain the merge-queue label."
  (seq-some
   (lambda (label)
     (equal label "merge-queue"))
   labels))

(defun gh--pr-size (labels)
  "Get the pr size from the given LABELS."
  (seq-some
   (lambda (label)
     (if (string-prefix-p "size/" label)
         (substring label 5)
       nil))
   labels))

(defun gh--count-approvals (reviews)
  "Count how many REVIEWS are approvals."
  (seq-reduce
   (lambda (acc review)
     "Accumulate approved reviews."
     (let* ((data (gethash "node" review))
            (state (gethash "state" data)))
       (if (equal state "APPROVED")
           (+ acc 1)
         acc)))
   reviews
   0))

(defun gh--load-query (file)
  "Load a gh query from FILE (for debugging)."
  (let ((query-buffer (find-file-noselect file t)))
    (with-current-buffer query-buffer
      (unwind-protect
          (json-parse-buffer)
        (kill-buffer query-buffer)))))

(defun gh--load (query)
  "Load the data from github using the given graphql QUERY."
  (with-temp-buffer
    (call-process "gh" nil t nil "api" "graphql" "-f" (concat "query=" query))
    (goto-char (point-min))
    (json-parse-buffer)))

(defun gh--load-prs ()
  "Query the graphql api for pr data."
  (let ((json-data (gh--load (gh-prs-query))))
    (gh--parse-all-prs-query json-data)))

(defun gh--load-pr (number)
  "Query the graphql api for an overview of a specific pr NUMBER."
  (let ((json-data (gh--load (gh-pr-query number))))
    (gh--parse-single-pr-query json-data)))

(defun gh--format-pr-title (title author)
  "Format TITLE and AUTHOR to a suitable fixed-width format for the pr buffer."
  (let ((width 70)
        (titleauthor (format "%s  (%s)" title author)))
    (if (< (length titleauthor) width)
        (format "%-70s" titleauthor)
      (concat (substring titleauthor 0 (- width 3)) "..."))))

(defun gh--format-labels (labels)
  "Format LABELS for inserting into the pr buffer."
  (mapconcat 'identity labels ","))

(defun gh--insert-pr-data (pr)
  "Insert PR data into the current buffer."
  (let* ((number (car pr))
         (nrest (cdr pr))
         (title (car nrest))
         (trest (cdr nrest))
         (author (car trest))
         (arest (cdr trest))
         (approvals (car arest))
         (aprest (cdr arest))
         (in-mergequeue (car aprest))
         (mrest (cdr aprest))
         (size (car mrest))
         (srest (cdr mrest))
         (labels (car srest)))
  (insert (format "%6d" number) " "
          (gh--format-pr-title title author) " "
          (format "[%2d]" approvals) " "
          (format "[%s]" (if in-mergequeue "X" " ")) " "
          (format "%3s" size) " "
          (gh--format-labels labels) "\n")))

(defun gh--insert-pr-summary (pr)
  "Insert PR summary data into the current buffer."
  (let* ((commit (car pr))
         (crest (cdr pr))
         (branch (car crest))
         (brest (cdr crest))
         (author (car brest))
         (arest (cdr brest))
         (title (car arest))
         (trest (cdr arest))
         (body (car trest))
         (borest (cdr trest))
         (files (car borest)))
    (insert
     (format "branch: %s\n" branch)
     (format "commit: %s\n" commit)
     (format "author: %s\n\n" author)
     (format "%s\n\n%s\n\n" title body))
    (seq-do (lambda (file)
              (insert (gh--summarize-file file)))
            files)))

(defun gh--summarize-file (file)
  "Generate a string summarizing FILE, similar to git diff --stat."
  (let ((path (car file))
        (additions (cadr file))
        (deletions (caddr file)))
    (format "%-80s | +%5d -%5d\n" path additions deletions)))

(defun gh-refresh-buffer ()
  "Refresh the github data in the current buffer."
  (interactive)
  (let ((inhibit-read-only t)
        (name (buffer-name)))
    (cond
     ((equal name "*github-prs*")
      (gh--refresh-prs))
     ((string-match
       (rx "*github-pr-" (group (one-or-more digit)) "*")
       name)
      (gh--refresh-pr (string-to-number (match-string 1 name))))
     (t (message "unrecognized github buffer")))))

(defun gh-move-up-buffer ()
  "Move 'up' one level in the page heirarchy."
  (interactive)
  (let ((name (buffer-name)))
    (cond
     ((string-match
       (rx "*github-pr-" (one-or-more digit) "*")
       name)
      (gh-open-buffer))
     (t (message (format "can't move up from %s" name))))))

(defun gh--refresh-prs ()
  "Refresh the current buffer with toplevel pr data."
  (erase-buffer)
  (let ((pr-data (gh--load-prs)))
    (insert "* outgoing\n\n")
    (seq-do 'gh--insert-pr-data
            (seq-filter (lambda (pr)
                          (equal gh-user (caddr pr)))
                        pr-data))
    (insert "\n\n* incoming\n\n")
    (seq-do 'gh--insert-pr-data
            (seq-filter (lambda (pr)
                          (not (equal gh-user (caddr pr))))
                        pr-data))))

(defun gh--refresh-pr (number)
  "Refresh the current buffer with a summary of pr NUMBER."
  (erase-buffer)
  (let ((pr-data (gh--load-pr number)))
    (gh--insert-pr-summary pr-data)))

(defun gh-open-buffer ()
  "Open a new github-mode buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*github-prs*")))
    (switch-to-buffer buffer)
    (if (= (buffer-size buffer) 0)
        (gh--refresh-prs))
    (github-mode)))

(defun gh-open-pr-summary (pr-number)
  "Open a summary of PR-NUMBER in a new buffer."
  (interactive "npr number: ")
  (let* ((pr-buffer-name (format "*github-pr-%d*" pr-number))
         (buffer (get-buffer-create pr-buffer-name)))
    (switch-to-buffer buffer)
    (if (= (buffer-size buffer) 0)
        (gh--refresh-pr pr-number))
    (github-mode)))

(defun gh-select-pr-summary ()
  "Open a pr summary for the pr specified in the line at point."
  (interactive)
  (let* ((pr-oneline (thing-at-point 'line))
         (pr-number (string-to-number pr-oneline)))
    (if (eq pr-number 0)  ;; user selected an invalid line
        nil
      (gh-open-pr-summary pr-number))))


(defvar github-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-x RET") 'gh-select-pr-summary)
    (define-key map (kbd "C-x u") 'gh-move-up-buffer)
    map)
  "Keymap for github-mode.")

(define-minor-mode github-mode
  "Minor mode for navigating github reviews in Emacs."
  :init-value nil
  :lighter " github"
  :keymap github-mode-map)


(provide 'github)

;;; github.el ends here
