;;; github --- interact with github pull requests

;; Copyright (C) 2021 Brian Kubisiak <brian@kubisiak.com>
;;
;; Author: Brian Kubisiak <brian@kubisiak.com>

;;; Commentary:

;; Github code reviews in Emacs

;;; Code:

(defvar gh-repo
  "Skydio/aircam"
  "Github owner/repo to query for reviews.")

(defvar gh-user
  "brian-kubisiak-skydio"
  "Github account username.")

(defun gh-pr-query ()
  "Retrieve the graphql query to use for getting the pr overview."
  ;; TODO: get test results too
  (concat
   "query {
     search(last:100, query:\"is:pr is:open involves:@me repo:"
   gh-repo
   "\", type:ISSUE) {
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
    }"))

(defun gh--parse-pr-query (query)
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

(defun gh--load-prs ()
  "Query the graphql api for pr data."
  (let ((json-data
         (with-temp-buffer
           (call-process "gh" nil t nil "api" "graphql" "-f"
                         (concat "query=" (gh-pr-query)))
           (goto-char (point-min))
           (json-parse-buffer))))
    (gh--parse-pr-query json-data)))

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

(defun gh-refresh-buffer ()
  "Refresh the github data in the current buffer."
  (interactive)
  (let ((name (buffer-name)))
    (cond
     ((equal name "*github-prs*")
      (gh--refresh-prs)))))

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

(defun gh-open-buffer ()
  "Open a new github-mode buffer."
  (interactive)
  (let ((buffer (get-buffer-create "*github-prs*")))
    (switch-to-buffer buffer)
    (gh--refresh-prs)))

(provide 'github)

;;; github.el ends here
