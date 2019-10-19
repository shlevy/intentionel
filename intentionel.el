;;; intentionel.el --- Intention tracking with org-brain  -*- lexical-binding: t -*-

;; Copyright (C) 2019 Shea Levy

;; Author: Shea Levy
;; URL: https://github.com/shlevy/intentionel
;; Version: 1.0.0
;; Package-Requires: (org stream org-brain)

;;; Commentary:

;; This package captures the automated aspects of my usage of org-mode
;; and org-brain to structure my intentional actions.  At best it is
;; most likely to remain a work-in-progress meeting only my needs at
;; the moment (and most likely it will simply be abandoned eventually
;; for some other approach), so if you're not me and you're using this
;; caveat progammator applies even more than usual.

;;; Code:

(require 'subr-x)
(require 'org)
(require 'stream)
(require 'org-brain)

(defun intentionel--active-task-p (pom)
  "Is the org entry at POM an \"active\" task?

An active task is one that will show up on the agenda, i.e. is in
TODO state, in IN PROGRESS state, is scheduled, or has a deadline."
  (org-with-point-at pom
    (when-let ((props (org-entry-properties))
	       (state (cdr (assoc "TODO" props))))
      (when (and (not (string= state "DONE"))
		 (or (string= state "TODO")
		     (string= state "IN PROGRESS")
		     (assoc "SCHEDULED" props)
		     (assoc "DEADLINE" props)))
	t))))

(defun intentionel--org-children (pom)
  "Get the immediate 'org-mode' children of the entry at POM.

Returns a stream of markers."
  (let ((child-level (org-with-point-at pom
		       (+ (funcall outline-level) 1))))
    (cl-labels
	((future-siblings
	  (node)
	  (org-with-point-at node
	    (while (and
		    (outline-next-heading)
		    (> (funcall outline-level) child-level)))
	    (if (and
		 (not (eobp))
		 (= (funcall outline-level) child-level))
		(let ((next-node (point-marker)))
		  (stream-cons next-node (future-siblings next-node)))
	      (stream-empty)))))
      (future-siblings pom))))

(defun intentionel--org-brain-headline-children (entry)
  "Get the immediate 'org-brain' children that are headlines of the ENTRY.

Returns a stream of markers"
  (seq-map
   #'org-brain-entry-marker
   (seq-filter
    (lambda (m) (not (org-brain-filep m)))
    (stream (org-brain-children entry)))))

(defun intentionel--active-p (intention)
  "Is INTENTION active?"
  (let* ((brain-children
	  (intentionel--org-brain-headline-children intention))
	 (org-children
	  (intentionel--org-children
	   (org-brain-entry-marker intention)))
	 (all-children (stream-append brain-children org-children)))
    (seq-some #'intentionel--active-task-p all-children)))

;;;###autoload
(defun intentionel-visualize-first-inactive (root)
  "Visualize the first inactive descendant of ROOT, if any."
  (interactive "sRoot:")
  (when-let
      ((descendants (seq-subseq (org-brain-descendants root) 0 -1))
       (first (seq-find
	       (lambda (d) (not (intentionel--active-p d)))
	       descendants)))
    (org-brain-visualize first)))

(provide 'intentionel)

;;; intentionel.el ends here
