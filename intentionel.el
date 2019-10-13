;;; intentionel.el --- Intention tracking with org-brain

;; Copyright (C) 2019 Shea Levy

;; Author: Shea Levy
;; URL: https://github.com/shlevy/intentionel
;; Version: 1.0.0
;; Package-Requires: ((org "20190527") (emacs "26.3"))

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

(defun intentionel--active-task-p ()
  "Is the current org entry an \"active\" task?

An active task is one that will show up on the agenda, i.e. is in
TODO state, in IN PROGRESS state, is scheduled, or has a deadline."
  (when-let ((props (org-entry-properties))
	     (state (cdr (assoc "TODO" props))))
    (when (and (not (string= state "DONE"))
	       (or (string= state "TODO")
		   (string= state "IN PROGRESS")
		   (assoc "SCHEDULED" props)
		   (assoc "DEADLINE" props)))
      t)))

(provide 'intentionel)

;;; intentionel.el ends here
