;;; test-helper.el --- Helper functions/settings for the intentionel test suite

;;; Commentary:

;; Put all non-test prep work here

;;; Code:

(require 'org)
(require 'seq) ; Needed until https://github.com/Kungsgeten/org-brain/pull/219 is merged
(require 'org-brain)

(setq org-todo-keywords
      '((sequence "BACKLOG(b)" "TODO(t)" "IN PROGRESS(p)" "|" "DONE(d)")))

(defconst test-helper-here (file-name-directory load-file-name))

(defun test-file (f)
  "Find a file F in the test dir."
  (concat test-helper-here f))

(setq org-brain-path test-helper-here)
(org-brain-update-id-locations)

;;; test-helper.el ends here
