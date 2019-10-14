;;; intentionel-test.el --- Tests for intentionel

;;; Commentary:

;; Unit tests for intentionel

;;; Code:

(require 'org)
(require 'ert)
(require 'intentionel)

(setq org-todo-keywords
      '((sequence "BACKLOG(b)" "TODO(t)" "IN PROGRESS(p)" "|" "DONE(d)")))

(defun test-file (f)
  "Find a file F in the test dir."
  (concat (file-name-directory load-file-name) f))

(ert-deftest intentionel-test-active-task-p ()
  "Tests active task detection predicate."
  (with-current-buffer (find-file-noselect (test-file "active-task.org"))
    (while (progn
	     (should (equal (intentionel--active-task-p (point)) (string= "t" (org-entry-get nil "active"))))
	     (outline-next-heading)))
    ))

;;; intentionel-test.el ends here
