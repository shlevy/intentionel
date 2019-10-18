;;; intentionel-test.el --- Tests for intentionel

;;; Commentary:

;; Unit tests for intentionel

;;; Code:

(require 'org)
(require 'ert)
(require 'intentionel)
(require 'seq)
(require 'org-brain)

(declare-function test-file "test-helper")

(ert-deftest intentionel-test-active-task-p ()
  "Tests active task detection predicate."
  (with-current-buffer (find-file-noselect (test-file "active-task.org"))
    (while (progn
	     (should (equal (intentionel--active-task-p (point)) (string= "t" (org-entry-get nil "active"))))
	     (outline-next-heading)))
    ))

(ert-deftest intentionel-test-org-children ()
  "Tests detection of direct children in the org layout."
  (let* ((parent (org-brain-entry-marker '("children" "Parent" "1ce6322e-04c4-4b11-bbe3-41124a9d4373")))
	 (org-children (intentionel--org-children parent))
	 (ids (seq-map (lambda (p) (org-entry-get p "test_id")) org-children))
	 (ids-list (seq-into ids 'list)))
    (should (equal ids-list '("1" "3")))))

;;; intentionel-test.el ends here
