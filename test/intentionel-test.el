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

(ert-deftest intentionel-test-org-brain-headline-children ()
  "Tests detection of org-brain children that are headline entries."
  (let* ((parent '("children" "Parent" "1ce6322e-04c4-4b11-bbe3-41124a9d4373"))
	 (brain-children (intentionel--org-brain-headline-children parent))
	 (ids (seq-map (lambda (p) (org-entry-get p "test_id")) brain-children))
	 (ids-list (seq-into ids 'list)))
    (should (equal ids-list '("5")))))

(ert-deftest intentionel-test-active-intention ()
  "Tests detection of active an inactive intentions"
  (should (intentionel--active-p '("children" "Parent" "1ce6322e-04c4-4b11-bbe3-41124a9d4373")))
  (should-not (intentionel--active-p '("children" "Sibling" "815fba5f-3f37-4af9-b200-e74e351de6a5"))))

;;; intentionel-test.el ends here
