;;; pwsql-test.el --- Test runner for pwsql
(require 'ert)

;; Uninstall eventually installed package
(when (package-installed-p 'pwsql)
  (package-delete 'pwsql))

;; Add the directory containing your package to load-path
(add-to-list
 'load-path
 (file-name-directory
  (or load-file-name buffer-file-name)))

;; Temporary load the file
(load "pwsql")
(message "Loading from: %s" (locate-library "pwsql"))

(ert-deftest pwsql-test--display-as-org-table ()
  "Test converting data tree to org table format."
  (let ((input '((col1 . [1 3]) (col2 . [2 4])))
        (expected '(("col1" "col2") (1 2) (3 4))))
    (should (equal (pwsql--display-as-org-table input) expected))))

(ert-deftest pwsql-test--table-as-plist ()
  "Test converting org table to plist format."
  (let ((input '(("a" "b") (1 2) (3 4)))
        (expected '((:b 2 :a 1) (:b 4 :a 3))))
    (should (equal (pwsql--table-as-plist input) expected))))

(defun pwsql-run-tests ()
  "Run all pwsql tests during development."
  (interactive)
  (ert-run-tests-interactively "pwsql-"))

(provide 'pwsql-test-runner)
