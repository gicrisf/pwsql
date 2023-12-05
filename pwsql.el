;;; pwsql.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 gicrisf
;;
;; Author: gicrisf <giovanni.crisalfi@protonmail.com>
;; Maintainer: gicrisf <giovanni.crisalfi@protonmail.com>
;; Created: December 05, 2023
;; Modified: December 05, 2023
;; Version: 0.0.1
;; Keywords: data tools sql
;; Homepage: https://github.com/gicrisf/pwsql
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'dash)

(defvar pwsql-server-instance "CRISALFI-ALTEN")

(defun pwsql--query (query-string)
  "Use a QUERY-STRING to retrieve data from a database through the Powershell."
  (let ((pscmd (list "$DS = Invoke-Sqlcmd -Query @'\n"
                 query-string
                 (format "\n'@ -ServerInstance '%s' -As DataSet" pwsql-server-instance)
                 "\n$QTable = $DS.Tables"
                 ; Collect data in a custom hashtable
                 "\n$maht = @{}"
                 "\nforeach ($col in $QTable.Columns)"
                 "\n{ $maht.add($col.ColumnName, $QTable.($col.ColumnName)) }"
                 ; The custom hashtable is returned as a Json object
                 "\n$maht | ConvertTo-Json")))
    (json-read-from-string
     (with-temp-buffer
       (let ((status (apply 'call-process "powershell.exe" nil (current-buffer) nil pscmd)))
         (unless (eq status 0)
       (error "Pwsql exited with status %s" status))
         (goto-char (point-min))
         (buffer-string))))))

;(res (pwsql-query query-string))

(defun pwsql--display-as-org-table (data-tree)
  "Display a pwsql DATA-TREE as a table in an 'org-mode' buffer."
  (let* ((syms (mapcar (lambda (x) (car x)) data-tree))
         ; You just got titles as symbols from the results' tree
         ; Now, you collect the respective strings.
         (titles (mapcar (lambda (x) (string-trim (symbol-name x))) syms))
         ; Values are given as vectors
         (vecs (mapcar (lambda (x) (cdr x)) data-tree))
         ; Convert to sequences
         (cols (mapcar (lambda (x) (cdr (append x nil))) vecs))
         ; Matrix rotation
         (vals (apply #'-zip cols)))

    ; Properly nested sequences
    `(,titles
      ,@vals)))

(provide 'pwsql)
;;; pwsql.el ends here
