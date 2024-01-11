;;; pwsql.el --- Description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 gicrisf
;;
;; Author: gicrisf <giovanni.crisalfi@protonmail.com>
;; Maintainer: gicrisf <giovanni.crisalfi@protonmail.com>
;; Created: December 05, 2023
;; Modified: December 05, 2023
;; Version: 0.0.2
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

(defvar pwsql-server-instance "PWSQL-INSTANCE")
(defvar pwsql-username "pwsql-usr")
(defvar pwsql-password "pwsql-pwd")

(defun pwsql--query (query-string &rest args)
  "Use a QUERY-STRING to retrieve data from a database through the Powershell.
Return a json string. You can explicit it in ARGS or you can ask for a csv string."
  (let* ((convert-to (or (plist-get args :convert-to) 'json))
         (pscmd (list "$DS = Invoke-Sqlcmd -Query @'\n"
                 query-string
                 (concat
                  (format "\n'@ -ServerInstance '%s' " pwsql-server-instance)
                  (format "-Username '%s' -Password '%s' " pwsql-username pwsql-password)
                  "-As DataSet")
                 "\n$QTable = $DS.Tables"
                 ; Collect data in a custom hashtable
                 "\n$maht = @{}"
                 "\nforeach ($col in $QTable.Columns)"
                 "\n{ $maht.add($col.ColumnName, $QTable.($col.ColumnName)) }"
                 ; The hashtable is returned as a Json/CSV Object
                 (concat "\n$maht | "
                         (pcase convert-to
                           ('json "ConvertTo-Json")
                           ;; TODO support CSV
                           ;; hint: you should re-organize the table
                           ('csv "ConvertTo-Csv"))))))
    (with-temp-buffer
       (let ((status
              (apply 'call-process "powershell.exe" nil (current-buffer) nil pscmd)))
         (unless (eq status 0)
           (error "Pwsql exited with status %s" status))
         (goto-char (point-min))
         (buffer-string)))))

(defun pwsql--display-as-org-table (data-tree)
  "Display a pwsql DATA-TREE as a table in an 'org-mode' buffer."
  (let* ((syms (mapcar (lambda (x) (car x)) data-tree))
         ; You just got titles as symbols from the results' tree
         ; Now, you collect the respective strings.
         (titles (mapcar (lambda (x) (string-trim (symbol-name x))) syms))
         ; Values are given as vectors
         (vecs (mapcar (lambda (x) (cdr x)) data-tree))
         ; Convert to sequences
         (cols (mapcar (lambda (x) (append x nil)) vecs))
         ; Matrix rotation
         (vals (apply #'-zip-lists cols)))

    ; Properly nested sequences
    `(,titles
      ,@vals)))

(defun pwsql--table-as-plist (table)
  "Convert an org TABLE in a comfortable plist."
  (mapcar (lambda (value)
            (cl-labels ((recursive-zipper
                         (symnames resvalues acc)
                         ; table titles, single result value, accumulator
                         (if (null resvalues)
                             acc
                           (let ((plist-el (list (intern (concat ":" (car symnames)))
                                                 (car resvalues))))
                             (recursive-zipper (cdr symnames)
                                               (cdr resvalues)
                                               (append plist-el acc))))))
              (recursive-zipper
               (car table)
               (cdr value)
               nil)))
          (cdr table)))

(provide 'pwsql)
;;; pwsql.el ends here
