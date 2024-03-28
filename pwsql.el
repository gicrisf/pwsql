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
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Description
;;
;;; Code:
(require 'dash)
(require 'deferred)

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
  (if (vectorp (cdr (car data-tree)))
        ; Get titles as symbols from the results' tree
      (let* ((syms (mapcar (lambda (x) (car x)) data-tree))
                ; Collect the respective strings.
             (titles (mapcar (lambda (x) (string-trim (symbol-name x))) syms))
                ; Values are given as vectors
             (vecs (mapcar (lambda (x) (cdr x)) data-tree))
                ; Convert to sequences
             (cols (mapcar (lambda (x) (append x nil)) vecs))
                ; Matrix rotation
             (vals (apply #'-zip-lists cols)))
        ; Properly nested sequences
        `(,titles
          ,@vals))
    (message "Cannot format as table:\n%s" (pp data-tree))))

(defun pwsql--table-as-plist (table)
  "Convert an org TABLE in a comfortable plist.
e.g. goes from =((1 2 3) (4 5 6))=
to =((:3 6 :2 5 :1 4))="
  (cl-labels
      ((recursive-zipper
        (symnames resvalues acc)
        ; "table titles, single result value, accumulator"
        (if (null resvalues)
            acc
          (let ((plist-el (list
                           (intern (concat ":" (format "%s" (car symnames))))
                                (car resvalues))))
            (recursive-zipper (cdr symnames)
                              (cdr resvalues)
                              (append plist-el acc))))))
    (mapcar (lambda (value)
              (recursive-zipper
               (car table)
               value
               nil))
            (cdr table))))

(defun pwsql--expand-tree (res-tree)
  (cl-labels
      ((recexpand
        (res-tree acc)
        (if (or (null (cdr (car res-tree)))
                (< (seq-length (cdr (car res-tree))) 1))
            acc
          (let ((new-acc (cons
                          (mapcar
                           (lambda (el)
                             (cons (car el) (aref (cdr el) 0)))
                           res-tree)
                          acc))
                (new-res-tree
                 (mapcar
                  (lambda (el)
                    (cons (car el) (seq-drop (cdr el) 1)))
                  res-tree)))
            (recexpand new-res-tree new-acc)))))
  (if (not (vectorp (cdr (car res-tree))))
      (list res-tree)
    (recexpand res-tree nil))))

(defun pwsql-get (query &rest args)
  (let* ((as (plist-get args :as))
         (as (if as as 'tree))
         (res (if (or (equal as 'raw)
                      (equal as 'tree)
                      (equal as 'table)
                      (equal as 'expanded-tree))
                  (pwsql--query query)
                (error "The provided :as value is not a viable option")))
         (res (if (or (equal as 'tree)
                      (equal as 'table)
                      (equal as 'expanded-tree))
                  (json-read-from-string res)
                res))
         (res (if (equal as 'table)
                  (pwsql--display-as-org-table res)
                res))
         (res (if (equal as 'expanded-tree)
                  (pwsql--expand-tree res)
                res)))
    res))

(defun pwsql-async-get (query &rest args)
  (lexical-let* ((as (plist-get args :as))
                 ;; defaults on tree
                 (as (if as as 'tree))
                 ;; check :as and pass query if it's fine
                 (query (if (or (equal as 'raw)
                                (equal as 'tree)
                                (equal as 'table)
                                (equal as 'expanded-tree))
                            query
                          (error "The provided :as value is not a viable option"))))
    (deferred:$
      (deferred:next
        (lambda ()
          (pwsql--query query)))
      (deferred:error it
        (lambda (e)
          (message "PWSQL query error: %s" e)))
      (deferred:nextc it
        (lambda (res)
          (if (or (equal as 'tree)
                  (equal as 'table)
                  (equal as 'expanded-tree))
              (json-read-from-string res)
            res)))
      (deferred:nextc it
        (lambda (res)
          (if (equal as 'table)
              (pwsql--display-as-org-table res)
            res)))
      (deferred:nextc it
        (lambda (res)
          (if (equal as 'expanded-tree)
              (pwsql--expand-tree res)
            res)))
      (deferred:error it
        (lambda (e)
          (message "PWSQL conversion error: %s" e))))))

(provide 'pwsql)
;;; pwsql.el ends here
