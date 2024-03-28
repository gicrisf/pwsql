# pwsql

This Emacs package provides a way to interact with a database using PowerShell under the hood.

## Usage

The `pwsql-get` function provides a simplified interface for users to execute any SQL query. It can return the results in various formats such as raw JSON, two kind of structured trees, or as an Org-mode table.

``` emacs-lisp
(pwsql-get QUERY &rest ARGS)
```

- QUERY: A string containing the SQL query to be executed.
- ARGS: Optional keyword arguments to specify the output format. The supported arguments are:
    - `:as`: Specifies the output format. Possible values are:
        - `'raw`: Returns the raw string passed by the powershell.
        - `'tree`: Returns the structured tree format of the query results.
        - `'expanded-tree`: Returns the expanded tree format of the query results.
        - `'table`: Displays the query results as an Org-mode table.

## Examples

Execute a SQL query and retrieve the results as raw JSON string:

```emacs-lisp
(pwsql-get "SELECT * FROM TableName" :as 'raw)
```

Execute a SQL query and retrieve the results as a structured tree:

```emacs-lisp
(pwsql-get "SELECT * FROM TableName" :as 'tree)
```

Execute a SQL query and display the results as an Org-mode table:

```emacs-lisp
(pwsql-get "SELECT * FROM TableName" :as 'table)
```

Execute a SQL query and retrieve the results as an expanded tree:

```emacs-lisp
(pwsql-get "SELECT * FROM TableName" :as 'expanded-tree)
```

I usually build my queries using a lisp DSL provided by [emacsql](https://github.com/magit/emacsql).

```emacs-lisp
(pwsql-get 
 (emacsql-format
  (emacsql-prepare
   `[:select [name id]
   :from people
   :where (> salary $s1)]) 
  50000)  
 :as 'table)
```

If you often run the same queries or variations of them, you can wrap them in functions!

```emacs-lisp
(defun some-query (salary)
 (emacsql-format
  (emacsql-prepare
   `[:select [name id]
   :from people
   :where (> salary $s1)]) 
  salary))
```

From which:

```emacs-lisp
(pwsql-get (some-query 50000) :as 'table)
```

If you run this from an org-babel block, it gives you the table.

## Configuration

Make sure your config file has the appropriate authentication credentials configured (server instance, username, password):

```emacs-lisp
(setq pwsql-server-instance "YOUR-SQL-INSTANCE")
(setq pwsql-username "your-username")
(setq pwsql-password "your-password") ;; Passwords shouldn't be left in plain sight.
```

A safer way to store your password is to store it as an environment variable and get it with `(getenv "PWSQLPWD")` or something similar.

## Why

`emacsql` is primarily aimed at SQLite databases. If your project requires access to databases that are not SQLite, using PowerShell can extend the compatibility to a wider range of databases. In particular, I developed this solution to work with MS-SQL (Microsoft SQL Server) in a Windows environment.

It's also a fast solution. You set your credentials and you're ready to go.

## Support
Have you found this package useful and would like to say thank you?

Why don't you help me keep myself awake by buying me a coffee?
I could use the extra energy to add more features!

[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/V7V425BFU)

<a href="https://liberapay.com/gicrisf/donate"><img alt="Donate using Liberapay" src="https://liberapay.com/assets/widgets/donate.svg"></a>
