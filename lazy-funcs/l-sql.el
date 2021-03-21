;;
;; @author Laura Viglioni
;; 2021
;; GNU Public License 3.0
;;

;;
;; sql related functions
;;

;;
;; add postgres/mysql dbs to lsp and emacs
;;

;; Variables related to sql configs
(setq lsp-sqls-connections nil)
(setq sql-connection-alist nil)

;;;###autoload
(defun format-postgres-sqls (host port user password db)
  (format "host=%s port=%s user=%s password=%s dbname=%s"
          host port user password db))

;;;###autoload
(defun format-mysql-sqls (host port user password db)
  (format "%s:%s@tcp(%s:%s)/%s" user password host port db))

;;;###autoload
(defun format-postgres-uri (host port user password db)
  (format "postgresql://%s:%s@%s:%s/%s" user password host port db))


;;;###autoload
(defun add-to-sqls-connections (db-type data-src-name)
  (add-to-list 'lsp-sqls-connections
               (list (cons 'driver db-type)
                     (cons 'dataSourceName data-src-name))))

;;;###autoload
(defmacro add-to-sql-conection-alist (db-type name host port user password db)
  `(add-to-list 'sql-connection-alist
                (list (quote ,name)
                     (list 'sql-product (quote ,db-type))
                     (list 'sql-user ,user)
                     (list 'sql-server ,host)
                     (list 'sql-port ,port)
                     (list 'sql-password ,password)
                     (list 'sql-database ,db))))

;;;###autoload
(defmacro sql-add-postgres-db (name &rest db-info)
  "Adds a mysql database to emacs and lsp
   This macro expects a name to the database and a p-list of parameters
   :port, :user, :password, :database, :host
   The only optional is :port, its default value is 5432
   e.g.:
   (sql-add-postgres-db
        my-db-name ;; notice that there are no quotes here
        :port 1234
        :user \"username\"
        :host \"my-host\"
        :database \"my-db\"
        :password \"mypassword\")"
  `(let ((port (or ,(plist-get db-info :port) 5432))
         (user ,(plist-get db-info :user))
         (password ,(plist-get db-info :password))
         (host ,(plist-get db-info :host))
         (db ,(plist-get db-info :database)))
     (throw-if (any-nil? user password host db (quote ,name)) "there are info missing!")
     (let ((full-uri (format-postgres-uri host port user password db))
           (data-src-name (format-postgres-uri host port user password db)))
       (add-to-sqls-connections "postgresql" data-src-name)
       (add-to-sql-conection-alist 'postgres ,name host port user password full-uri))))

;;;###autoload
(defmacro sql-add-mysql-db (name &rest db-info)
  "Adds a mysql database to emacs and lsp
   This macro expects a name to the database and a p-list of parameters
   :port, :user, :password, :database, :host
   The only optional is :port, its default value is 3306
   e.g.:
   (sql-add-mysql-db
        my-db-name ;; notice that there are no quotes here
        :port 1234
        :user \"username\"
        :host \"my-host\"
        :database \"my-db\"
        :password \"mypassword\")"
  `(let ((port (or ,(plist-get db-info :port) 3306))
         (user ,(plist-get db-info :user))
         (password ,(plist-get db-info :password))
         (host ,(plist-get db-info :host))
         (db ,(plist-get db-info :database)))
     (throw-if (any-nil? user password host db (quote ,name)) "there are info missing!")
     (add-to-sqls-connections "mysql" (format-mysql-sqls host port user password db))
     (add-to-sql-conection-alist 'mysql ,name host port user password db)))
