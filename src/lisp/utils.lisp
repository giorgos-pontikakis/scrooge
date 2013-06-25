(in-package :scrooge)


;;; GENERAL UTILITIES

(defun single-item-list-p (list)
  (and (listp list)
       (null (cdr list))))

(defun fmt-amount (amount &optional (decimal-digits 2))
  (let ((format-control (concatenate 'string
                                     "~," (write-to-string decimal-digits) "F")))
    (if (not (numberp amount))
        amount
        (format nil format-control amount))))

(defun lists->alist (lists)
  (mapcar (lambda (list)
            (cons (car list)
                  (cadr list)))
          lists))

(defun getfer (indicator &optional default)
  #'(lambda (place)
      (getf place indicator default)))

(defun mapply (fn arglist-list)
  (mapcar (lambda (arglist)
            (apply fn arglist))
          arglist-list))

(defun factory (fn pre-arg &rest post-args)
  (lambda (&rest args)
    (apply fn pre-arg (append args post-args))))



;;; SQL UTILITIES

(defun get-dao-plist (type &rest args)
  "Like get-dao, but returns a plist instead of an object"
  (query (sql-compile `(:select * :from ,type :where (:and ,@(mapcar (lambda (key val)
                                                                       (list := key val))
                                                                     (dao-keys type)
                                                                     args))))
         :plist))

(defun ilike (filter)
  (if (or (null filter)
          (eq filter :null))
      "%"
      (concatenate 'string "%" filter "%")))

(defmacro define-existence-predicate (name table primary-key)
  "Defines a predicate which takes the value of the primary key field and checks
if it already exists in the database."
  `(defun ,name (,primary-key)
     (with-db ()
       (query (:select 1 :from ',table
               :where (:= ',primary-key ,primary-key))
              :single))))

(defmacro define-existence-predicate* (name table field primary-key)
  "Defines a predicate which takes the value of the field and checks if it
already exists in the database. The predicate also accepts the value of the
primary key of the table; if given, the record with this primary key value is
excluded for the search - useful for updates."
  `(defun ,name (,field &optional ,primary-key)
     (with-db ()
       (if ,primary-key
           (query (:select 1 :from ',table
                   :where (:and (:= ',field ,field)
                                (:not (:= ',primary-key ,primary-key))))
                  :single)
           (query (:select 1 :from ',table
                   :where (:= ',field ,field))
                  :single)))))

(defun referenced-by-sql (account-id table columns)
  `(:select 1 :from ,table :where
            (:or ,@(mapcar (lambda (col)
                             (list ':= account-id col))
                           columns))))

(defun referenced-by (id table &rest columns)
  (with-db ()
    (query (sql-compile (referenced-by-sql id table columns))
           :single)))

(defun children-records (records id &key (parent-key :parent-id))
  "From a set of records (plists), find the direct children of the record with
the given id."
  (remove-if-not (lambda (rec)
                   (eql (getf rec parent-key) id))
                 records))

(defun subtree-records (records id &key (key :id))
  "From a set of records, collect all descendants of the record with the given
id, i.e. all records of the subtree with root specified by the given id."
  (dft (lambda (head)
         (children-records records
                           (getf head key)))
       (find id records :key (getfer :id))))

(defun subtree-record-ids (records id &key (key :id))
  (mapcar (getfer key)
          (subtree-records records id :key key)))
