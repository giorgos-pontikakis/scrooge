(in-package :scrooge)


;;;----------------------------------------------------------------------
;;; SQL utilities
;;;----------------------------------------------------------------------

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


;;;----------------------------------------------------------------------
;;; Miscellaneous
;;;----------------------------------------------------------------------

(defun single-item-list-p (list)
           (and list (null (cdr list))))

(defun fmt-amount (amount &optional (decimal-digits 2))
  (let ((format-control (concatenate 'string
                                     "~," (write-to-string decimal-digits) "F")))
    (if (not (numberp amount))
        amount
        (format nil format-control amount))))

(defun dfs (expander-fn root)
  "Depth first search"
  (labels ((dfs-aux (expanded fringe)
             (if (null fringe)
                 expanded
                 (let ((head (first fringe))
                       (tail (rest fringe)))
                   (dfs-aux (list* head expanded)
                            (append (funcall expander-fn head)
                                    tail))))))
    (dfs-aux nil (list root))))

(defun lists->alist (lists)
  (mapcar (lambda (list)
            (cons (car list)
                  (cadr list)))
          lists))

(defun int-5digits-p (num)
  (and (integerp num)
       (> num 9999)
       (<= num 99999)))

(defun valid-tin-p (tin)
  "Check the taxation identification number (ΑΦΜ)"
  (flet ((char-parse-integer (d)
           (- (char-int d) (char-int #\0))))
    (let* ((len (length tin))
           (digits (map 'vector #'char-parse-integer
                        (nreverse (subseq tin 0 (1- len)))))
           (control-digit (char-parse-integer (elt tin (1- len)))))
      (let ((sum (iter (for d in-vector (subseq digits 0 (1- len)))
                       (for i from 1)
                       (reducing (* d (expt 2 i))
                                 by #'+))))
        (and (= len 9)
             (= (mod (mod sum 11)
                     10)
                control-digit))))))

(defun chk-date (date)
  (if (eql :null date)
      :date-null
      nil))

(defun chk-amount (float)
  (cond ((eql float :null)
         :empty-amount)
        ((non-positive-real-p float)
         :non-positive-amount)
        ((> float 9999999)
         :amount-overflow)
        (t
         nil)))

(defun chk-amount* (float)
  "Same as chk-amount but allow null values or zeros"
  (cond ((eql float :null)
         nil)
        ((non-positive-real-p float)
         :non-positive-amount)
        ((> float 9999999)
         :amount-overflow)
        (t
         nil)))
