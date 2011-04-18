(in-package :scrooge)

;;;----------------------------------------------------------------------
;;; Generic predicate for chk- functions
;;;----------------------------------------------------------------------

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
        (= (mod (mod sum 11)
                10)
           control-digit)))))

(defun chk-amount (float)
  (if (positive-real-p float)
      nil
      :non-positive-amount))

(defun chk-amount* (float)
  "Same as chk-amount but allow null values"
  (if (or (eql float :null)
          (positive-real-p float)
          (zerop float))
      nil
      :non-positive-amount))



;;;----------------------------------------------------------------------
;;; MD5
;;;----------------------------------------------------------------------

(defun md5sum-sequence->string (str)
  (format nil "~(~{~2,'0X~}~)"
                 (map 'list #'identity (md5:md5sum-sequence str))))



;;;----------------------------------------------------------------------
;;; SQL utilities
;;;----------------------------------------------------------------------

(defun ilike (filter)
  (if (or (null filter)
          (eq filter :null))
      "%"
      (concatenate 'string "%" filter "%")))

(defmacro define-existence-predicate (name table field)
  `(defun ,name (,field)
     (with-db ()
       (query (:select 1 :from ',table :where (:= ',field ,field)) :single))))

(defmacro define-uniqueness-predicate (name table unique-field id-field)
  `(defun ,name (,unique-field &optional ,id-field)
     (with-db ()
       (not (if ,id-field
                (query (:select 1
                                :from ',table
                                :where (:and (:not (:= ',id-field ,id-field))
                                             (:= ',unique-field ,unique-field)))
                       :single)
                (query (:select 1
                                :from ',table
                                :where (:= ',unique-field ,unique-field))
                       :single))))))



;;;----------------------------------------------------------------------
;;; Miscellaneous
;;;----------------------------------------------------------------------

(defun today ()
  (universal-time-to-timestamp (get-universal-time)))


(defun parameters->plist (&rest params)
  (mapcan (lambda (param)
            (list (key param)
                  (val* param)))
          params))

(defun parameters->styles (&rest params)
  (mapcan (lambda (param)
            (list (key param)
                  (if (validp param)
                      ""
                      "attention")))
          params))

(defun parse-option-dao (option-dao)
  (flet ((parse-config-fn (lisp-type)
           (cond ((string= lisp-type "integer") #'parse-integer)
                 ((string= lisp-type "float")   #'parse-float)
                 ((string= lisp-type "date")    #'parse-date)
                 ((string= lisp-type "string")  #'identity)
                 (t (error "Unknown lisp-type in option table.")))))
    (let ((config-value (config-value option-dao)))
      (if (eql config-value :null)
          nil
          (funcall (parse-config-fn (lisp-type option-dao))
                   config-value)))))

(defun get-option (id)
  (with-db ()
    (parse-option-dao (get-dao 'option (etypecase id
                                         (symbol (string-downcase id))
                                         (string id))))))
