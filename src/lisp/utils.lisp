(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



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

(defun ninsert-list (n thing list)
  ;; by Kent Pitman (named insert-before-element-n-destructively 28 Oct 1992)
  (if (= n 0)
      (cons thing list) ;There's no way to be destructive in this case, so just cons.
      (let ((tail (nthcdr (1- n) list)))
        (when (null tail)
          (error "There is no position ~D in ~S." n list))
        (push thing (cdr tail))
        list)))

(defun valid-tin-p (tin)
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