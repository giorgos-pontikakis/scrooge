(in-package :scrooge)



;;; GENERAL CHECKS

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
      (let ((sum (loop for d across (subseq digits 0 (1- len))
                       for i from 1
                       summing (* d (expt 2 i)))))
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
         :amount-overflow)))



;;; CHECKS FOR PROJECT/COMPANY CONSTRAINTS

(defun tx-project-constraints-chker (account-id)
  #'(lambda (project-id)
      (if (eql account-id (account-id 'project-account))
          (cond ((not (suppliedp project-id))
                 :project-id-not-supplied)
                ((eql (val project-id) :null)
                 :project-id-null)
                ((and (val project-id)
                      (not (project-id-exists-p (val project-id))))
                 :project-id-unknown))
          nil)))

(defun tx-company-constraints-chker (role &optional cash-tx-p)
  #'(lambda (company-title)
      (when (suppliedp company-title)
        (with-db ()
          (let ((constraints
                  (query (:select 'immediate-tx-only-p 'revenues-account-id 'expenses-account-id
                          :from 'company
                          :where (:= 'title (val company-title)))
                         :plist)))
            (cond ((and (not cash-tx-p)
                        (getf constraints :immediate-tx-only-p))
                   :company-immediate-tx-only)
                  ((and (eql (getf constraints :revenues-account-id) :null)
                        (customer-p role))
                   :company-supplier-only)
                  ((and (eql (getf constraints :expenses-account-id) :null)
                        (not (customer-p role)))
                   :company-customer-only)))))))
