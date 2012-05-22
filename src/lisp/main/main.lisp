(in-package :scrooge)


;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun main-navbar (&optional active)
  (with-html
    (:div :class "grid_12"
          (:div :class "section-navbar"
                (navbar `((home    ,(home)                       "Αρχική")
                          (company ,(company)                    "Εταιρίες")
                          (project ,(project)                    "Έργα")
                          (cash    ,(cash "revenue")             "Μετρητά")
                          (cheque  ,(cheque "receivable")        "Επιταγές")
                          (invoice ,(invoice "customer" "debit") "Χρεωπιστώσεις")
                          (tx      ,(tx)                         "Ημερολόγιο"))
                        :css-class "hnavbar"
                        :active active)))))



;;; ----------------------------------------------------------------------
;;; Widgets
;;; ----------------------------------------------------------------------

;;; account-radio tree

(defclass rev/exp-account-tree (account-tree)
  ((op :initform :read)
   (disabled :accessor disabled :initarg :disabled))
  (:default-initargs :item-class 'rev/exp-account-node :disabled nil))

(defclass rev/exp-account-node (account-node)
  ())

(defmethod selector ((node rev/exp-account-node) selected-p)
  (make-instance 'input-radio
                 :disabled (disabled (collection node))
                 :name 'account-id
                 :value (key node)
                 :body nil
                 :checked selected-p))

(defmethod payload ((node rev/exp-account-node) enabled-p)
  (html ()
    (str (lisp->html (getf (record node) :title)))))

(defmethod controls ((node rev/exp-account-node) controls-p)
  (declare (ignore controls-p))
  (list nil nil))


;;; ----------------------------------------------------------------------
;;; Utilities
;;; ----------------------------------------------------------------------

(defun get-cheque-records (table company-id)
  (let* ((search (getf (filter table) :search))
         (since (getf (filter table) :since))
         (until (getf (filter table) :until))
         (cstate (getf (filter table) :cstate))
         (payable-p (string= (kind table) "payable"))
         (base-query `(:select cheque.id (:as bank.title bank) serial state-id
                               due-date (:as company.title company) amount payable-p
                               :from cheque
                               :left-join bank
                               :on (:= bank.id cheque.bank-id)
                               :inner-join company
                               :on (:= company.id cheque.company-id)))
         (where nil))
    (when search
      (push `(:or (:ilike company.title ,(ilike search))
                  (:ilike bank.title ,(ilike search)))
            where))
    (when cstate
      (push `(:= ,cstate state-id)
            where))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since due-date) where))
    (when (and until (not (eql until :null)))
      (push `(:<= due-date ,until) where))
    (when company-id
      (push `(:= company.id ,company-id) where))
    (when (kind table)
      (push `(:= cheque.payable-p ,payable-p) where))
    (let ((sql `(:order-by (,@base-query :where
                                         (:and
                                          ,@where))
                           due-date)))
      (query (sql-compile sql)
             :plists))))