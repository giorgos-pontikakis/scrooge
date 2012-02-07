(in-package :scrooge)


;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun main-navbar (&optional active)
  (with-html
    (:div :class "grid_12"
          (:div :class "section-navbar"
                (navbar `((home    ,(home)                 "Αρχική")
                          (company ,(company)              "Εταιρίες")
                          (project ,(project)              "Έργα")
                          (cash    ,(cash "revenue")       "Μετρητά")
                          (cheque  ,(cheque "receivable")  "Επιταγές")
                          (invoice ,(invoice "receivable") "Χρεωπιστώσεις")
                          (tx      ,(tx)                   "Συναλλαγές"))
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
