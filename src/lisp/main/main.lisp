(in-package :scrooge)


;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun main-navbar (&optional active)
  (with-html
    (:div :class "grid_12"
      (:div :class "section-navbar"
        (navbar `((home    ,(home)
                           "Αρχική")
                  (company ,(company)
                           "Εταιρίες")
                  (project ,(project :cstate "ongoing")
                           "Έργα")
                  (cash    ,(cash "customer")
                           "Μετρητά")
                  (cheque  ,(cheque "customer" :cstate "pending")
                           "Επιταγές")
                  (invoice ,(invoice "customer" "debit")
                           "Χρεωπιστώσεις")
                  (account ,(account)
                           "Λογαριασμοί")
                  (tx      ,(tx)
                           "Ημερολόγιο"))
                :css-class "hnavbar"
                :active active)))))



;;; ----------------------------------------------------------------------
;;; Widgets
;;; ----------------------------------------------------------------------

;;; radio-account-tree

(defclass radio-account-tree (account-tree)
  ((op       :accessor op       :initform :catalogue)
   (disabled :accessor disabled :initarg :disabled))
  (:default-initargs :item-class 'radio-account-node :disabled nil))

(defclass radio-account-node (account-node)
  ())

(defmethod selector ((node radio-account-node) selected-p)
  (make-instance 'input-radio
                 :disabled (disabled (collection node))
                 :name 'account-id
                 :value (key node)
                 :body nil
                 :checked selected-p))

(defmethod payload ((node radio-account-node) enabled-p)
  (html ()
    (str (lisp->html (getf (record node) :title)))))

(defmethod controls ((node radio-account-node) controls-p)
  (declare (ignore controls-p))
  (list nil nil))
