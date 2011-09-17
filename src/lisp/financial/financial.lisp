(in-package :scrooge)


;;; UI elements

(defun financial-navbar (active)
  (navbar `((tx      ,(tx)                   "Συναλλαγές")
            (cash    ,(cash "revenue")       "Μετρητά")
            (invoice ,(invoice "receivable") "Τιμολόγια")
            #|(cheque      ,(cheque "receivable")  "Επιταγές")|#
            #|(account     ,(account/overview)     "Λογαριασμοί")|#)
          :css-class "section-navbar hnavbar grid_12"
          :active active))



;;; main page

(defpage root-page financial ("financial/") ()
  (with-view-page
    (with-document ()
      (:head
       (:title "Οικονομικά")
       (global-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'financial)
             (financial-navbar nil)
             (:div :id "content"
                   (:p "Financial content not yet available")))))))


;;; account-radio tree

(defclass rev/exp-account-tree (account-tree)
  ((op :initform :read))
  (:default-initargs :item-class 'rev/exp-account-node))

(defclass rev/exp-account-node (account-node)
  ())

(defmethod selector ((node rev/exp-account-node) enabled-p)
  (make-instance 'input-radio
                 :name 'account-id
                 :value (key node)
                 :body nil))

(defmethod payload ((node rev/exp-account-node) enabled-p)
  (html ()
    (str (lisp->html (getf (record node) :title)))))

(defmethod controls ((node rev/exp-account-node) controls-p)
  (declare (ignore controls-p))
  (list nil nil))
