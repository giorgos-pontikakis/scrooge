(in-package :scrooge)


;;; UI elements

(defun financial-navbar (active-page-name)
  (navbar `((transaction ,(transaction)          "Συναλλαγές")
            (invoice     ,(invoice "receivable") "Τιμολόγια")
            (cash        ,(cash "revenue")       "Μετρητά")
            (cheque      ,(cheque "receivable")  "Επιταγές")
            (account     ,(account/overview)     "Λογαριασμοί"))
          :css-class "section-navbar hnavbar grid_12"
          :active-page-name active-page-name))



;;; main page

(define-dynamic-page financial ("financial/") ()
  (with-auth ("configuration")
    (no-cache)
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


(defun make-account-radio-tree (revenues-p)
  (make-instance 'account-radio-tree
                 :op :read
                 :root-key (if revenues-p
                               *revenues-root-account*
                               *expenses-root-account*)
                 :filter (list :debit-p (not revenues-p))))
