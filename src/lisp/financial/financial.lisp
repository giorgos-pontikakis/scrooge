(in-package :scrooge)


;;; UI elements

(defun financial-navbar (active-page-name)
  (navbar `((transaction ,(transaction)          "Συναλλαγές")
            (account     ,(account/overview)     "Λογαριασμοί")
            (cheque      ,(cheque "receivable")  "Επιταγές")
            (cash        ,(cash "revenue")       "Μετρητά")
            (invoice     ,(invoice "receivable") "Τιμολόγια"))
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


(defun account-tree (revenues-p)
  (make-instance 'account-radio-tree
                 :root-key (if revenues-p
                               *revenues-root-account*
                               *expenses-root-account*)
                 :filter (not revenues-p)))
