(in-package :scrooge)


;;; UI elements

(defun financial-navbar (active-page-name)
  (display (make-instance 'horizontal-navbar
                          :style "section-navbar hnavbar grid_12"
                          :spec `((transaction ,(transaction)          "Συναλλαγές")
                                  (account     ,(account/overview)     "Λογαριασμοί")
                                  (cheque      ,(cheque "receivable")  "Επιταγές")
                                  (cash        ,(cash "revenue")       "Μετρητά")
                                  (invoice     ,(invoice "receivable") "Τιμολόγια")))
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
             (:div :id "body"
                   (:p "Financial content not yet available")))))))
