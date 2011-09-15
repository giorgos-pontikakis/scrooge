(in-package :scrooge)


;;; UI elements

(defun financial-navbar (active)
  (navbar `((tx ,(tx)          "Συναλλαγές")
            (cash        ,(cash "revenue")       "Μετρητά")
            (invoice     ,(invoice "receivable") "Τιμολόγια")
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
