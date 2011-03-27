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


(defun account-tree (revenues-p)
  (make-instance 'account-radio-tree
                 :root-id (if revenues-p
                              *revenues-root-account*
                              *expenses-root-account*)
                 :filter (not revenues-p)))

;; (defun account-tree (revenues-p)
;;   (apply #'make-instance 'account-radio-tree
;;          (if revenues-p
;;              (list :filter nil :root-id *revenues-root-account*)
;;              (list :filter t   :root-id *expenses-root-account*))))