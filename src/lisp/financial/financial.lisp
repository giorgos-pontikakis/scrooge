(in-package :scrooge)


;;; UI elements

(defun financial-menu (&optional active-page-name)
  (display (make-instance 'horizontal-navbar
                          :id "config-menu"
                          :style "hnavbar grid_12"
                          :spec '((transaction "Συναλλαγές")
                                  ((cheque "receivable") "Επιταγές")))
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
             (financial-menu nil)
             (:div :id "body"
                   (:p "Financial content not yet available")))))))
