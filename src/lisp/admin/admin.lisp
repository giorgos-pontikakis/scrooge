(in-package :scrooge)


;;; UI elements

(defun admin-menu (&optional active-page-name)
  (display (make-instance 'horizontal-navbar
                          :id "config-menu"
                          :style "hnavbar grid_12"
                          :spec '((company "Εταιρίες")
                                  (project "Έργα")))
           :active-page-name active-page-name))



;;; main page

(define-dynamic-page admin ("admin/") ()
  (with-auth ("configuration")
    (no-cache)
    (with-document ()
      (:head
       (:title "Οικονομικά")
       (global-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'admin)
             (admin-menu nil)
             (:div :id "body"
                   (:p "Admin content not yet available")))))))
