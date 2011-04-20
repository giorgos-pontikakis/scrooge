(in-package :scrooge)


;;; UI elements

(defun admin-navbar (&optional active-page-name)
  (navbar `((company ,(company) "Εταιρίες")
            (project ,(project) "Έργα"))
          :style "section-navbar hnavbar grid_12"
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
             (admin-navbar nil)
             (:div :id "body"
                   (:p "Admin content not yet available")))))))
