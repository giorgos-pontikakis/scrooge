(in-package :scrooge)


;;; UI elements

(defun admin-navbar (&optional active)
  (navbar `((company ,(company) "Εταιρίες")
            (project ,(project) "Έργα"))
          :css-class "section-navbar hnavbar grid_12"
          :active active))



;;; main page

(defpage dynamic-page admin ("admin/") ()
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
             (:div :id "content"
                   (:p "Admin content not yet available")))))))
