(in-package :scrooge)


;;; UI elements

(defun admin-navbar (&optional active)
  (navbar `((company ,(company) "Εταιρίες")
            (project ,(project) "Έργα"))
          :css-class "section-navbar hnavbar grid_12"
          :active active))



;;; main page

(defpage root-page admin ("admin/") ()
  (with-view-page
    (with-document ()
      (:head
       (:title "Διαχείριση")
       (global-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'admin)
             (admin-navbar nil)
             (:div :id "content"
                   (:p "Admin content not yet available")))))))
