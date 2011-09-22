(in-package :scrooge)


(defpage root-page home ("") ()
  (with-view-page
    (with-document ()
      (:head
       (:title "Αρχική")
       (global-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header)
             (main-navbar 'home)
             (:div :class "clear")
             (:div :id "home-window" :class "window grid_12"
                   (:p "Home content not yet available"))
             (footer))))))