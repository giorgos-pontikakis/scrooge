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
          (clear)
          (:div :class "grid_12"
            (:div :id "home-window" :class "window"
              (:p "Home content not yet available")))
          (footer))))))
