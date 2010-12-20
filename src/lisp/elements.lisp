(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; ------------------------------------------------------------
;;; Static references
;;; ------------------------------------------------------------

(defun css (file)
  (with-html
    (:link :href file
           :rel "stylesheet"
           :type "text/css")))

(defun js (file)
  (with-html
    (:script :type "text/javascript"
             :src file)))

(defun img (file)
  (with-html
    (:img :src (url 'img file))))



;;; ------------------------------------------------------------
;;; HTML Head
;;; ------------------------------------------------------------

(defun jquery ()
  (js (url 'lib "jquery-1.4.4/jquery-1.4.4.min.js")))


(defun 960gs ()
  (css (url 'css "reset.css"))
  (css (url 'lib "960gs/code/css/960.css")))

(defun error-headers ()
  (css (url 'css "global.css")))

(defun global-headers ()
  (960gs)
  (css (url 'css "global.css")))

(defun config-headers ()
  (global-headers)
  (jquery))

(defun clear ()
  (with-html
    (:div :class "clear")))



;;; ------------------------------------------------------------
;;; Global User Interface Elements
;;; ------------------------------------------------------------

(defun ok-button ()
  (with-html
    (:button :type "submit" (img "tick.png"))))

(defun cancel-button (href)
  (with-html
    (:a :href href (img "cancel.png"))))

(defun logo ()
  (with-html
    (:div :id "logo"
          :class "grid_2"
          (:h1 "Scrooge"))))

(defun header (active-item)
  (with-html
    (:div :id "header"
          (logo)
          (primary-navbar active-item)
          (logout-menu)
          (clear))))

(defun footer ()
  (with-html
    (:div :class "grid_12" :id "footer" "Powered by lisp")))

(defun primary-navbar (active-page-name)
  (display (make-instance 'horizontal-navbar
                          :id "navbar"
                          :style "hnavbar grid_8 prefix_1"
                          :spec '((home   "Αρχική")
                                  (main   "Οικονομικά")
                                  (admin  "Διαχείριση")
                                  (config "Ρυθμίσεις")))
           :active-page-name active-page-name))

(defun logout-menu ()
  (display (make-instance 'menu
                          :id "logout"
                          :style "hnavbar grid_1"
                          :spec `((logout "" "Έξοδος")))))

(defun main-menu (active-page-name)
  (display (make-instance 'horizontal-navbar
                          :id "main-navbar"
                          :spec '((companies "Εταιρίες")
                                  (projects "Έργα")
                                  (transactions "Συναλλαγές")))
           :active-page-name active-page-name))


(defun notifications (messages params)
  (with-html
    (:div :id "notifications"
          (:p :class "title" "Μηνύματα")
          (display (make-instance 'messenger
                                  :messages messages
                                  :style "msg-error")
                   :params params))))


;;; ------------------------------------------------------------
;;; CRUD actions menu
;;; ------------------------------------------------------------

(defun standard-actions-spec (view create update delete)
  `((view   ,view   "Προβολή")
    (create ,create "Δημιουργία")
    (update ,update "Επεξεργασία")
    (delete ,delete "Διαγραφή")))
