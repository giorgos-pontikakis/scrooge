(in-package :scrooge)



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

(defun jquery-ui ()
  (js (url 'lib "jquery-ui-1.8.7.custom/js/jquery-ui-1.8.7.custom.min.js")))

(defun js-autocomplete ()
  (js (url 'js "main.js")))

(defun 960gs ()
  (css (url 'css "reset.css"))
  (css (url 'lib "960gs/code/css/960.css")))

(defun error-headers ()
  (css (url 'css "global.css")))

(defun css-autocomplete ()
  (css (url 'lib "jquery-ui-1.8.7.custom/css/custom-theme/jquery-ui-1.8.7.custom.css")))

(defun global-headers ()
  (960gs)
  (css (url 'css "global.css")))

(defun config-headers ()
  (global-headers)
  (jquery))

(defun company-headers ()
  (global-headers)
  (jquery)
  (jquery-ui)
  (js-autocomplete)
  (css-autocomplete))

(defun clear ()
  (with-html
    (:div :class "clear")))



;;; ------------------------------------------------------------
;;; Global User Interface Elements
;;; ------------------------------------------------------------

(defun ok-button (&optional label)
  (with-html
    (:button :class "ok-button"
             :type "submit" (img "tick.png") (str label))))

(defun cancel-button (href &optional label)
  (with-html
    (:a :class "cancel-button"
        :href href (str label))))

(defun logo ()
  (with-html
    (:div :id "logo"
          :class "grid_2"
          (:h1 "(scrooge) "))))

(defun header (active-item)
  (with-html
    (:div :id "header"
          (logo)
          (primary-navbar active-item)
          (logout-menu)
          (clear))))

(defun footer ()
  (with-html
    (:div :id "footer" :class "grid_12 footer"
          (:p "Powered by lisp"))))

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

(defun notifications (messages params)
  (with-html
    (:div :id "notifications"
          #|(:p :class "title" "Μηνύματα")|#
          (display (make-instance 'messenger
                                  :messages messages
                                  :style "msg-error")
                   :params params))))



;;; ------------------------------------------------------------
;;; CRUD actions menu
;;; ------------------------------------------------------------

(defun standard-actions-spec (catalogue create update delete)
  `((catalogue ,catalogue "Κατάλογος")
    (create    ,create    "Δημιουργία")
    (update    ,update    "Επεξεργασία")
    (delete    ,delete    "Διαγραφή")))

(defun company-actions-spec (catalogue details create update archive delete)
  `((catalogue ,catalogue "Κατάλογος")
    (details   ,details   "Λεπτομέρειες")
    (create    ,create    "Δημιουργία")
    (update    ,update    "Επεξεργασία")
    (archive   ,archive   "Αρχειοθέτηση")
    (delete    ,delete    "Διαγραφή")))
