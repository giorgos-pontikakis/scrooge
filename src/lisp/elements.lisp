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
  (js (url 'lib "jquery/jquery-1.5.min.js")))

(defun jquery-ui ()
  (js (url 'lib "jquery/jquery-ui-1.8.9.custom/js/jquery-ui-1.8.9.custom.min.js")))

(defun jquery-autocomplete ()
  (js (url 'js "main.js"))
  (css (url 'lib "jquery/jquery-ui-1.8.9.custom/css/custom-theme/jquery-ui-1.8.9.custom.css")))

(defun error-headers ()
  (css (url 'css "global.css")))

(defun 960gs ()
  (css (url 'lib "960gs/code/css/960.css")))

(defun login-headers ()
  (css (url 'css "reset.css"))
  (960gs)
  (css (url 'css "common.css"))
  (css (url 'css "login.css")))

(defun global-headers ()
  (css (url 'css "reset.css"))
  (960gs)
  (css (url 'css "common.css"))
  (css (url 'css "navbar.css"))
  (css (url 'css "crud.css"))
  (css (url 'css "sidebar.css")))

(defun config-headers ()
  (global-headers)
  (jquery)
  (jquery-ui)
  (jquery-autocomplete)
  (css (url 'css "config.css")))

(defun admin-headers ()
  (global-headers)
  (jquery)
  (jquery-ui)
  (jquery-autocomplete)
  (css (url 'css "admin.css")))

(defun financial-headers ()
  (global-headers)
  (jquery)
  (jquery-ui)
  (jquery-autocomplete)
  (css (url 'css "financial.css")))



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
          (:h1 "(scrooge)"))))

(defun header (active-item)
  (with-html
    (:div :id "header"
          (logo)
          (primary-navbar active-item)
          (logout-menu)
          (:div :class "clear" ""))))

(defun footer ()
  (with-html
    (:div :id "footer" :class "grid_12 footer"
          (:p "Powered by lisp"))))

(defun primary-navbar (active-page-name)
  (display (make-instance 'horizontal-navbar
                          :id "primary-navbar"
                          :style "hnavbar grid_8 prefix_1"
                          :spec `((financial ,(financial) "Οικονομικά")
                                  (admin     ,(admin)     "Διαχείριση")
                                  (config    ,(config)    "Ρυθμίσεις")))
           :active-page-name active-page-name))

(defun logout-menu ()
  (display (make-instance 'menu
                          :id "logout"
                          :style "hnavbar grid_1"
                          :spec `((logout ,(logout) "Έξοδος")))))

(defun notifications (messages)
  (unless (every #'validp (parameters *page*))
    (with-html
      (:div :id "notifications"
            (:p :class "title" "Μηνύματα")
            (display (make-instance 'messenger
                                    :messages messages
                                    :style "msg-error")
                     :params (parameters *page*))))))



;;; ------------------------------------------------------------
;;; CRUD actions menu
;;; ------------------------------------------------------------

(defun crud-actions-spec (catalogue create update delete)
  `((catalogue ,catalogue "Κατάλογος")
    (create    ,create    "Δημιουργία")
    (update    ,update    "Επεξεργασία")
    (delete    ,delete    "Διαγραφή")))

(defun crud+details-actions-spec (catalogue details create update delete)
  `((catalogue ,catalogue "Κατάλογος")
    (details   ,details   "Λεπτομέρειες")
    (create    ,create    "Δημιουργία")
    (update    ,update    "Επεξεργασία")
    (delete    ,delete    "Διαγραφή")))

(defun crud+details+archive-actions-spec (catalogue details create update archive delete)
  `((catalogue ,catalogue "Κατάλογος")
    (details   ,details   "Λεπτομέρειες")
    (create    ,create    "Δημιουργία")
    (update    ,update    "Επεξεργασία")
    (archive   ,archive   "Αρχειοθέτηση")
    (delete    ,delete    "Διαγραφή")))



;;; ------------------------------------------------------------
;;; Searchbox
;;; ------------------------------------------------------------

(defun searchbox (submit-url search)
  (with-html
    (:div :id "searchbox"
          (:form :method :get
                 :action submit-url
                 (:p :class "search"
                     (textbox 'search :value search)
                     (submit (html ()
                               (img "magnifier.png")))
                     (cancel-button submit-url))))))