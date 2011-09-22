(in-package :scrooge)


;;; ------------------------------------------------------------
;;; Convenience functions
;;; ------------------------------------------------------------

(defun img (href)
  (with-html
    (:img :src (url 'img href))))


;;; ------------------------------------------------------------
;;; HTML Head
;;; ------------------------------------------------------------

(defun jquery ()
  (js (url 'lib "jquery/jquery-1.5.min.js")))

(defun jquery-ui ()
  (js (url 'lib "jquery/jquery-ui-1.8.9.custom/js/jquery-ui-1.8.9.custom.min.js"))
  (js (url 'lib "jquery/jquery.ui.datepicker-el.js")))

(defun jquery-autocomplete ()
  (js (url 'js "main.js"))
  (css (url 'lib "jquery/jquery-ui-1.8.9.custom/css/custom-theme/jquery-ui-1.8.9.custom.css")))

(defun error-headers ()
  (css (url 'css "common.css")))

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

(defun main-headers ()
  (global-headers)
  (jquery)
  (jquery-ui)
  (jquery-autocomplete)
  (css (url 'css "main.css")))




;;; ------------------------------------------------------------
;;; Global User Interface Elements
;;; ------------------------------------------------------------

(defun selector-img (enabled-p)
  (with-html
    (if enabled-p
        (img "bullet_red.png")
        (img "bullet_blue.png"))))

(defun logo ()
  (with-html
    (:div :id "logo"
          :class "grid_2"
          (:h1 "(scrooge)"))))

(defun header (&optional config-p)
  (with-html
    (:div :id "header"
          (logo)
          (logout-menu config-p)
          (:div :class "clear" ""))))

(defun footer ()
  (with-html
    (:div :id "footer" :class "grid_12"
          (:p "Powered by lisp"))
    (:div :class "clear" "")))

(defun logout-menu (config-p)
  (with-html
    (:div :id "logout"
        (:ul (:li (:span (fmt "~A@~A" (session-value 'user) (machine-instance)))
              (:a :class (if config-p "active" "")
                  :href (if config-p (home) (config/city)) "Ρυθμίσεις"))
             (:li (:a :href (logout) "Έξοδος"))))))

(defun notifications (&optional (page *page*) (parameters *parameters*))
  (unless (every #'validp parameters)
    (with-html
      (:div :id "notifications"
            (messenger (messages page) parameters
                       :css-class "msg-error")))))



;;; ------------------------------------------------------------
;;; CRUD actions menu
;;; ------------------------------------------------------------

(defun crud-actions-spec (read create update delete)
  `((:read   ,read   "Κατάλογος")
    (:create ,create "Δημιουργία")
    (:update ,update "Επεξεργασία")
    (:delete ,delete "Διαγραφή")))

(defun crud+details-actions-spec (read create details update delete)
  `((:read    ,read    "Κατάλογος")
    (:create  ,create  "Δημιουργία")
    (:details ,details "Λεπτομέρειες")
    (:update  ,update  "Επεξεργασία")
    (:delete  ,delete  "Διαγραφή")))



;;; ------------------------------------------------------------
;;; Searchbox
;;; ------------------------------------------------------------

(defun searchbox (submit-url search)
  (form submit-url (html ()
                     (:p :class "title" "Αναζήτηση")
                     (:p :class "search"
                         (input-text 'search :value search)
                         (submit (html ()
                                   (img "magnifier.png")))))))

(defun label-input-text (disabled record styles)
  (html (name label &optional extra-styles)
    (label name label)
    (input-text name
                :value (getf record (make-keyword name))
                :disabled disabled
                :css-class (conc (getf styles (make-keyword name))
                                 extra-styles))))