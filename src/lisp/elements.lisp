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

(defun selector-img (enabled-p)
  (with-html
    (if enabled-p
        (img "bullet_red.png")
        (img "bullet_blue.png"))))

(defun ok-button (&optional label)
  (with-html
    (:button :class "ok-button"
             :type "submit" (img "tick.png")
             (str label))))

(defun cancel-button (href &optional label)
  (with-html
    (:a :class "cancel-button"
        :href href
        (str label))))

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
    (:div :id "footer" :class "grid_12"
          (:p "Powered by lisp"))))

(defun primary-navbar (active-page-name)
  (display (make-instance 'horizontal-navbar
                          :id "primary-navbar"
                          :style "hnavbar grid_6 prefix_1"
                          :spec `((financial ,(financial) "Οικονομικά")
                                  (admin     ,(admin)     "Διαχείριση")
                                  (config    ,(config)    "Ρυθμίσεις")))
           :active-page-name active-page-name))

(defun logout-menu ()
  (with-html
    (:ul :id "logout" :class "grid_3 hnavbar"
         (:li (fmt "~A@~A" (session-value 'user) (machine-instance)))
         (:li (display (make-instance 'menu
                                      :style "hnavbar"
                                      :spec `((logout ,(logout) "Έξοδος"))))))))

(defun notifications (messages)
  (unless (every #'validp (parameters *page*))
    (with-html
      (:div :id "notifications"
            (display (make-instance 'messenger
                                    :messages messages
                                    :style "msg-error"
                                    :parameters (parameters *page*)))))))



;;; ------------------------------------------------------------
;;; CRUD actions menu
;;; ------------------------------------------------------------

(defun crud-actions-spec (catalogue create update delete)
  `((:catalogue ,catalogue "Κατάλογος")
    (:create    ,create    "Δημιουργία")
    (:update    ,update    "Επεξεργασία")
    (:delete    ,delete    "Διαγραφή")))

(defun crud+details-actions-spec (catalogue create details update delete)
  `((:catalogue ,catalogue "Κατάλογος")
    (:create    ,create    "Δημιουργία")
    (:details   ,details   "Λεπτομέρειες")
    (:update    ,update    "Επεξεργασία")
    (:delete    ,delete    "Διαγραφή")))



;;; ------------------------------------------------------------
;;; Searchbox
;;; ------------------------------------------------------------

(defmacro searchbox (submit-url search)
  `(with-form ,submit-url
     (:p :class "title" "Αναζήτηση")
     (:p :class "search"
         (textbox 'search :value ,search)
         (submit (html ()
                   (img "magnifier.png")))
         (cancel-button ,submit-url))))
