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
;;; Misc UI elements
;;; ------------------------------------------------------------

;;; actions menu

(defun action-anchors/crud (create update delete)
  `((:create ,create "Δημιουργία")
    (:update ,update "Επεξεργασία")
    (:delete ,delete "Διαγραφή")))

(defun action-anchors/crud+details (create details update delete)
  `((:create  ,create  "Δημιουργία")
    (:details ,details "Λεπτομέρειες")
    (:update  ,update  "Επεξεργασία")
    (:delete  ,delete  "Διαγραφή")))

(defun enabled-actions/crud (op id)
  (ecase op
    (:catalogue (if id
                    '(:create :update :delete)
                    '(:create)))
    (:create '())
    (:update '())
    (:delete '())))

(defun enabled-actions/crud+details (op id &rest extra-ids)
  (ecase op
    (:catalogue (if id
                    `(:create :details :delete ,@extra-ids)
                    '(:create)))
    (:details `(:update ,@extra-ids))
    (:create '())
    (:update '(:details))
    (:delete '())))

(defun enabled-actions/crud+ranks (op id)
  (ecase op
    (:catalogue (if id
                    '(:create :update :delete :rank-up :rank-down)
                    '(:create)))
    (:create '())
    (:update '())
    (:delete '())))

(defun actions-menu (spec enabled)
  (if (emptyp enabled)
      (with-html
        (:div :class "hmenu actions"
              (:ul
               (:li :class "invisible" "No available action."))))
      (let ((disabled (set-difference (mapcar #'first spec) enabled)))
        (funcall #'menu spec
                 :css-class "hmenu actions"
                 :disabled disabled))))

(defun make-menu-spec (anchor-spec)
  (mapcar (lambda (item)
            (destructuring-bind (action href label) item
              (list action
                    (html ()
                      (:a :href href
                          :class (string-downcase action)
                          (str label))))))
          anchor-spec))


;;; filters

(defun filters-navbar (spec &optional active)
  (navbar spec
          :css-class "hnavbar"
          :active active
          :test #'string-equal))

(defun searchbox (submit-fn cancel-fn filter &optional css-class)
  (let ((hidden (remove-from-plist filter :search))
        (term (getf filter :search)))
    (form (funcall submit-fn)
          (html ()
            (:div :id "searchbox"
                  (:p :class "search"
                      "Αναζήτηση: "
                      (input-text 'search :id "search-input"
                                          :value term
                                          :css-class css-class)
                      (:button :type "submit"
                               (img "magnifier.png"))
                      (:a :class "cancel"
                          :href (apply cancel-fn hidden)
                          (img "cross.png")))))
          :hidden hidden)))

(defun datebox (submit-fn filter)
  (let ((hidden (remove-from-plist filter :since :until)))
    (form (funcall submit-fn)
          (html ()
            (:div :id "datebox" :class "hnavbar"
                  (:ul
                   (:li (label 'since "Από: ")
                        (input-text 'since :value (getf filter :since)
                                           :css-class "datepicker"))
                   (:li (label 'since "Εώς: ")
                        (input-text 'until :value (getf filter :until)
                                           :css-class "datepicker"))
                   (:li (:button :type "submit"
                                 (img "tick.png")))
                   (:li (:a :class "cancel"
                            :href (apply submit-fn hidden)
                            (img "cross.png"))))))
          :hidden hidden)))



;;; label-input-text for forms

(defun label-input-text (disabled record styles)
  (html (name label &optional extra-styles)
    (label name label)
    (input-text name
                :value (getf record (make-keyword name))
                :disabled disabled
                :css-class (conc (getf styles (make-keyword name)) " " extra-styles))))
