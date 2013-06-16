(in-package :scrooge)


;;; ------------------------------------------------------------
;;; Convenience functions
;;; ------------------------------------------------------------

(defun img (href)
  (with-html
      (:img :src (url 'img href))))

(defun clear ()
  (with-html
      (:div :class "clear" "")))

(defun back (href)
  (with-html
      (:a :id "back" :href href "« Επιστροφή")))



;;; ------------------------------------------------------------
;;; HTML Head
;;; ------------------------------------------------------------

(defun favicon ()
  (with-html ()
    (:link :rel "icon" :type "image/png" :href (url 'img "money.png"))))

(defun jquery ()
  (js (url 'lib "jquery/jquery-1.5.min.js")))

(defun jquery-ui ()
  (js (url 'lib "jquery/jquery-ui-1.8.9.custom/js/jquery-ui-1.8.9.custom.min.js"))
  (js (url 'lib "jquery/jquery.ui.datepicker-el.js")))

(defun jquery-autocomplete ()
  (js (url 'js "main.js"))
  (css (url 'lib "jquery/jquery-ui-1.8.9.custom/css/custom-theme/jquery-ui-1.8.9.custom.css")))

(defun js-globals ()
  (with-html
      (:script :type "text/javascript"
               "var accounts = {}; accounts.project = " (str (account-id 'project-account)))))

(defun error-headers ()
  (favicon)
  (css (url 'css "common.css")))

(defun 960gs ()
  (css (url 'lib "960gs/code/css/960.css")))

(defun login-headers ()
  (favicon)
  (css (url 'css "reset.css"))
  (960gs)
  (css (url 'css "common.css"))
  (css (url 'css "forms.css"))
  (css (url 'css "login.css")))

(defun global-headers ()
  (favicon)
  (css (url 'css "reset.css"))
  (960gs)
  (css (url 'css "common.css"))
  (css (url 'css "forms.css"))
  (css (url 'css "navigation.css"))
  (css (url 'css "crud.css")))

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
  (css (url 'css "main.css"))
  (js-globals))

(defun advanced-headers ()
  (global-headers)
  (jquery)
  (jquery-ui)
  (jquery-autocomplete)
  (css (url 'css "advanced.css")))

(defun print-headers ()
  (main-headers)
  (css (url 'css "print.css"))
  (with-html
      (:link :media "screen" :rel "stylesheet" :href (url 'css "print.css") )))



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
      (:span :class "logo" "(scrooge)")))

(defun header (&optional active-menu)
  (with-html
      (:div :id "header"
            (header-menu active-menu)
            (system-menu)
            (:div :class "clear" ""))))

(defun header-menu (active-tag)
  (with-html
      (:div :class "grid_8"
            (logo)
            (obj 'navbar :spec `((main ,(company) "Συναλλαγές")
                                 (advanced ,(account) "Λογιστική")
                                 (config ,(config/city) "Ρυθμίσεις"))
                         :id "header-menu"
                         :css-class "hnavbar"
                         :active active-tag))))

(defun footer ()
  (with-html
      (:div :id "footer" :class "grid_12"
            (:p "Powered by lisp"))
    (:div :class "clear" "")))

(defun print-pages-footer ()
  (with-html
      (:div :id "footer" :class "grid_12"
            (:p (str (format-timestring t (now)
                                        :format '(:day "/" :month "/" :year
                                                  ", "
                                                  (:hour 2) ":" :min)
                                        :timezone +greek-zone+))))
    (:div :class "clear" "")))

(defun system-menu ()
  (with-html
      (:div :id "system-menu" :class "grid_4"
            (:ul :class "hnavbar"
                 (:li (:span (fmt "~A@~A" (session-value 'user) (machine-instance))))
                 (:li (:a :href (logout) "Έξοδος"))))))

(defun notifications (&optional (page *page*) (parameters *parameters*))
  (unless (every #'validp parameters)
    (with-html
      (:div :class "notifications"
            (obj 'messenger :messages (messages page)
                            :parameters parameters
                            :css-class "msg-error")))))



;;; ------------------------------------------------------------
;;; Misc UI elements
;;; ------------------------------------------------------------

(defun filter-area (&rest widgets)
  (with-html
      (:div :class "grid_12"
            (:div :class "filter-area"
                  (display widgets)))))

(defun secondary-filter-area (&rest widgets)
  (with-html
      (:div :class "secondary-filter-area"
            (display widgets))))

(defun top-actions-area (actions search)
  (with-html ()
    (:div :class "grid_12 top-actions"
          (:div :class "grid_8 alpha"
                (display actions))
          (:div :class "grid_4 omega"
                (display search)))))


;;; actions menu

(defun actions-menu (spec &optional disabled)
  (scrooge-menu spec
                :css-class "hmenu actions"
                :disabled disabled))

(defgeneric make-spec-line (action link))

(defmethod make-spec-line ((action symbol) (link string))
  (html ()
        (:a :href link
            :class (string-downcase action)
            (str (assoc-value *action-labels* action)))))

(defmethod make-spec-line ((action symbol) (link list))
  (destructuring-bind (href label &optional css-class) link
    (html ()
          (:a :href href
              :class (or css-class (string-downcase action))
              (str label)))))

(defmethod make-spec-line ((action symbol) (link function))
  (declare (ignore action))
  link)

(defmethod make-spec-line ((action symbol) (link null))
  (declare (ignore action link))
  nil)

(defun make-menu-spec (actions-and-links)
  (loop for key in actions-and-links by #'cddr
        for val in (cdr actions-and-links) by #'cddr
        when val
          collect (list key (make-spec-line key val))))


;;; widgets

(defun filter-navbar (spec &key active id)
  (html ()
        (:div :id id :class "filter-navbar"
              (obj 'navbar
                   :spec spec
                   :css-class "hnavbar"
                   :active active
                   :test #'string-equal))))

(defun searchbox (submit-fn cancel-fn filter &optional css-class)
  (let ((hidden (remove-from-plist filter :search))
        (term (getf filter :search)))
    (make-instance 'form
                   :action (funcall submit-fn)
                   :body (html ()
                               (:div :id "searchbox" :class "inline-form"
                                     (:p :class "search"
                                         "Αναζήτηση: "
                                         (obj 'input-text
                                              :name 'search
                                              :id "search-input"
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
    (html ()
      (:div :id "datebox" :class "inline-form filter-navbar"
            (obj 'form :action (funcall submit-fn)
                       :body (html ()
                               (:p
                                (:label "Από: "
                                        (obj 'input-text
                                             :name 'since
                                             :value (getf filter :since)
                                             :css-class "datepicker"))
                                (:label  :id "until"
                                         "Εώς: "
                                         (:input-text :name 'until
                                                      :value (getf filter :until)
                                                      :css-class "datepicker"))
                                (:button :type "submit"
                                         (img "tick.png"))
                                (:a :class "cancel"
                                    :href (apply submit-fn hidden)
                                    (img "cross.png"))))
                       :hidden hidden)))))



;;; ------------------------------------------------------------
;;; selector and controls for crud collections
;;; ------------------------------------------------------------

(defun simple-selector (row selected-p url-fn id-key)
  (let* ((id (key row))
         (table (collection row))
         (filter (filter table))
         (start (page-start (paginator table) (index row) (start-index table))))
    (html ()
      (:a :id id
          :href (if selected-p
                    (apply url-fn :start start filter)
                    (apply url-fn
                           id-key
                           id
                           filter))
          (selector-img selected-p)))))

(defun simple-controls (row enabled-p url-fn id-key)
  (let ((id (key row))
        (table (collection row)))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (apply url-fn
                                          id-key
                                          id
                                          (filter table))))
        (list nil nil))))


(defgeneric navbar (section active))
