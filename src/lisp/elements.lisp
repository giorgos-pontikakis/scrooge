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
  (css (url 'css "crud.css"))
  (css (url 'css "login.css")))

(defun global-headers ()
  (favicon)
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

(defun filter-area (&rest widgets)
  (with-html
    (:div :class "grid_12"
          (:div :class "filter-area"
                (display widgets)))))

(defun secondary-filter-area (&rest widgets)
  (with-html
    (:div :class "secondary-filter-area"
          (display widgets))))

(defun top-actions (actions search)
  (with-html ()
    (:div :class "grid_12 top-actions"
          (:div :class "grid_8 alpha"
                (display actions))
          (:div :class "grid_4 omega"
                (display search)))))


;;; actions menu

(defun actions-menu (spec &optional disabled)
  (let ((all (mapcar #'first spec)))
    (if (subsetp all disabled)
        (with-html
          (:div :class "hmenu actions"
                (:ul
                 (:li :class "invisible" "No available action."))))
        (funcall #'menu spec
                 :css-class "hmenu actions"
                 :disabled disabled))))

(defgeneric make-spec-line (action link))

(defmethod make-spec-line ((action symbol) (link string))
  (html ()
    (:a :href link
        :class (string-downcase action)
        (str (assoc-value *action-labels* action)))))

(defmethod make-spec-line ((action cons) (link string))
  (html ()
    (:a :href link
        :class (string-downcase (car action))
        (str (cdr action)))))

(defmethod make-spec-line ((action symbol) (link function))
  (declare (ignore action))
  link)

(defmethod make-spec-line ((action symbol) (link null))
  (declare (ignore action link))
  nil)

(defun make-menu-spec (actions-and-links)
  (iter
    (for key in actions-and-links by #'cddr)
    (for val in (cdr actions-and-links) by #'cddr)
    (when val
      (collect (list key (make-spec-line key val))))))

(defmethod make-menu-spcf ((widget family-mixin) ops &rest args)
  (iter
    (for op in ops by #'cddr)
    (let ((link (apply (action-url-fn widget op) args (filter widget))))
      (when link
        (collect (list key (make-spec-line key link)))))))


;;; widgets

(defun filter-navbar (spec &key active id)
  (html ()
    (:div :id id :class "filter-navbar"
          (navbar spec
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
    (html ()
      (:div :id "datebox" :class "inline-form filter-navbar"
            (form (funcall submit-fn)
                  (html ()
                    (:p
                     (label 'since "Από: ")
                     (input-text 'since :value (getf filter :since)
                                        :css-class "datepicker")
                     (label 'since "Εώς: " :id "since")
                     (input-text 'until :value (getf filter :until)
                                        :css-class "datepicker")
                     (:button :type "submit"
                              (img "tick.png"))
                     (:a :class "cancel"
                         :href (apply submit-fn hidden)
                         (img "cross.png"))))
                  :hidden hidden)))))



;;; label-input-text for forms

(defun label-input-text (disabled record styles)
  (html (name label &key default-value extra-styles)
    (label name label)
    (input-text name
                :value (or (getf record (make-keyword name)) default-value)
                :disabled disabled
                :css-class (conc (getf styles (make-keyword name)) " " extra-styles))))
