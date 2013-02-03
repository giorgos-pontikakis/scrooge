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
  (css (url 'css "main.css")))

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
      (navbar `((main ,(company) "Συναλλαγές")
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
                  (label 'until "Εώς: " :id "until")
                  (input-text 'until :value (getf filter :until)
                                     :css-class "datepicker")
                  (:button :type "submit"
                    (img "tick.png"))
                  (:a :class "cancel"
                    :href (apply submit-fn hidden)
                    (img "cross.png"))))
              :hidden hidden)))))


;;; label-input-text for forms

(defun label-datum (disabled record styles)
  (html (name label &key href default-value enabled-styles disabled-styles common-styles format-fn)
    (let ((value (or (getf record (make-keyword name))
                     default-value))
          (all-styles (conc (getf styles (make-keyword name))
                            " "
                            (if disabled
                                disabled-styles
                                enabled-styles)
                            " "
                            common-styles)))
      (label name label)
      (display (make-instance 'textbox :name name
                                       :value value
                                       :href href
                                       :disabled disabled
                                       :css-class all-styles
                                       :format-fn format-fn)))))


;;; textbox-maker

(defun textbox-maker (record enabled-p)
  (lambda (arglist)
    (destructuring-bind (name &key href format-fn css-class (disabled (not enabled-p)))
        (ensure-list arglist)
      (make-instance 'textbox :name name
                              :css-class css-class
                              :value (getf record (make-keyword name))
                              :disabled disabled
                              :href href
                              :format-fn format-fn))))



;;; APP-SECTION

(defun app-section (temtx-id)
  (with-db ()
    (let* ((temtx (get-dao 'temtx temtx-id))
           (db-id (debit-account-id temtx))
           (cr-id (credit-account-id temtx))
           (cash-id (account-id 'cash-account))
           (normal-section (cond
                             ;; cash
                             ((or (and (eql db-id cash-id)
                                       (or (member cr-id *revenue-accounts*)
                                           (member cr-id *receivable-accounts*)))
                                  (and (eql cr-id cash-id)
                                       (or (member db-id *payable-accounts*)
                                           (member db-id *expense-accounts*))))
                              :cash)
                             ;; cheques
                             ((intersection (list (account-id 'cheque-receivable-account)
                                                  (account-id 'cheque-payable-account))
                                            (list db-id cr-id))
                              :cheque)
                             ;; invoices
                             ((or (and (member db-id *receivable-accounts*)
                                       (member cr-id *revenue-accounts*))
                                  (and (member db-id *expense-accounts*)
                                       (member cr-id *payable-accounts*))
                                  (and (member db-id *revenue-accounts*)
                                       (member cr-id *receivable-accounts*))
                                  (and (member db-id *payable-accounts*)
                                       (member cr-id *expense-accounts*)))
                              :invoice))))
      (if (lib-p temtx)
          (values :libtx normal-section)
          (values normal-section normal-section)))))

(defun app-section-label (temtx-id)
  (multiple-value-bind (active normal) (app-section temtx-id)
    (if (eql active :libtx)
        (case normal
          (:cash "Βιβλιοθήκη (αντί για Μετρητά)")
          (:cheque "Βιβλιοθήκη (αντί για Επιταγές)")
          (:invoice "Βιβλιοθήκη (αντί για Χρεωπιστώσεις)"))
        (case active
          (:cash "Μετρητά")
          (:cheque "Επιταγές")
          (:invoice "Χρεωπιστώσεις")
          ((nil) "Χωρίς αντιστοίχιση")))))
