(in-package :scrooge)


;;; ----------------------------------------------------------------------
;;; Appconfig validation
;;; ----------------------------------------------------------------------

(defun chk-acc-title* (title)
  "Same as chk-acc-title but accepts :null"
  (if (not (acc-title-exists-p title))
      :account-title-unknown
      nil))



;;; ----------------------------------------------------------------------
;;; Appconfig - Actions
;;; ----------------------------------------------------------------------

(define-dynamic-page actions/config/update ("actions/config/update" :request-type :post)
    ((cash-account     string chk-acc-title*)
     (revenues-account string chk-acc-title*)
     (expenses-account string chk-acc-title*))
  (declare (ignorable cash-account revenues-account expenses-account))
  (with-auth ("configuration")
    (no-cache)
    (with-db ()
      (flet ((update-option (p new-value)
               (let ((dao (get-dao 'appconfig (string-downcase (name p)))))
                 (setf (config-value dao) new-value)
                 (save-dao dao)))
             (maybe-account-id (p)
               (if-let (value (val p))
                 (account-id value)
                 nil)))
        (if (every #'validp (parameters *page*))
            (progn
              (update-option cash-account     (maybe-account-id cash-account))
              (update-option revenues-account (maybe-account-id revenues-account))
              (update-option expenses-account (maybe-account-id expenses-account))
              (see-other (config)))
            (see-other (config/update)))))))



;;; ----------------------------------------------------------------------
;;; Appconfig UI elements
;;; ----------------------------------------------------------------------

(defun appconfig-menu (disabled-items)
  (display (make-instance 'actions-menu
                          :id "appconfig-actions"
                          :style "hnavbar actions"
                          :spec `((catalogue ,(config)        "Προβολή")
                                  (update    ,(config/update) "Επεξεργασία")))
           :disabled-items disabled-items))

(defun appconfig-notifications ()
  (notifications
   '((cash-account (:account-title-unknown "Άγνωστος λογαριασμός μετρητών"))
     (revenues-root-account (:account-title-unknown "Άγνωστος λογαριασμός ρίζας εσόδων"))
     (expenses-root-account (:account-title-unknown "Άγνωστος λογαριασμός ρίζας εξόδων")))))



;;; ----------------------------------------------------------------------
;;; Database interface
;;; ----------------------------------------------------------------------

(defun get-appconfig-plist ()
  (let ((daos (with-db ()
                (select-dao 'appconfig))))
    (mapcan (lambda (dao)
              (list (make-keyword (string-upcase (id dao)))
                    (parse-option-dao dao)))
            daos)))



;;; ----------------------------------------------------------------------
;;; Pages
;;; ----------------------------------------------------------------------

(define-dynamic-page config ("config/") ()
  (with-auth ("configuration")
    (no-cache)
    (with-document ()
      (:head
       (:title "Ρυθμίσεις")
       (config-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'config)
             (config-navbar 'general)
             (:div :id "bank-window" :class "window grid_10"
                   (:div :class "title" "Ρυθμίσεις » Γενικά")
                   (appconfig-menu '(catalogue))
                   (appconfig-form 'details :data (get-appconfig-plist))))))))

(define-dynamic-page config/update ("config/update")
    ((cash-account     string chk-acc-title*)
     (revenues-account string chk-acc-title*)
     (expenses-account string chk-acc-title*))
  (declare (ignorable cash-account revenues-account expenses-account))
  (with-auth ("configuration")
    (no-cache)
    (with-document ()
      (:head
       (:title "Ρυθμίσεις » Επεξεργασία")
       (config-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'config)
             (config-navbar 'general)
             (:div :id "config-window" :class "window grid_12"
                   (:div :class "title" "Ρυθμίσεις » Επεξεργασία")
                   (appconfig-menu '(update))
                   (with-form (actions/config/update)
                     (appconfig-form 'update
                                     :data (plist-union
                                            (apply #'parameters->plist (parameters *page*))
                                            (get-appconfig-plist))
                                     :styles (apply #'parameters->styles (parameters *page*)))))
             (footer))))))

(defun appconfig-form (op &key data styles)
  (with-db ()
    (let ((disabledp (eql op 'details)))
      (flet ((label+acc-textbox (name label &optional extra-styles)
               (with-html
                 (label name label)
                 (textbox name
                          :value (if-let (acc-id (getf data (make-keyword name)))
                                   (title (get-dao 'account acc-id))
                                   :null)
                          :disabledp disabledp
                          :style (conc (getf styles (make-keyword name))
                                       " " extra-styles)))))
        (with-html
          (:div :id "appconfig-data-form" :class "data-form"
                (label+acc-textbox 'cash-account "Λογαριασμός Μετρητών" "ac-account")
                (label+acc-textbox 'revenues-root-account "Λογαριασμός ρίζας εσόδων" "ac-account")
                (label+acc-textbox 'expenses-root-account "Λογαριασμός ρίζας εξόδων" "ac-account"))
          (:div :class "data-form-buttons grid_12"
                (unless disabledp
                  (ok-button "Ανανέωση")
                  (cancel-button (config) "Άκυρο"))))))))
