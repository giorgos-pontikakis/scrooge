(in-package :scrooge)


;;; ----------------------------------------------------------------------
;;; Option validation
;;; ----------------------------------------------------------------------

(defun chk-acc-title* (title)
  "Same as chk-acc-title but accepts :null"
  (if (not (acc-title-exists-p title))
      :account-title-unknown
      nil))

(defun chk-num-rows (num)
  (if (positive-integer-p num)
      nil
      :non-positive-num-rows))



;;; ----------------------------------------------------------------------
;;; Option - Actions
;;; ----------------------------------------------------------------------

(define-dynamic-page actions/config/update ("actions/config/update" :request-type :post)
    ((cash-account          string  chk-acc-title*)
     (revenues-root-account string  chk-acc-title*)
     (expenses-root-account string  chk-acc-title*)
     (crud-table-num-rows   integer chk-num-rows))
  (with-auth ("configuration")
    (no-cache)
    (with-db ()
      (flet ((update-option (p new-value)
               (let ((dao (get-dao 'option (string-downcase (name p)))))
                 (setf (config-value dao) new-value)
                 (save-dao dao)))
             (maybe-account-id (p)
               (if-let (value (val p))
                 (account-id value)
                 nil)))
        (if (every #'validp (parameters *page*))
            (progn
              (update-option cash-account (maybe-account-id cash-account))
              (update-option revenues-root-account (maybe-account-id revenues-root-account))
              (update-option expenses-root-account (maybe-account-id expenses-root-account))
              (update-option crud-table-num-rows (val crud-table-num-rows))
              (see-other (config)))
            (see-other (config/update)))))))



;;; ----------------------------------------------------------------------
;;; Option UI elements
;;; ----------------------------------------------------------------------

(defun option-menu (disabled-items)
  (display (make-instance 'actions-menu
                          :id "option-actions"
                          :style "hnavbar actions"
                          :spec `((catalogue ,(config)        "Προβολή")
                                  (update    ,(config/update) "Επεξεργασία")))
           :disabled-items disabled-items))

(defun option-notifications ()
  (notifications
   '((cash-account (:account-title-unknown "Άγνωστος λογαριασμός μετρητών"))
     (revenues-root-account (:account-title-unknown "Άγνωστος λογαριασμός ρίζας εσόδων"))
     (expenses-root-account (:account-title-unknown "Άγνωστος λογαριασμός ρίζας εξόδων"))
     (crud-table-num-rows (:non-positive-num-rows "Ο αριθμός των
     γραμμών των καταλόγων πρέπει να είναι θετικός ακέραιος")))))



;;; ----------------------------------------------------------------------
;;; Database interface
;;; ----------------------------------------------------------------------

(defun get-option-plist ()
  (let ((daos (with-db ()
                (select-dao 'option))))
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
                   (option-menu '(catalogue))
                   (option-form 'details :data (get-option-plist))))))))

(define-dynamic-page config/update ("config/update")
    ((cash-account          string  chk-acc-title*)
     (revenues-root-account string  chk-acc-title*)
     (expenses-root-account string  chk-acc-title*)
     (crud-table-num-rows   integer chk-num-rows))
  (declare (ignorable cash-account revenues-root-account expenses-root-account crud-table-num-rows))
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
                   (option-menu '(update))
                   (with-form (actions/config/update)
                     (option-form 'update
                                  :data (plist-union
                                         (apply #'parameters->plist (parameters *page*))
                                         (get-option-plist))
                                  :styles (apply #'parameters->styles (parameters *page*)))))
             (footer))))))

(defun option-form (op &key data styles)
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
          (:div :id "option-data-form" :class "data-form"
                (label+acc-textbox 'cash-account "Λογαριασμός Μετρητών" "ac-account")
                (label+acc-textbox 'revenues-root-account "Λογαριασμός ρίζας εσόδων" "ac-account")
                (label+acc-textbox 'expenses-root-account "Λογαριασμός ρίζας εξόδων" "ac-account")
                (label 'crud-table-num-rows "Αριθμός γραμμών στους καταλόγους")
                (textbox 'crud-table-num-rows
                         :value (getf data :crud-table-num-rows)
                         :disabledp disabledp
                         :style (getf styles :crud-table-num-rows))
                (:div :class "data-form-buttons grid_12"
                      (unless disabledp
                        (ok-button "Ανανέωση")
                        (cancel-button (config) "Άκυρο")))))))))
