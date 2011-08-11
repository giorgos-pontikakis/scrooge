(in-package :scrooge)


;;; ----------------------------------------------------------------------
;;; Option validation
;;; ----------------------------------------------------------------------

(defun chk-acc-title* (title)
  "Same as chk-acc-title but accepts :null"
  (if (or (eql title :null) (acc-title-exists-p title))
      nil
      :account-title-unknown))

(defun chk-num-rows (num)
  (if (positive-integer-p num)
      nil
      :non-positive-num-rows))



;;; ----------------------------------------------------------------------
;;; Option - Actions
;;; ----------------------------------------------------------------------

(define-dynamic-page actions/config/update ("actions/config/update" :request-type :post)
    ((cash-account               string  chk-acc-title*)
     (revenues-root-account      string  chk-acc-title*)
     (expenses-root-account      string  chk-acc-title*)
     (invoice-receivable-account string  chk-acc-title*)
     (invoice-payable-account    string  chk-acc-title*)
     (cheque-receivable-account  string  chk-acc-title*)
     (cheque-payable-account     string  chk-acc-title*)
     (crud-table-num-rows        integer chk-num-rows))
  (declare (ignorable cash-account
                      revenues-root-account
                      expenses-root-account
                      invoice-receivable-account
                      invoice-payable-account
                      cheque-receivable-account
                      cheque-payable-account
                      crud-table-num-rows))
  (with-auth ("configuration")
    (no-cache)
    (with-db ()
      (flet ((update-option (p new-value)
               (let ((dao (get-dao 'option (string-downcase (name p)))))
                 (setf (config-value dao) new-value)
                 (save-dao dao))))
        (if (every #'validp (parameters *page*))
            (progn
              (mapc #'(lambda (p)
                        (update-option p (account-id (val p))))
                    (list cash-account
                          revenues-root-account
                          expenses-root-account
                          invoice-receivable-account
                          invoice-payable-account
                          cheque-receivable-account
                          cheque-payable-account))
              (update-option crud-table-num-rows (val crud-table-num-rows))
              (see-other (config)))
            (see-other (config/update :cash-account (raw cash-account)
                                      :revenues-root-account (raw revenues-root-account)
                                      :expenses-root-account (raw expenses-root-account)
                                      :invoice-receivable-account (raw invoice-receivable-account)
                                      :invoice-payable-account (raw invoice-payable-account)
                                      :cheque-receivable-account (raw cheque-receivable-account)
                                      :cheque-payable-account (raw cheque-payable-account) )))))))




;;; ----------------------------------------------------------------------
;;; Option UI elements
;;; ----------------------------------------------------------------------

(defun option-menu (disabled)
  (menu `((:read   ,(config)        "Προβολή")
          (:update ,(config/update) "Επεξεργασία"))
        :id "option-actions"
        :css-class "hmenu actions"
        :disabled disabled))

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

(defun option-record ()
  (with-db ()
    (mapcan (lambda (dao)
              (list (make-keyword (string-upcase (id dao)))
                    (if (search "-account" (id dao))
                        (if-let (account-id (parse-option-dao dao))
                          (title (get-dao 'account account-id))
                          :null)
                        (parse-option-dao dao))))
            (select-dao 'option))))



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
                   (option-menu '(:read))
                   (option-form :read
                                :data (option-record))))))))

(define-dynamic-page config/update ("config/update")
    ((cash-account               string  chk-acc-title*)
     (revenues-root-account      string  chk-acc-title*)
     (expenses-root-account      string  chk-acc-title*)
     (invoice-receivable-account string  chk-acc-title*)
     (invoice-payable-account    string  chk-acc-title*)
     (cheque-receivable-account  string  chk-acc-title*)
     (cheque-payable-account     string  chk-acc-title*)
     (crud-table-num-rows        integer chk-num-rows))
  (declare (ignorable cash-account
                      revenues-root-account
                      expenses-root-account
                      invoice-receivable-account
                      invoice-payable-account
                      cheque-receivable-account
                      cheque-payable-account
                      crud-table-num-rows))
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
                   (option-menu '(:update))
                   (with-form (actions/config/update)
                     (option-form :update
                                  :data (plist-union
                                         (apply #'params->plist (parameters *page*))
                                         (option-record))
                                  :styles (apply #'params->styles (parameters *page*)))))
             (footer))))))

(defun option-form (op &key data styles)
  (with-db ()
    (let ((disabled (eql op :read)))
      (flet ((label-input-text (name label &optional extra-styles)
               (label name label)
               (input-text name
                           :value (getf data (make-keyword name))
                           :disabled disabled
                           :css-class (conc (getf styles (make-keyword name))
                                            " " extra-styles))))
        (with-html
          (:div :id "option-data-form" :class "data-form"
                (label-input-text 'cash-account
                                  "Λογαριασμός Μετρητών"
                                  "ac-account")
                (label-input-text 'revenues-root-account
                                  "Λογαριασμός ρίζας εσόδων"
                                  "ac-account")
                (label-input-text 'expenses-root-account
                                  "Λογαριασμός ρίζας εξόδων"
                                  "ac-account")
                (label-input-text 'invoice-receivable-account
                                  "Λογαριασμός τιμολογημένων ποσών προς είσπραξη"
                                  "ac-account")
                (label-input-text 'invoice-payable-account
                                  "Λογαριασμός τιμολογημένων ποσών προς πληρωμή"
                                  "ac-account")
                (label-input-text 'cheque-receivable-account
                                  "Λογαριασμός επιταγών προς είσπραξη"
                                  "ac-account")
                (label-input-text 'cheque-payable-account
                                  "Λογαριασμός επιταγών προς πληρωμή"
                                  "ac-account")
                (label-input-text 'crud-table-num-rows
                                  "Αριθμός γραμμών στους καταλόγους"
                                  "ac-account")
                (:div :class "data-form-buttons grid_12"
                      (unless disabled
                        (ok-button :body "Ανανέωση")
                        (cancel-button (config) :body "Άκυρο")))))))))
