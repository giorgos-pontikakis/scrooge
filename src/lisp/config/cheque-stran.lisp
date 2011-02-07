(in-package :scrooge)


(define-existence-predicate cheque-stran-exists-p cheque-stran id)

(defun chk-cheque-stran-id (id)
  (if (cheque-stran-exists-p id)
      nil
      :cheque-stran-id-unknown))



;;; ------------------------------------------------------------
;;; Cheque state transitions - actions
;;; ------------------------------------------------------------

(define-regex-page actions/config/cheque-stran/create
    (("actions/config/cheque-stran/" cheque-kind "/create")
     :registers (cheque-kind "(receivable|payable)")
     :request-type :post)
    ((title          string)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title)
     (from-status    string chk-cheque-status)
     (to-status      string chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (let* ((debit-acc-id (account-id (val debit-account)))
                 (credit-acc-id (account-id (val credit-account)))
                 (new-cheque-stran (make-instance 'cheque-stran
                                                  :title (val title)
                                                  :debit-acc-id debit-acc-id
                                                  :credit-acc-id credit-acc-id
                                                  :payable-p (string= cheque-kind "payable")
                                                  :from-status (val from-status)
                                                  :to-status (val to-status))))
            (insert-dao new-cheque-stran)
            (see-other (config/cheque-stran cheque-kind :id (id new-cheque-stran)))))
        (see-other (config/cheque-stran/create cheque-kind
                                               :title (raw title)
                                               :debit-account (raw debit-account)
                                               :credit-account (raw credit-account)
                                               :from-status (raw from-status)
                                               :to-status (raw to-status))))))

(define-regex-page actions/config/cheque-stran/update
    (("actions/config/cheque-stran/" cheque-kind "/update")
     :registers (cheque-kind "(receivable|payable)")
     :request-type :post)
  ((id             integer chk-cheque-stran-id t)
   (title          string)
   (debit-account  string chk-acc-title)
   (credit-account string chk-acc-title)
   (from-status    string chk-cheque-status)
   (to-status      string chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (let* ((debit-acc-id (account-id (val debit-account)))
                 (credit-acc-id (account-id (val credit-account))))
            (execute (:update 'cheque-stran :set
                              'title (val title)
                              'debit-acc-id debit-acc-id
                              'credit-acc-id credit-acc-id
                              'from-status (val from-status)
                              'to-status (val to-status)
                              :where (:= 'id (val id)))))
          (see-other (config/cheque-stran cheque-kind)))
        (see-other (config/cheque-stran/update cheque-kind
                                               :id (val id)
                                               :title (raw title)
                                               :debit-account (raw debit-account)
                                               :credit-account (raw credit-account)
                                               :from-status (raw from-status)
                                               :to-status (raw to-status))))))

(define-regex-page actions/config/cheque-stran/delete
    (("actions/config/cheque-stran/" cheque-kind "/delete")
     :registers (cheque-kind "(receivable|payable)")
     :request-type :post)
    ((id integer chk-cheque-stran-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'cheque-stran (val id)))
          (see-other (config/cheque-stran cheque-kind)))
        (see-other (notfound)))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun cheque-stran-menu (id &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "cheque-stran-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (crud-actions-spec (cheque-stran :id id)
                                                   (cheque-stran/create)
                                                   (cheque-stran/update :id id)
                                                   (cheque-stran/delete :id id)))
           :disabled-items disabled-items))

(defun cheque-stran-notifications ()
  (notifications
   '((title (:cheque-stran-title-null "Το όνομα καταστατικής μεταβολής είναι κενό."
             :cheque-stran-title-exists "Το όνομα καταστατικής μεταβολής υπάρχει ήδη.")))))



;;; ------------------------------------------------------------
;;; Cheque state transitions - table
;;; ------------------------------------------------------------

;;; table

(defclass cheque-stran-table (crud-table)
  ((header-labels :initform '("Περιγραφή"
                              "Αρχική<br />Κατάσταση" "Τελική<br />Κατάσταση"
                              "Λογαριασμός Χρέωσης" "Λογαριασμός Πίστωσης"))))

(defmethod read-records ((table cheque-stran-table))
  (with-db ()
    (query (:order-by (:select 'id 'cheque-stran.title
                               (:as 'debit-account.title 'debit-account-title)
                               (:as 'credit-account.title 'credit-account-title)
                               'from-status 'to-status
                               :from 'cheque-stran
                               :inner-join (:as 'account 'debit-account)
                               :on (:= 'debit-acc-id 'debit-account.id)
                               :inner-join (:as 'account 'credit-account)
                               :on (:= 'credit-acc-id 'credit-account.id))
                      'cheque-stran.title)
           :plists)))

;;; rows

(defclass cheque-stran-row (crud-row)
  ())

(defmethod cells ((row cheque-stran-row) &key)
  (let* ((id (key row))
         (record (record row))
         (cheque-kind (filter (table row))))
    (list :selector (make-instance 'selector-cell
                                   :states (list
                                            :on (config/cheque-stran cheque-kind)
                                            :off (config/cheque-stran cheque-kind :id id)))
          :payload (mapcar (lambda (name)
                             (make-instance 'textbox-cell
                                            :name name
                                            :value (getf record (make-keyword name))))
                           '(title from-status to-status debit-account-title credit-account-title))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (config/cheque-stran cheque-kind :id id))))))



;;; ------------------------------------------------------------
;;; Cheque state transitions - pages
;;; ------------------------------------------------------------

(define-regex-page config/cheque-stran (("config/cheque-stran/" cheque-kind)
                                        :registers (cheque-kind "(receivable|payable)"))
    ((id integer chk-cheque-stran-id))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((cheque-stran-table (make-instance 'cheque-stran-table
                                                 :op 'catalogue
                                                 :filter cheque-kind)))
          (with-document ()
            (:head
             (:title "Καταστατικές Μεταβολές Επιταγών")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'cheque-stran)
                   (:div :id "sidebar" :class "sidebar grid_3"
                         "")
                   (:div :id "cheque-stran-window" :class "window grid_9"
                         (:div :class "title" "Κατάλογος Καταστατικών Μεταβολών Επιταγών")
                         (cheque-stran-menu (val id)
                                            (if (val id)
                                                '(catalogue)
                                                '(catalogue update delete)))
                         (display cheque-stran-table
                                  :selected-id (val* id)))
                   (footer))))))))

(define-regex-page config/cheque-stran/create (("config/cheque-stran/" cheque-kind "/create")
                                               :registers (cheque-kind "(receivable|payable)"))

    ((title          string)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title)
     (from-status    string chk-cheque-status)
     (to-status      string chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
    (let ((cheque-stran-table (make-instance 'cheque-stran-table
                                             :op 'catalogue
                                             :filter cheque-kind)))
      (with-document ()
        (:head
         (:title "Καταστατικές Μεταβολές Επιταγών > Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque)
               (:div :id "sidebar" :class "sidebar grid_3"
                     (:p :class "title" "Φίλτρα")
                     (cheque-notifications))
               (:div :class "window grid_9"
                     (:div :class "title" "Καταστατικές Μεταβολές Επιταγών > Δημιουργία")
                     (cheque-stran-menu cheque-kind
                                        '(create update delete))
                     (with-form (actions/config/cheque-stran/create cheque-kind)
                       (display cheque-stran-table
                                :selected-id nil
                                :selected-data (parameters->plist title
                                                                  debit-account
                                                                  credit-account
                                                                  from-status
                                                                  to-status))))))))))
