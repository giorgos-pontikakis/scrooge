(in-package :scrooge)


(define-existence-predicate cheque-stran-exists-p cheque-stran id)
(define-existence-predicate cheque-status-exists-p cheque-status id)

(defun chk-cheque-stran-id (id)
  (if (cheque-stran-exists-p id)
      nil
      :cheque-stran-id-unknown))

(defun chk-cheque-status (status)
  (if (cheque-status-exists-p status)
      nil
      :cheque-status-invalid))



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
    (break)
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
     (debit-account  string  chk-acc-title)
     (credit-account string  chk-acc-title)
     (from-status    string  chk-cheque-status)
     (to-status      string  chk-cheque-status))
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

(defun cheque-stran-menu (id cheque-kind &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "cheque-stran-actions"
                          :style "hnavbar actions grid_12 alpha"
                          :spec (crud-actions-spec (config/cheque-stran cheque-kind :id id)
                                                   (config/cheque-stran/create cheque-kind)
                                                   (config/cheque-stran/update cheque-kind :id id)
                                                   (config/cheque-stran/delete cheque-kind :id id)))
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
  ((header-labels :initform '("" "<br />Περιγραφή"
                              "Αρχική<br />Κατάσταση" "Τελική<br />Κατάσταση"
                              "Λογαριασμός<br /> Χρέωσης" "Λογαριασμός<br /> Πίστωσης"))
   (paginator :initform nil))
  (:default-initargs :item-class 'cheque-stran-row))

(defmethod read-records ((table cheque-stran-table))
  (with-db ()
    (query (:order-by (:select 'cheque-stran.id 'cheque-stran.title
                               (:as 'debit-account-tbl.title 'debit-account)
                               (:as 'credit-account-tbl.title 'credit-account)
                               'from-status 'to-status
                               :from 'cheque-stran
                               :inner-join (:as 'account 'debit-account-tbl)
                               :on (:= 'debit-acc-id 'debit-account-tbl.id)
                               :inner-join (:as 'account 'credit-account-tbl)
                               :on (:= 'credit-acc-id 'credit-account-tbl.id))
                      'cheque-stran.title)
           :plists)))

;;; rows

(defclass cheque-stran-row (crud-row)
  ())

(defmethod cells ((row cheque-stran-row) &key start)
  (declare (ignore start))
  (let* ((id (key row))
         (record (record row))
         (cheque-kind (filter (collection row))))
    (list :selector (make-instance 'selector-cell
                                   :states (list
                                            :on (config/cheque-stran cheque-kind)
                                            :off (config/cheque-stran cheque-kind :id id)))
          :payload (list (make-instance 'textbox-cell
                                        :name 'title
                                        :value (getf record :title))
                         (make-instance 'dropdown-cell
                                        :name 'from-status
                                        :selected (getf record :from-status)
                                        :alist *cheque-statuses*)
                         (make-instance 'dropdown-cell
                                        :name 'to-status
                                        :selected (getf record :to-status)
                                        :alist *cheque-statuses*)
                         (make-instance 'textbox-cell
                                        :name 'debit-account
                                        :value (getf record :debit-account))
                         (make-instance 'textbox-cell
                                        :name 'credit-account
                                        :value (getf record :credit-account)))
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
                                                 :id "cheque-stran-table"
                                                 :filter cheque-kind)))
          (with-document ()
            (:head
             (:title "Καταστατικές Μεταβολές Επιταγών")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'cheque-stran)
                   #|(:div :id "sidebar" :class "sidebar grid_3"
                         "")|#
                   (:div :id "cheque-stran-window" :class "window grid_12"
                         (:div :class "title" "Κατάλογος Καταστατικών Μεταβολών Επιταγών")
                         (cheque-stran-menu (val id)
                                            cheque-kind
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
                                             :op 'create
                                             :id "cheque-stran-table"
                                             :filter cheque-kind)))
      (with-document ()
        (:head
         (:title "Καταστατικές Μεταβολές Επιταγών > Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque)
               #|(:div :id "sidebar" :class "sidebar grid_3"
                     (cheque-stran-notifications))|#
               (:div :class "window grid_12"
                     (:div :class "title" "Καταστατικές Μεταβολές Επιταγών > Δημιουργία")
                     (cheque-stran-menu nil
                                        cheque-kind
                                        '(create update delete))
                     (with-form (actions/config/cheque-stran/create cheque-kind)
                       (display cheque-stran-table
                                :selected-id nil
                                :selected-data (parameters->plist title
                                                                  debit-account
                                                                  credit-account
                                                                  from-status
                                                                  to-status))))))))))

(define-regex-page config/cheque-stran/update (("config/cheque-kind/" cheque-kind "/update")
                                               :registers (cheque-kind "(receivable|payable)"))
    ((id     integer chk-cheque-stran-id t)
     (title          string)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title)
     (from-status    string chk-cheque-status)
     (to-status      string chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((cheque-stran-table (make-instance 'cheque-stran-table
                                                 :op 'update
                                                 :id "cheque-stran-table"
                                                 :filter cheque-kind)))
          (with-document ()
            (:head
             (:title "Καταστατικές Μεταβολές Επιταγών > Επεξεργασία")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'cheque-stran)
                   #|(:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
                         (cheque-stran-notifications))|#
                   (:div :id "cheque-stran-window" :class "window grid_12"
                         (:div :class "title" "Καταστατικές Μεταβολές Επιταγών > Επεξεργασία")
                         (cheque-stran-menu (val id)
                                            cheque-kind
                                            '(create update))
                         (with-form (actions/config/cheque-stran/update cheque-kind
                                                                        :id (val* id))
                           (display cheque-stran-table
                                    :selected-id (val id)
                                    :selected-data (parameters->plist title
                                                                      debit-account
                                                                      credit-account
                                                                      from-status
                                                                      to-status))))
                   (footer)))))
        (see-other (notfound)))))

(define-regex-page config/cheque-stran/delete (("config/cheque-stran/" cheque-kind "/delete")
                                        :registers (cheque-kind "(receivable|payable)"))
    ((id integer chk-cheque-stran-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((cheque-stran-table (make-instance 'cheque-stran-table
                                                 :id "cheque-stran-table"
                                                 :filter cheque-kind
                                                 :op 'delete)))
          (with-document ()
            (:head
             (:title "Καταστατικές Μεταβολές Επιταγών > Διαγραφή")
             (config-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'config)
                   (config-navbar 'cheque-stran)
                   (:div :id "cheque-stran-window" :class "window grid_12"
                         (:div :class "title" "Καταστατικές Μεταβολές Επιταγών > Διαγραφή")
                         (cheque-stran-menu (val id)
                                            cheque-kind
                                            '(create delete))
                         (with-form (actions/config/cheque-stran/delete cheque-kind
                                                                        :id (val id))
                           (display cheque-stran-table
                                    :selected-id (val id))))
                   (footer)))))
        (see-other (notfound)))))
