(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass temtx-family (family-mixin)
  ()
  (:default-initargs :parameter-groups '(:system (temtx-id)
                                         :payload (title debit-account credit-account
                                                   customer-p sign propagated-p)
                                         :filter ())))

(defclass temtx-page (auth-dynamic-page temtx-family)
  ((messages
    :allocation :class
    :reader messages
    :initform
    '((title
       (:temtx-title-null
        "Η περιγραφή της Πρότυπης Συναλλαγής είναι κενή."
        :temtx-title-exists
        "Έχει ήδη οριστεί Πρότυπη Συναλλαγή με αυτή την περιγραφή."))
      (debit-account
       (:account-title-null
        "Άκυρος λογαριασμός χρέωσης: Το όνομα είναι κενό."
        :account-title-unknown
        "Άκυρος λογαριασμός χρέωσης: Δεν υπάρχει λογαριασμός με αυτό το όνομα."))
      (credit-account
       (:account-title-null
        "Άκυρος λογαριασμός πίστωσης: Το όνομα είναι κενό."
        :account-title-unknown
        "Άκυρος λογαριασμός πίστωσης: Δεν υπάρχει λογαριασμός με αυτό το όνομα."))))))

(defun temtx-top-actions (op)
  (top-actions-area
   (make-instance 'scrooge-menu
                  :spec (make-menu-spec
                         `(:create (,(family-url 'config/temtx/create) "Νέο Πρότυπο Συναλλαγής")))
                  :css-class "hmenu"
                  :disabled (list op))
   nil))



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(define-existence-predicate temtx-exists-p temtx id)

(defun temtx-title-exists-p (title customer-p &optional id)
  (with-db ()
    (if id
        (query
         (:select 1 :from 'temtx :where (:and (:= 'customer-p customer-p)
                                              (:= 'title title)
                                              (:not (:= 'id id))))
         :single)
        (query (:select 1 :from 'temtx :where (:and (:= 'title title)
                                                    (:= 'customer-p customer-p)))
               :single))))

(defun chk-temtx-id (temtx-id)
  (if (temtx-exists-p temtx-id)
      nil
      :temtx-id-unknown))

(defun chk-temtx-title (title customer-p)
  (cond ((temtx-title-exists-p title customer-p)
         nil)
        ((eql title :null)
         :temtx-title-null)
        (t
         :temtx-title-unknown)))

(defun chk-temtx-title/create-update (title customer-p &optional temtx-id)
  (cond ((eql title :null)
         :temtx-title-null)
        ((temtx-title-exists-p title customer-p temtx-id)
         :temtx-title-exists)))



;;; ------------------------------------------------------------
;;; Template TX Table
;;; ------------------------------------------------------------

;;; table

(defclass temtx-table (scrooge-table)
  ((header-labels :initform '("" "<br />Περιγραφή"
                              "Λογαριασμός<br />Χρέωσης" "Λογαριασμός<br />Πίστωσης"
                              "Κατεύθυνση" "Πρόσημο" "Διάδοση"))
   (paginator     :initform nil))
  (:default-initargs :item-class 'temtx-row))

(defmethod get-records ((table temtx-table))
  (query (:order-by (:select 'temtx.id 'temtx.title 'temtx.customer-p
                      'temtx.sign 'temtx.propagated-p
                      (:as 'debit-account.title 'debit-account)
                      (:as 'credit-account.title 'credit-account)
                      :from 'temtx
                      :inner-join (:as 'account 'debit-account)
                      :on (:= 'debit-acc-id 'debit-account.id)
                      :inner-join (:as 'account 'credit-account)
                      :on (:= 'credit-acc-id 'credit-account.id))
                    (:desc 'customer-p) (:desc 'temtx.sign) 'temtx.title)
         :plists))

(defmethod actions ((tbl temtx-table) &key)
  (let* ((temtx-id (selected-key tbl))
         (hrefs (if temtx-id
                    (list :update (config/temtx/update :temtx-id temtx-id)
                          :delete (config/temtx/delete :temtx-id temtx-id))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))


;;; rows

(defclass temtx-row (scrooge-row/plist)
  ())

(defmethod selector ((row temtx-row) selected-p)
  (simple-selector row selected-p #'config/temtx :temtx-id))

(defmethod payload ((row temtx-row) enabled-p)
  (let ((record (record row))
        (disabled (not enabled-p)))
    (list (make-instance 'textbox
                         :name 'title
                         :value (getf record :title)
                         :disabled disabled)
          (make-instance 'textbox
                         :name 'debit-account
                         :value (getf record :debit-account)
                         :css-class "ac-account"
                         :disabled disabled)
          (make-instance 'textbox
                         :name 'credit-account
                         :value (getf record :credit-account)
                         :css-class "ac-account"
                         :disabled disabled)
          (make-instance 'dropdown
                         :name 'customer-p
                         :value-label-alist '((t . "Πελάτης")
                                              (nil . "Προμηθευτής"))
                         :selected (getf record :customer-p)
                         :disabled disabled)
          (make-instance 'dropdown
                         :name 'sign
                         :value-label-alist '((+1 . "Χρέωση μόνο")
                                              (-1 . "Πίστωση μόνο")
                                              (0 . "Πίστωση και Χρέωση"))
                         :selected (getf record :sign)
                         :disabled disabled)
          (make-instance 'input-checkbox
                         :name 'propagated-p
                         :value t
                         :checked (getf record :propagated-p)
                         :disabled disabled
                         :body nil))))

(defmethod controls ((row temtx-row) controls-p)
  (simple-controls row controls-p #'config/temtx :temtx-id))


;;; paginator

(defclass temtx-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg temtx-paginator) start)
  (apply #'config/temtx :start start))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage temtx-page config/temtx ("config/temtx")
    ((start    integer)
     (temtx-id integer chk-temtx-id))
  (with-view-page
    (let ((temtx-table (make-instance 'temtx-table
                                      :op :catalogue
                                      :selected-key (val temtx-id)
                                      :id "temtx-table")))
      (with-document ()
        (:head
         (:title "Πρότυπα Συναλλαγών » Κατάλογος")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'temtx)
               (temtx-top-actions :catalogue)
               (:div :class "grid_12"
                     (:div :id "temtx-window" :class "window"
                           (:div :class "title" "Κατάλογος")
                           (actions temtx-table)
                           (display temtx-table)))
               (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage temtx-page config/temtx/create ("config/temtx/create")
    ((debit-account  string  chk-account-title)
     (credit-account string  chk-account-title)
     (customer-p     boolean)
     (sign           integer chk-sign)
     (propagated-p   boolean)
     (title          string  (chk-temtx-title/create-update title customer-p)))
  (with-view-page
    (let ((temtx-table (make-instance 'temtx-table
                                      :op :create)))
      (with-document ()
        (:head
         (:title "Πρότυπα Συναλλαγών » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'temtx)
               (temtx-top-actions :create)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" "Δημιουργία")
                           (actions temtx-table)
                           (notifications)
                           (with-form (actions/config/temtx/create)
                             (display temtx-table :payload (params->payload)))))))))))

(defpage temtx-page actions/config/temtx/create ("actions/config/temtx/create" :request-type :post)
    ((debit-account  string   chk-account-title)
     (credit-account string   chk-account-title)
     (customer-p     boolean)
     (sign           integer  chk-sign)
     (propagated-p   boolean)
     (title          string  (chk-temtx-title/create-update title customer-p)))
  (with-controller-page (config/temtx/create)
    (let* ((debit-acc-id (account-id (val debit-account)))
           (credit-acc-id (account-id (val credit-account)))
           (new-temtx (make-instance 'temtx
                                     :title (val title)
                                     :debit-acc-id debit-acc-id
                                     :credit-acc-id credit-acc-id
                                     :customer-p (val customer-p)
                                     :sign (val sign)
                                     :propagated-p (val propagated-p))))
      (insert-dao new-temtx)
      (see-other (config/temtx :temtx-id (temtx-id new-temtx))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage temtx-page config/temtx/update ("config/temtx/update")
    ((temtx-id       integer chk-temtx-id      t)
     (debit-account  string  chk-account-title)
     (credit-account string  chk-account-title)
     (customer-p     boolean)
     (sign           integer chk-sign)
     (propagated-p   boolean)
     (title          string  (chk-temtx-title/create-update title customer-p temtx-id)))
  (with-view-page
    (let ((temtx-table (make-instance 'temtx-table
                                      :selected-key (val temtx-id)
                                      :op :update)))
      (with-document ()
        (:head
         (:title "Πρότυπα Συναλλαγών » Επεξεργασία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'temtx)
               (temtx-top-actions :update)
               (:div :class "grid_12"
                     (:div :id "temtx-window" :class "window"
                           (:div :class "title" "Επεξεργασία")
                           (actions temtx-table)
                           (notifications)
                           (with-form (actions/config/temtx/update :temtx-id (val temtx-id))
                             (display temtx-table :payload (params->payload)))))
               (footer)))))))

(defpage temtx-page actions/config/temtx/update ("actions/config/temtx/update" :request-type :post)
    ((temtx-id       integer chk-temtx-id      t)
     (debit-account  string  chk-account-title)
     (credit-account string  chk-account-title)
     (customer-p     boolean)
     (sign           integer chk-sign)
     (propagated-p   boolean)
     (title          string  (chk-temtx-title/create-update title customer-p temtx-id)))
  (with-controller-page (config/temtx/update)
    (let ((debit-acc-id (account-id (val debit-account)))
          (credit-acc-id (account-id (val credit-account))))
      (execute (:update 'temtx :set
                        'title (val title)
                        'debit-acc-id debit-acc-id
                        'credit-acc-id credit-acc-id
                        'customer-p (val customer-p)
                        'sign (val sign)
                        'propagated-p (val propagated-p)
                        :where (:= 'id (val temtx-id)))))
    (see-other (config/temtx :temtx-id (val temtx-id)))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage temtx-page config/temtx/delete ("config/temtx/delete")
    ((temtx-id integer chk-temtx-id t))
  (with-view-page
    (let ((temtx-table (make-instance 'temtx-table
                                      :selected-key (val temtx-id)
                                      :op :delete)))
      (with-document ()
        (:head
         (:title "Πρότυπα Συναλλαγών » Διαγραφή")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'temtx)
               (temtx-top-actions :delete)
               (:div :class "grid_12"
                     (:div :id "temtx-window" :class "window"
                           (:div :class "title" "Διαγραφή")
                           (actions temtx-table)
                           (with-form (actions/config/temtx/delete :temtx-id (val temtx-id))
                             (display temtx-table))))
               (footer)))))))

(defpage temtx-page actions/config/temtx/delete ("actions/config/temtx/delete" :request-type :post)
    ((temtx-id integer chk-temtx-id t))
  (with-controller-page (config/temtx/delete)
    (delete-dao (get-dao 'temtx (val temtx-id)))
    (see-other (config/temtx))))
