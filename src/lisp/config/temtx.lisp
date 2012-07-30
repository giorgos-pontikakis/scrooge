(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass temtx-family (family-mixin)
  ()
  (:default-initargs :parameter-groups '(:system (temtx-id)
                                         :payload (title debit-account credit-account
                                                   customer-p balance)
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
(define-existence-predicate* temtx-title-exists-p temtx title id)

(defun chk-temtx-id (temtx-id)
  (if (temtx-exists-p temtx-id)
      nil
      :temtx-id-unknown))

(defun chk-temtx-title (title)
  (cond ((eql title :null)
         :temtx-title-null)
        ((temtx-title-exists-p title)
         nil)
        (t
         :temtx-title-unknown)))



;;; ------------------------------------------------------------
;;; Template TX Table
;;; ------------------------------------------------------------

;;; table

(defclass temtx-table (scrooge-table)
  ((header-labels :initform '("" "<br />Περιγραφή"
                              "Λογαριασμός<br />Χρέωσης" "Λογαριασμός<br />Πίστωσης"
                              "Πελάτης;" "Εταιρική χρέωση;"))
   (paginator     :initform nil))
  (:default-initargs :item-class 'temtx-row))

(defmethod get-records ((table temtx-table))
  (query (:order-by (:select 'temtx.id 'temtx.title 'customer-p 'temtx.balance
                             (:as 'debit-account.title 'debit-account)
                             (:as 'credit-account.title 'credit-account)
                             :from 'temtx
                             :inner-join (:as 'account 'debit-account)
                             :on (:= 'debit-acc-id 'debit-account.id)
                             :inner-join (:as 'account 'credit-account)
                             :on (:= 'credit-acc-id 'credit-account.id))
                    (:desc 'customer-p) (:desc 'temtx.balance) 'temtx.title)
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
                         :name 'balance
                         :value-label-alist '(("debit"  . "Χρέωση μόνο")
                                              ("credit" . "Πίστωση μόνο")
                                              ("both"   . "Πίστωση και Χρέωση"))
                         :selected (getf record :balance)
                         :disabled disabled))))

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
    ((title          string)
     (debit-account  string  chk-account-title)
     (credit-account string  chk-account-title)
     (customer-p     boolean)
     (balance        string  chk-balance))
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
    ((title          string)
     (debit-account  string  chk-account-title)
     (credit-account string  chk-account-title)
     (customer-p     boolean)
     (balance        string  chk-balance))
  (with-controller-page (config/temtx/create)
    (let* ((debit-acc-id (account-id (val debit-account)))
           (credit-acc-id (account-id (val credit-account)))
           (new-temtx (make-instance 'temtx
                                     :title (val title)
                                     :debit-acc-id debit-acc-id
                                     :credit-acc-id credit-acc-id
                                     :customer-p (val customer-p)
                                     :balance (val balance))))
      (insert-dao new-temtx)
      (see-other (config/temtx :temtx-id (temtx-id new-temtx))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage temtx-page config/temtx/update ("config/temtx/update")
    ((temtx-id       integer chk-temtx-id                            t)
     (title          string)
     (debit-account  string  chk-account-title)
     (credit-account string  chk-account-title)
     (customer-p     boolean)
     (balance        string  chk-balance))
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
    ((temtx-id       integer chk-temtx-id                            t)
     (title          string)
     (debit-account  string  chk-account-title)
     (credit-account string  chk-account-title)
     (customer-p     boolean)
     (balance        string  chk-balance))
  (with-controller-page (config/temtx/update)
    (let ((debit-acc-id (account-id (val debit-account)))
          (credit-acc-id (account-id (val credit-account))))
      (execute (:update 'temtx :set
                        'title (val title)
                        'debit-acc-id debit-acc-id
                        'credit-acc-id credit-acc-id
                        'customer-p (val customer-p)
                        'balance (val balance)
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
