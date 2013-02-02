(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass temtx-family (family-mixin)
  ()
  (:default-initargs :parameter-groups '(:system (temtx-id)
                                         :payload (title debit-account credit-account
                                                   sign propagated-p lib-p)
                                         :filter ())))

(defclass temtx-page (auth-regex-page temtx-family)
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
        "Άκυρος λογαριασμός χρέωσης: Δεν υπάρχει λογαριασμός με αυτό το όνομα."
        :temtx-basic-constraint-conflicts
        "Παραβίαση του Βασικού Περιορισμού για τα Πρότυπα Συναλλαγών."
        :temtx-referenced
        "Δεν μπορούν να αλλαχθούν οι λογαριασμοί του προτύπου γιατί
        υπάρχουν συναλλαγές που αναφέρονται στο πρότυπο αυτό."))
      (credit-account
       (:account-title-null
        "Άκυρος λογαριασμός πίστωσης: Το όνομα είναι κενό."
        :account-title-unknown
        "Άκυρος λογαριασμός πίστωσης: Δεν υπάρχει λογαριασμός με αυτό το όνομα."
        :temtx-basic-constraint-conflicts
        ""
        :temtx-referenced
        ""))))))



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(define-existence-predicate temtx-exists-p temtx id)

(defun temtx-conflict-account-ids (debit-account-id credit-account-id propagated-p id)
  (with-db ()
    (let* ((where-form (if id `(:not (:= id ,id)) t))
           (sql `(:select id
                  :from temtx
                  :where (:and (:in id (:select (:temtx-conflicts ,debit-account-id
                                                                  ,credit-account-id
                                                                  ,propagated-p)))
                               ,where-form))))
      (query (sql-compile sql) :plists))))

(defun ref-temtx-changed-accounts-p (debit-account-id credit-account-id propagated-p id)
  (with-db ()
    (let ((temtx-dao (if id (get-dao 'temtx id) nil)))
      (and temtx-dao
           (temtx-referenced-p id)
           (or (not (eql debit-account-id (debit-account-id temtx-dao)))
               (not (eql credit-account-id (credit-account-id temtx-dao)))
               (not (eql propagated-p (propagated-p temtx-dao))))))))

(defun temtx-basic-constraint-conflicts (debit-account credit-account propagated-p &optional temtx-id)
  (let ((debit-account-id (account-id debit-account))
        (credit-account-id (account-id credit-account)))
    (cond ((temtx-conflict-account-ids debit-account-id
                                       credit-account-id
                                       propagated-p
                                       temtx-id)
           :temtx-basic-constraint-conflicts)
          ((ref-temtx-changed-accounts-p debit-account-id
                                         credit-account-id
                                         propagated-p
                                         temtx-id)
           :temtx-referenced))))

(defun temtx-referenced-p (temtx-id)
  (or (referenced-by temtx-id 'cheque-stran 'temtx-id)
      (referenced-by temtx-id 'tx 'temtx-id)))

(flet ((temtx-title-existence-query (title customer-p id force-chequing-p)
         (let ((temtx-table (if force-chequing-p 'temtx-chq 'temtx)))
           (if id
               `(:select 1 :from ,temtx-table :where (:and (:= customer-p ,customer-p)
                                                           (:= title ,title)
                                                           (:not (:= id ,id))))
               `(:select 1 :from ,temtx-table :where (:and (:= title ,title)
                                                           (:= customer-p ,customer-p)))))))
  (defun temtx-title-exists-p (title customer-p &optional id)
    (with-db ()
      (query (sql-compile (temtx-title-existence-query title customer-p id nil))
             :single)))
  (defun temtx-chq-title-exists-p (title customer-p &optional id)
    (with-db ()
      (query (sql-compile (temtx-title-existence-query title customer-p id t))
             :single))))

(defun chk-temtx-id (temtx-id)
  (cond ((temtx-exists-p temtx-id)
         nil)
        ((eql temtx-id :null)
         :temtx-id-null)
        (t
         :temtx-id-unknown)))

(defun chk-temtx-id/ref (temtx-id)
  (cond ((chk-temtx-id temtx-id))
        ((temtx-referenced-p temtx-id) :temtx-referenced)))

(defun chk-temtx-title (title customer-p)
  (cond ((temtx-title-exists-p title customer-p)
         nil)
        ((eql title :null)
         :temtx-title-null)
        (t
         :temtx-title-unknown)))

(defun chk-temtx-chq-title (title customer-p)
  (cond ((temtx-chq-title-exists-p title customer-p)
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

(defun chk-sign (integer)
  (if (member integer '(-1 0 1 :null))
      nil
      :unknown-temtx-sign))

;; (defun chk-lib-p (boolean debit-account-id credit-account-id)
;;   (if boolean
;;       (cond
;;         ;; cash
;;         ((or (and (eql debit-account-id (account-id 'cash-account))
;;                   (or (member credit-account-id *revenue-accounts*)
;;                       (member credit-account-id *receivable-accounts*)))
;;              (and (eql credit-account-id (account-id 'cash-account))
;;                   (or (member debit-account-id *payable-accounts*)
;;                       (member debit-account-id *expense-accounts*))))
;;          :temtx-handled-by-cash)
;;         ;; cheques
;;         ((intersection (list (account-id 'cheque-receivable-account)
;;                              (account-id 'cheque-payable-account))
;;                        (list debit-account-id credit-account-id))
;;          :temtx-handled-by-cheques)
;;         ;; invoices
;;         ((or (and (member debit-account-id *receivable-accounts*)
;;                   (member credit-account-id *revenue-accounts*))
;;              (and (member debit-account-id *expense-accounts*)
;;                   (member credit-account-id *payable-accounts*)))
;;          :temtx-handled-by-invoices))
;;       ;;
;;       nil))



;;; ------------------------------------------------------------
;;; UI Elements
;;; ------------------------------------------------------------

(defun temtx-page-title (role op-label)
  (conc "Πρότυπα Συναλλαγών » " (if (customer-p role) "Πελάτες" "Προμηθευτές") " » " op-label))

(defun temtx-top-actions (op)
  (let ((role (first *registers*)))
    (top-actions-area
     (make-instance 'scrooge-menu
                    :spec (make-menu-spec
                           `(:create (,(family-url 'config/temtx/create)
                                      ,(conc "Νέο Πρότυπο Συναλλαγής " (if (customer-p role)
                                                                           "Πελάτη"
                                                                           "Προμηθευτή")))))
                    :css-class "hmenu"
                    :disabled (list op))
     nil)))



;;; ------------------------------------------------------------
;;; Template TX Table
;;; ------------------------------------------------------------

;;; table

(defclass temtx-table (scrooge-table)
  ((header-labels :initform '("" "<br />Περιγραφή"
                              "Λογαριασμός<br />Χρέωσης" "Λογαριασμός<br />Πίστωσης"
                              "Πρόσημο<br />Εταιρικής Συναλλαγής" "Διάδοση" "Βιβλιοθήκη" "" ""))
   (paginator :accessor paginator :initarg :paginator)
   (role :accessor role :initarg :role))
  (:default-initargs :id "temtx-table"
                     :item-class 'temtx-row
                     :paginator (make-instance 'temtx-paginator
                                               :id "temtx-paginator"
                                               :css-class "paginator")))

(defmethod get-records ((table temtx-table))
  (query (:order-by (:select 'temtx.id 'temtx.title 'temtx.customer-p
                             'temtx.sign 'temtx.propagated-p 'temtx.lib-p
                             (:as 'debit-account.title 'debit-account)
                             (:as 'credit-account.title 'credit-account)
                     :from 'temtx
                     :inner-join (:as 'account 'debit-account)
                     :on (:= 'debit-account-id 'debit-account.id)
                     :inner-join (:as 'account 'credit-account)
                     :on (:= 'credit-account-id 'credit-account.id)
                     :where (:= 'temtx.customer-p (customer-p (role table))))
                    (:desc 'customer-p) (:desc 'temtx.sign) 'temtx.title)
         :plists))

(defmethod actions ((tbl temtx-table) &key)
  (let* ((temtx-id (selected-key tbl))
         (role (role tbl))
         (hrefs (if temtx-id
                    (list :update (config/temtx/update role :temtx-id temtx-id)
                          :delete (if (temtx-referenced-p temtx-id)
                                      nil
                                      (config/temtx/delete role :temtx-id temtx-id)))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))

(defmethod filters ((tbl temtx-table))
  (let ((role (role tbl))
        (filter-spec `((customer ,(config/temtx "customer") "Πελάτες")
                       (supplier ,(config/temtx "supplier") "Προμηθευτές"))))
    (filter-area (filter-navbar filter-spec
                                :active role
                                :id "cheque-role-navbar"))))


;;; rows

(defclass temtx-row (scrooge-row/plist)
  ())

(defmethod selector ((row temtx-row) selected-p)
  (let ((role (role (collection row))))
    (simple-selector row selected-p #'(lambda (&rest args)
                                        (apply #'config/temtx role args))
                     :temtx-id)))

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
                         :name 'sign
                         :value-label-alist '((+1 . "Χρέωση")
                                              (-1 . "Πίστωση")
                                              (0  . "Πίστωση και Χρέωση")
                                              (:null . "Ούτε πίστωση, ούτε χρέωση"))
                         :selected (getf record :sign)
                         :disabled disabled)
          (make-instance 'input-checkbox
                         :name 'propagated-p
                         :value t
                         :checked (getf record :propagated-p)
                         :disabled disabled
                         :body nil)
          (make-instance 'input-checkbox
                         :name 'lib-p
                         :value t
                         :checked (getf record :lib-p)
                         :disabled disabled
                         :body nil))))

(defmethod controls ((row temtx-row) controls-p)
  (let ((role (role (collection row))))
    (simple-controls row controls-p #'(lambda (&rest args)
                                        (apply #'config/temtx role args)) :temtx-id)))


;;; paginator

(defclass temtx-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg temtx-paginator) start)
  (config/temtx (role (table pg)) :start start))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage temtx-page config/temtx (("config/temtx/" (role "(customer|supplier)")))
    ((start    integer)
     (temtx-id integer chk-temtx-id))
  (with-view-page
    (let ((temtx-table (make-instance 'temtx-table
                                      :role role
                                      :op :catalogue
                                      :selected-key (val temtx-id)
                                      :start-index (val start)
                                      :id "temtx-table")))
      (with-document ()
        (:head
          (:title (str (temtx-page-title role "Κατάλογος")))
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (config-navbar 'temtx)
            (temtx-top-actions :catalogue)
            (filters temtx-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" "Κατάλογος")
                (actions temtx-table)
                (display temtx-table)))
            (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage temtx-page config/temtx/create (("config/temtx/" (role "(customer|supplier)") "/create"))
    ((debit-account  string  chk-account-title)
     (credit-account string  chk-account-title)
     (sign           integer chk-sign)
     (propagated-p   boolean)
     (lib-p          boolean)
     (title          string))
  (validate-parameters (lambda (title)
                         (chk-temtx-title/create-update title (customer-p role)))
                       title)
  (validate-parameters #'temtx-basic-constraint-conflicts
                       debit-account credit-account propagated-p)
  (with-view-page
    (let ((temtx-table (make-instance 'temtx-table
                                      :role role
                                      :op :create)))
      (with-document ()
        (:head
          (:title (str (temtx-page-title role "Δημιουργία")))
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (config-navbar 'temtx)
            (temtx-top-actions :create)
            (filters temtx-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" "Δημιουργία")
                (actions temtx-table)
                (notifications)
                (with-form (actions/config/temtx/create role)
                  (display temtx-table :payload (params->payload)))))))))))

(defpage temtx-page actions/config/temtx/create
    (("actions/config/temtx/" (role "(customer|supplier)") "/create") :request-type :post)
    ((debit-account  string   chk-account-title)
     (credit-account string   chk-account-title)
     (sign           integer  chk-sign)
     (propagated-p   boolean)
     (lib-p          boolean)
     (title          string))
  (validate-parameters (lambda (title)
                         (chk-temtx-title/create-update title (customer-p role)))
                       title)
  (validate-parameters #'temtx-basic-constraint-conflicts
                       debit-account credit-account propagated-p)
  (with-controller-page (config/temtx/create role)
    (let* ((debit-account-id (account-id (val debit-account)))
           (credit-account-id (account-id (val credit-account)))
           (new-temtx (make-instance 'temtx
                                     :title (val title)
                                     :debit-account-id debit-account-id
                                     :credit-account-id credit-account-id
                                     :customer-p (customer-p role)
                                     :sign (val sign)
                                     :propagated-p (val propagated-p)
                                     :lib-p (val lib-p))))
      (insert-dao new-temtx)
      (see-other (config/temtx role :temtx-id (temtx-id new-temtx))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage temtx-page config/temtx/update (("config/temtx/" (role "(customer|supplier)") "/update"))
    ((temtx-id       integer chk-temtx-id      t)
     (debit-account  string  chk-account-title)
     (credit-account string  chk-account-title)
     (sign           integer chk-sign)
     (propagated-p   boolean)
     (lib-p          boolean)
     (title          string))
  (validate-parameters (lambda (title)
                         (chk-temtx-title/create-update title (customer-p role) (val temtx-id)))
                       title)
  (validate-parameters (lambda (debit-account credit-account)
                         (temtx-basic-constraint-conflicts debit-account
                                                           credit-account
                                                           (val propagated-p)
                                                           (val temtx-id)))
                       debit-account credit-account)
  (with-view-page
    (let ((temtx-table (make-instance 'temtx-table
                                      :role role
                                      :selected-key (val temtx-id)
                                      :op :update)))
      (with-document ()
        (:head
          (:title (str (temtx-page-title role "Επεξεργασία")))
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (config-navbar 'temtx)
            (temtx-top-actions :update)
            (filters temtx-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" "Επεξεργασία")
                (actions temtx-table)
                (notifications)
                (with-form (actions/config/temtx/update role :temtx-id (val temtx-id))
                  (display temtx-table :payload (params->payload)))))
            (footer)))))))

(defpage temtx-page actions/config/temtx/update
    (("actions/config/temtx/" (role "(customer|supplier)") "/update") :request-type :post)
    ((temtx-id       integer chk-temtx-id      t)
     (debit-account  string  chk-account-title)
     (credit-account string  chk-account-title)
     (sign           integer chk-sign)
     (propagated-p   boolean)
     (lib-p          boolean)
     (title          string))
  (validate-parameters (lambda (title)
                         (chk-temtx-title/create-update title (customer-p role) (val temtx-id)))
                       title)
  (validate-parameters (lambda (debit-account credit-account)
                         (temtx-basic-constraint-conflicts debit-account
                                                           credit-account
                                                           (val propagated-p)
                                                           (val temtx-id)))
                       debit-account credit-account)
  (with-controller-page (config/temtx/update role)
    (let ((debit-account-id (account-id (val debit-account)))
          (credit-account-id (account-id (val credit-account))))
      (execute (:update 'temtx :set
                        'title (val title)
                        'debit-account-id debit-account-id
                        'credit-account-id credit-account-id
                        'customer-p (customer-p role)
                        'sign (val sign)
                        'propagated-p (val propagated-p)
                        'lib-p (val lib-p)
                        :where (:= 'id (val temtx-id)))))
    (see-other (config/temtx role :temtx-id (val temtx-id)))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage temtx-page config/temtx/delete (("config/temtx/" (role "(customer|supplier)") "/delete"))
    ((temtx-id integer chk-temtx-id/ref t))
  (with-view-page
    (let ((temtx-table (make-instance 'temtx-table
                                      :role role
                                      :selected-key (val temtx-id)
                                      :op :delete)))
      (with-document ()
        (:head
          (:title (str (temtx-page-title role "Διαγραφή")))
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (config-navbar 'temtx)
            (temtx-top-actions :delete)
            (filters temtx-table)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" "Διαγραφή")
                (actions temtx-table)
                (with-form (actions/config/temtx/delete role :temtx-id (val temtx-id))
                  (display temtx-table))))
            (footer)))))))

(defpage temtx-page actions/config/temtx/delete
    (("actions/config/temtx/" (role "(customer|supplier)") "/delete") :request-type :post)
    ((temtx-id integer chk-temtx-id/ref t))
  (with-controller-page (config/temtx/delete role)
    (delete-dao (get-dao 'temtx (val temtx-id)))
    (see-other (config/temtx role))))
