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
        :temtx-basic-constraint-violation
        "Παραβίαση του Βασικού Περιορισμού για τα Πρότυπα Συναλλαγών."
        :temtx-referenced
        "Δεν μπορούν να αλλαχθούν οι λογαριασμοί του προτύπου γιατί
        υπάρχουν συναλλαγές που αναφέρονται στο πρότυπο αυτό."))
      (credit-account
       (:account-title-null
        "Άκυρος λογαριασμός πίστωσης: Το όνομα είναι κενό."
        :account-title-unknown
        "Άκυρος λογαριασμός πίστωσης: Δεν υπάρχει λογαριασμός με αυτό το όνομα."
        :temtx-basic-constraint-violation
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

(defun temtx-constraints-chker (&optional temtx-id)
  #'(lambda (debit-account credit-account propagated-p)
      (when (and (suppliedp debit-account)
                 (suppliedp credit-account))
        (let ((debit-account-id (account-id (val debit-account)))
              (credit-account-id (account-id (val credit-account))))
          (cond ((temtx-conflict-account-ids debit-account-id
                                             credit-account-id
                                             (val propagated-p)
                                             (val temtx-id))
                 :temtx-basic-constraint-violation)
                ((ref-temtx-changed-accounts-p debit-account-id
                                               credit-account-id
                                               (val propagated-p)
                                               (val temtx-id))
                 :temtx-referenced))))))

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

;; (defun chk-temtx-title (title customer-p)
;;   (cond ((temtx-title-exists-p title customer-p)
;;          nil)
;;         ((eql title :null)
;;          :temtx-title-null)
;;         (t
;;          :temtx-title-unknown)))

(defun temtx-chq-title-chker (customer-p)
  #'(lambda (title)
      (cond ((temtx-chq-title-exists-p title customer-p)
             nil)
            ((eql title :null)
             :temtx-title-null)
            (t
             :temtx-title-unknown))))

(defun temtx-title/create-update-chker (customer-p &optional temtx-id)
  #'(lambda (title)
      (when (suppliedp title)
        (cond ((eql (val title) :null)
               :temtx-title-null)
              ((temtx-title-exists-p (val title) customer-p (val temtx-id))
               :temtx-title-exists)))))

(defun chk-sign (integer)
  (if (member integer '(-1 0 1 :null))
      nil
      :unknown-temtx-sign))



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

(defclass temtx-table (scrooge-crud-table/plist)
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

(defmethod extra-info ((table temtx-table))
  (with-html
    (:div :class "window-footer"
      (:ul :class "notes"
        (:li "Χ: Χρέωση")
        (:li "Π: Πίστωση")
        (:li "Χ+Π: Χρέωση και Πίστωση")
        (:li "—: Ούτε Χρέωση, ούτε Πίστωση")))))


;;; rows

(defclass temtx-row (scrooge-row)
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
                         :value-label-alist '((+1 . "Χ")
                                              (-1 . "Π")
                                              (0  . "Χ+Π")
                                              (:null . "—"))
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
                (display temtx-table)
                (extra-info temtx-table)))
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
  (validate-parameters (temtx-title/create-update-chker (customer-p role))
                       title)
  (validate-parameters (temtx-constraints-chker)
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
                  (display temtx-table :payload (params->payload)))
                (extra-info temtx-table)))))))))

(defpage temtx-page actions/config/temtx/create
    (("actions/config/temtx/" (role "(customer|supplier)") "/create") :request-type :post)
    ((debit-account  string   chk-account-title)
     (credit-account string   chk-account-title)
     (sign           integer  chk-sign)
     (propagated-p   boolean)
     (lib-p          boolean)
     (title          string))
  (validate-parameters (temtx-title/create-update-chker (customer-p role))
                       title)
  (validate-parameters (temtx-constraints-chker)
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
  (validate-parameters (temtx-title/create-update-chker (customer-p role) temtx-id)
                       title)
  (validate-parameters (temtx-constraints-chker temtx-id)
                       debit-account credit-account propagated-p)
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
                  (display temtx-table :payload (params->payload)))
                (extra-info temtx-table)))
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
  (validate-parameters (temtx-title/create-update-chker (customer-p role) temtx-id)
                       title)
  (validate-parameters (temtx-constraints-chker temtx-id)
                       debit-account credit-account propagated-p)
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
                  (display temtx-table))
                (extra-info temtx-table)))
            (footer)))))))

(defpage temtx-page actions/config/temtx/delete
    (("actions/config/temtx/" (role "(customer|supplier)") "/delete") :request-type :post)
    ((temtx-id integer chk-temtx-id/ref t))
  (with-controller-page (config/temtx/delete role)
    (delete-dao (get-dao 'temtx (val temtx-id)))
    (see-other (config/temtx role))))
