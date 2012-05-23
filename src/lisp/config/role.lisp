(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass account-role-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(account-role-id))
   (payload-parameter-names
    :allocation :class
    :initform '(account))
   (filter-parameter-names
    :allocation :class
    :initform '())
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))
   (messages
    :allocation :class
    :reader messages
    :initform
    '((account (:account-title-unknown "Άγνωστος λογαριασμός"))))))



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(define-existence-predicate account-role-id-exists-p account-role id)

(defun chk-account-role-id (account-role-id)
  (if (account-role-id-exists-p account-role-id)
      nil
      :account-role-id-unknown))



;;; ----------------------------------------------------------------------
;;; Account-Role table
;;; ----------------------------------------------------------------------

;;; table

(defclass account-role-table (scrooge-table)
  ((header-labels :initform '("" "Ρόλος" "Λογαριασμός"))
   (paginator     :initform nil))
  (:default-initargs :item-class 'account-role-row :id "account-role-table"))

(defmethod get-records ((table account-role-table))
  (query (:order-by (:select 'account-role.id (:as 'account.title 'account) 'description 'rank
                     :from 'account-role
                     :inner-join 'account
                     :on (:= 'account-role.account-id 'account.id))
                    'rank)
         :plists))

(defmethod actions ((tbl account-role-table) &key key)
  (let ((hrefs (if key
                   (list :update (config/account-role/update :account-role-id key))
                   nil)))
    (actions-menu (make-menu-spec hrefs)
                  (if (and key (eql (op tbl) :update))
                      '(:update)
                      '()))))

;;; rows

(defclass account-role-row (scrooge-row/plist)
  ())

(defmethod selector ((row account-role-row) selected-p)
  (let ((account-role-id (key row)))
    (html ()
      (:a :href (if selected-p
                    (config/account-role)
                    (config/account-role :account-role-id account-role-id))
          (selector-img selected-p)))))

(defmethod controls ((row account-role-row) controls-p)
  (simple-controls row controls-p #'config/account-role :account-role-id))

(defmethod payload ((row account-role-row) enabled-p)
  (list (make-instance 'textbox
                       :name 'description
                       :value (getf (record row) :description)
                       :disabled t)
        (make-instance 'textbox
                       :css-class "ac-account"
                       :name 'account
                       :value (getf (record row) :account)
                       :disabled (not enabled-p))))



;;; ----------------------------------------------------------------------
;;; VIEW
;;; ----------------------------------------------------------------------

(defpage account-role-page config/account-role ("config/account-role")
    ((account-role-id string chk-account-role-id)
     (account         string chk-account-title))
  (with-view-page
    (let* ((op :catalogue)
           (account-role-table (make-instance 'account-role-table :op op)))
      (with-document ()
        (:head
         (:title "Ρόλοι λογαριασμών » Κατάλογος")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'account-role)
               (:div :class "grid_12"
                     (:div :id "bank-window" :class "window"
                           (:div :class "title" "Κατάλογος")
                           (actions account-role-table :key (val account-role-id))
                           (display account-role-table :key (val account-role-id))))
               (footer)))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage account-role-page config/account-role/update ("config/account-role/update")
    ((account-role-id string chk-account-role-id t)
     (account         string chk-account-title))
  (with-view-page
    (let* ((op :update)
           (account-role-table (make-instance 'account-role-table
                                              :op op)))
      (with-document ()
        (:head
         (:title "Ρόλοι λογαριασμών » Επεξεργασία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'account-role)
               (:div :class "grid_12"
                     (:div :id "account-role-window" :class "window"
                           (:div :class "title" "Επεξεργασία")
                           (actions account-role-table :key (val account-role-id))
                           (with-form (actions/config/account-role/update :account-role-id (val account-role-id))
                             (display account-role-table :key (val account-role-id)
                                                         :payload (params->payload)
                                                         :styles (params->styles)))))
               (footer)))))))

(defpage account-role-page actions/config/account-role/update
    ("actions/config/account-role/update" :request-type :post)
    ((account-role-id string chk-account-role-id t)
     (account         string chk-account-title))
  (with-controller-page (config/account-role/update)
    (execute (:update 'account-role :set
                      :account-id (account-id (val account))
                      :where (:= 'id (val account-role-id))))
    (see-other (config/account-role :account-role-id (val account-role-id) ))))
