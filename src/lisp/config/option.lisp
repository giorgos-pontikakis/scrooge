(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass acc-role-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id))
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

(defun chk-acc-role-id (id)
  (if (account-role-id-exists-p id)
      nil
      :account-role-id-unknown))


;;; ----------------------------------------------------------------------
;;; UI elements
;;; ----------------------------------------------------------------------

(defun account-role-menu (id &optional disabled)
  (anchor-menu `((:read   ,(config/account-role :id id)        "Προβολή")
                 (:update ,(config/account-role/update :id id) "Επεξεργασία"))
               :id "option-actions"
               :css-class "hmenu actions"
               :disabled disabled))



;;; ----------------------------------------------------------------------
;;; Account-Role table
;;; ----------------------------------------------------------------------

;;; table

(defclass account-role-table (scrooge-table)
  ((header-labels :initform '("" "Ρόλος" "Λογαριασμός"))
   (paginator     :initform nil))
  (:default-initargs :item-class 'account-role-row :id "account-role-table"))

(defmethod get-records ((table account-role-table))
  (query (:order-by (:select 'account-role.id (:as 'account.title 'account) 'description
                     :from 'account-role
                     :inner-join 'account
                     :on (:= 'account-id 'account.id))
                    'description)
         :plists))


;;; rows

(defclass account-role-row (scrooge-row/plist)
  ())

(defmethod selector ((row account-role-row) selected-p)
  (let ((id (key row)))
    (html ()
      (:a :href (if selected-p
                    (config/account-role)
                    (config/account-role :id id))
          (selector-img selected-p)))))

(defmethod controls ((row account-role-row) controls-p)
  (simple-controls row controls-p #'config/account-role))

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

(defpage acc-role-page config/account-role ("config/account-role")
    ((id      string chk-acc-role-id)
     (account string chk-acc-title))
  (with-view-page
    (let ((account-role-table (make-instance 'account-role-table :op :read)))
      (with-document ()
        (:head
         (:title "Ρυθμίσεις")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'account-role)
               (:div :id "bank-window" :class "window grid_12"
                     (:div :class "title" "Ρυθμίσεις » Γενικά")
                     (account-role-menu (val id) '(:read))
                     (display account-role-table :key (val id)))
               (footer)))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage acc-role-page config/account-role/update ("config/account-role/update")
    ((id      string chk-acc-role-id t)
     (account string chk-acc-title))
  (with-view-page
    (let ((account-role-table (make-instance 'account-role-table
                                             :op :update)))
      (with-document ()
        (:head
         (:title "Ρυθμίσεις » Επεξεργασία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'account-role)
               (:div :id "account-role-window" :class "window grid_12"
                     (:div :class "title" "Ρυθμίσεις » Επεξεργασία")
                     (account-role-menu (val id) '(:update))
                     (with-form (actions/config/account-role/update :id (val id))
                       (display account-role-table :key (val id)
                                                   :payload (params->payload)
                                                   :styles (params->styles))))
               (footer)))))))

(defpage acc-role-page actions/config/account-role/update
    ("actions/config/account-role/update" :request-type :post)
    ((id      string chk-acc-role-id t)
     (account string chk-acc-title))
  (with-controller-page (config/account-role/update)
    (execute (:update 'account-role :set
                      :account-id (account-id (val account))
                      :where (:= 'id (val id))))
    (see-other (config/account-role :id (val id) ))))
