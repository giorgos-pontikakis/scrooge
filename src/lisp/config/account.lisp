(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass account-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id parent-id debit-p))
   (payload-parameter-names
    :allocation :class
    :initform '(title chequing-p))
   (filter-parameter-names
    :allocation :class
    :initform '(debit-p))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))
   (messages
    :allocation :class
    :reader messages
    :initform '((title (:account-title-null "Το όνομα λογαριασμού είναι κενό."
                        :account-title-exists "Αυτό το όνομα λογαριασμού υπάρχει ήδη."))))))



;;; ------------------------------------------------------------
;;; Account - Validation
;;; ------------------------------------------------------------

(defun ref-subaccounts (acc-id)
  (with-db ()
    (query (:select 'id :from 'account :where (:= 'parent-id acc-id))
           :single)))

(defun ref-transactions (acc-id)
  (with-db ()
    (query (:select 'id
            :from 'tx
            :where (:or (:= 'debit-acc-id acc-id)
                        (:= 'credit-acc-id acc-id)))
           :single)))

(defun acc-referenced-p (id)
  (or (ref-subaccounts id)
      (ref-transactions id)))

(define-existence-predicate  acc-id-exists-p account id)
(define-existence-predicate* acc-title-exists-p account title id)

(defun chq-acc-title-exists-p (title)
  (with-db ()
    (query (:select 1 :from 'account
            :where (:and (:= 'title title)
                         (:= 'chequing-p t)))
           :single)))

(defun non-chq-acc-title-exists-p (title)
  (with-db ()
    (query (:select 1 :from 'account
            :where (:and (:= 'title title)
                         (:= 'chequing-p nil)))
           :single)))

(defun chk-parent-acc-id (val)
  (if (or (null val) (acc-id-exists-p val))
      nil
      :parent-acc-id-unknown))

(defun chk-acc-id (id)
  (cond ((eql :null id)
         :account-id-null)
        ((not (acc-id-exists-p id))
         :acc-id-unknown)
        (t
         nil)))

(defun chk-acc-id/ref (id)
  (if (or (chk-acc-id id)
          (acc-referenced-p id))
      :acc-referenced
      nil))

(defun chk-acc-title/create (title)
  (cond ((eql :null title) :account-title-null)
        ((acc-title-exists-p title) :account-title-exists)
        (t nil)))

(defun chk-acc-title/update (title id)
  (cond ((eql title :null) :account-title-null)
        ((acc-title-exists-p title id) :account-title-unknown)
        (t nil)))

(defun chk-acc-title (title)
  (cond ((eql title :null) :account-title-null)
        ((not (acc-title-exists-p title)) :account-title-unknown)
        (t nil)))

(defun chk-chq-acc-title (title)
  (cond ((eql title :null) :account-title-null)
        ((not (chq-acc-title-exists-p title)) :account-title-unknown)
        (t nil)))

(defun chk-non-chq-acc-title (title)
  (cond ((eql title :null) :account-title-null)
        ((not (non-chq-acc-title-exists-p title)) :account-title-unknown)
        (t nil)))

(defun chk-debitp (debitp id)
  (with-db ()
    (if (or (null id)
            (eql debitp (debit-p (get-dao 'account id))))
        nil
        :invalid-debitp-acc-id-combo)))

(defun chk-chequing-p (chequing-p id)
  (with-db ()
    (let ((dependent-tx (ref-transactions id))
          (chequing-p-changed-p (not (eql chequing-p
                                          (chequing-p (get-dao 'account id))))))
      (if (and dependent-tx chequing-p-changed-p)
          :chequing-p-cannot-change
          nil))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun account-actions (op id debitp)
  (actions-menu (make-menu-spec
                 (action-anchors/crud (config/account/create :debitp debitp :parent-id id)
                                      (config/account/update :id id)
                                      (if (or (null id)
                                              (acc-referenced-p id))
                                          nil
                                          (config/account/delete :id id))))
                (enabled-actions/crud op id)))



;;; ------------------------------------------------------------
;;; Account tree
;;; ------------------------------------------------------------

;;; tree

(defclass account-tree (scrooge-tree)
  ()
  (:default-initargs :item-class 'account-node))

(defmethod get-records ((tree account-tree))
  (query (:select 'id 'title 'parent-id
          :from 'account
          :where (:= 'debit-p (getf (filter tree) :debit-p)))
         :plists))


;;; nodes

(defclass account-node (scrooge-node/plist)
  ((record-class :allocation :class :initform 'account)))

(defmethod selector ((node account-node) selected-p)
  (let ((id (key node)))
    (html ()
      (:a :href (if selected-p
                    (config/account)
                    (config/account :id id))
          (selector-img selected-p)))))

(defmethod payload ((node account-node) enabled-p)
  (make-instance 'textbox
                 :name 'title
                 :value (getf (record node) :title)
                 :disabled (not enabled-p)))

(defmethod controls ((node account-node) controls-p)
  (let ((id (key node)))
    (if controls-p
        (html ()
          (:div (display (make-instance 'ok-button))
                (display (make-instance 'cancel-button :href (config/account :id id)))))
        (list nil nil))))



;;; ------------------------------------------------------------
;;; Account form
;;; ------------------------------------------------------------

(defclass account-form (crud-form/obj)
  ((record-class :allocation :class :initform 'account)))

(defmethod display ((form account-form) &key styles)
  (let* ((record (record form))
         (dependent-tx-p (if (slot-boundp record 'id)
                             (ref-transactions (account-id record))
                             nil)))
    (with-html
      (:div :id "config-account-data-form" :class "data-form"
            (:div :class "data-form-first"
                  (label 'title "Τίτλος")
                  (input-text 'title
                              :value (title record)
                              :css-class (getf styles :title)))
            (input-checkbox 'chequing-p t "Λογαριασμός επιταγών"
                            :css-class "inline"
                            :checked (chequing-p record)
                            :disabled dependent-tx-p
                            :readonly dependent-tx-p)
            (:div :class "data-form-buttons"
                  (ok-button :body (if (eql (op form) :update) "Ανανέωση" "Δημιουργία"))
                  (cancel-button (cancel-url form) :body "Άκυρο"))))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage account-page config/account ("config/account")
    ((id integer chk-acc-id))
  (with-view-page
    (let ((op :catalogue))
      (with-document ()
        (:head
         (:title "Λογαριασμοί")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'account)
               (iter
                 (for debit-p in (list t nil))
                 (for div-id in '("debit-accounts" "credit-accounts"))
                 (for id-debit-p = (and (suppliedp id)
                                        (eql debit-p (debit-p (get-dao 'account (val id))))))
                 (for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
                 (for account-tree = (make-instance 'account-tree
                                                    :op op
                                                    :filter `(:debit-p ,debit-p)))
                 (let ()
                   (htm
                    (:div :id div-id :class "window grid_6"
                          (:div :class "title" (str window-title))
                          (notifications)
                          (account-actions op
                                           (if id-debit-p
                                               (val id)
                                               nil)
                                           debit-p)
                          (display account-tree :key (val id) :hide-root-p t)))))
               (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage account-page config/account/create ("config/account/create")
    ((parent-id integer chk-parent-acc-id)
     (debitp    boolean (chk-debitp debitp parent-id))
     (title     string  chk-acc-title/create)
     (chequing-p   boolean))
  (with-view-page
    (let* ((op :create)
           (account-form (make-instance 'account-form
                                        :op op
                                        :record nil
                                        :cancel-url (config/account))))
      (with-document ()
        (:head
         (:title "Λογαριασμός » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'account)
               (:div :class "window grid_12"
                     (:div :class "title" "Λογαριασμός » Δημιουργία")
                     (notifications)
                     (account-actions op
                                      (val parent-id)
                                      debitp)
                     (with-form (actions/config/account/create :parent-id (val parent-id)
                                                               :debitp (val debitp))
                       (display account-form :payload (params->payload)
                                             :styles (params->styles))))
               (footer)))))))

(defpage account-page actions/config/account/create ("actions/account/create"
                                                     :request-type :post)
    ((parent-id integer chk-parent-acc-id)
     (title     string  chk-acc-title/create t)
     (debitp    boolean (chk-debitp debitp parent-id))
     (chequing-p   boolean))
  (with-controller-page (config/account/create)
    (with-db ()
      (let ((new-dao (make-instance 'account
                                    :parent-id (or (val parent-id) :null)
                                    :title (val title)
                                    :debit-p (val debitp)
                                    :chequing-p (val chequing-p))))
        (insert-dao new-dao)
        (see-other (config/account :id (account-id new-dao)))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage account-page config/account/update ("config/account/update")
    ((id      integer chk-acc-id t)
     (title   string  (chk-acc-title/update title id))
     (chequing-p boolean (chk-chequing-p chequing-p id)))
  (with-view-page
    (let* ((op :update)
           (account-form (make-instance 'account-form
                                        :op op
                                        :record (get-dao 'account (val id))
                                        :cancel-url (config/account :id (val id)))))
      (with-document ()
        (:head
         (:title "Λογαριασμός » Επεξεργασία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'account)
               (:div :class "window grid_12"
                     (:div :class "title" "Λογαριασμός » Επεξεργασία")
                     (notifications)
                     (account-actions op
                                      (val id)
                                      (debit-p (with-db ()
                                                 (get-dao 'account (val id)))))
                     (with-form (actions/config/account/update :id (val id))
                       (display account-form :payload (params->payload)
                                             :styles (params->styles))))
               (footer)))))))

(defpage account-page actions/config/account/update ("actions/config/account/update"
                                                     :request-type :post)
    ((id         integer chk-acc-id                     t)
     (title      string  (chk-acc-title/update title id)   t)
     (chequing-p boolean (chk-chequing-p chequing-p id)))
  (with-controller-page (config/account/update)
    (execute (:update 'account :set
                      :title (val title)
                      :chequing-p (val chequing-p)
                      :where (:= 'id (val id))))
    (see-other (config/account :id (val id)))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage account-page config/account/delete ("config/account/delete")
    ((id integer chk-acc-id/ref t))
  (with-view-page
    (let ((op :delete))
      (with-document ()
        (:head
         (:title "Λογαριασμός » Διαγραφή")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'account)
               (iter
                 (for debit-p in (list t nil))
                 (for id-debit-p  = (eql debit-p (debit-p (get-dao 'account (val id)))))
                 (for div-id in '("debit-accounts" "credit-accounts"))
                 (for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
                 (for account-tree = (make-instance 'account-tree
                                                    :op op
                                                    :filter `(:debit-p ,debit-p)))
                 (htm
                  (:div :id div-id :class "window grid_6"
                        (:div :class "title" (str window-title))
                        (notifications)
                        (account-actions op (val id) debit-p)
                        (with-form (actions/config/account/delete :id (val id))
                          (display account-tree :key (val id) :hide-root-p t)))))
               (footer)))))))

(defpage account-page actions/config/account/delete ("actions/config/account/delete"
                                                     :request-type :post)
    ((id integer chk-acc-id/ref t))
  (with-controller-page (config/account/delete)
    (delete-dao (get-dao 'account (val id)))
    (see-other (config/account))))
