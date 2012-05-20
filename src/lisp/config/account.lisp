(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass account-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(account-id parent-id debit-p))
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

(defun ref-subaccounts (account-id)
  (with-db ()
    (query (:select 'id :from 'account :where (:= 'parent-id account-id))
           :single)))

(defun ref-transactions (account-id)
  (with-db ()
    (query (:select 'id
            :from 'tx
            :where (:or (:= 'debit-acc-id account-id)
                        (:= 'credit-acc-id account-id)))
           :single)))

(defun acc-referenced-p (account-id)
  (or (ref-subaccounts account-id)
      (ref-transactions account-id)))

(define-existence-predicate  account-id-exists-p account id)
(define-existence-predicate* account-title-exists-p account title id)

(defun chq-account-title-exists-p (title)
  (with-db ()
    (query (:select 1 :from 'account
            :where (:and (:= 'title title)
                         (:= 'chequing-p t)))
           :single)))

(defun non-chq-account-title-exists-p (title)
  (with-db ()
    (query (:select 1 :from 'account
            :where (:and (:= 'title title)
                         (:= 'chequing-p nil)))
           :single)))

(defun chk-parent-acc-id (val)
  (if (or (null val) (account-id-exists-p val))
      nil
      :parent-acc-id-unknown))

(defun chk-account-id (account-id)
  (cond ((eql :null account-id)
         :account-id-null)
        ((not (account-id-exists-p account-id))
         :acc-id-unknown)))

(defun chk-account-id/ref (account-id)
  (cond ((chk-account-id account-id))
        ((acc-referenced-p account-id) :account-referenced)))

(defun chk-account-title/create (title)
  (cond ((eql :null title) :account-title-null)
        ((account-title-exists-p title) :account-title-exists)))

(defun chk-account-title/update (title account-id)
  (cond ((eql title :null) :account-title-null)
        ((account-title-exists-p title account-id) :account-title-unknown)))

(defun chk-account-title (title)
  (cond ((eql title :null) :account-title-null)
        ((not (account-title-exists-p title)) :account-title-unknown)
        (t nil)))

(defun chk-chq-acc-title (title)
  (cond ((eql title :null) :account-title-null)
        ((not (chq-account-title-exists-p title)) :account-title-unknown)))

(defun chk-non-chq-acc-title (title)
  (cond ((eql title :null) :account-title-null)
        ((not (non-chq-account-title-exists-p title)) :account-title-unknown)))

(defun chk-debitp (debitp account-id)
  (with-db ()
    (if (or (null account-id)
            (eql debitp (debit-p (get-dao 'account account-id))))
        nil
        :invalid-debitp-acc-id-combo)))

(defun chk-chequing-p (chequing-p account-id)
  (with-db ()
    (let ((dependent-tx (ref-transactions account-id))
          (chequing-p-changed-p (not (eql chequing-p
                                          (chequing-p (get-dao 'account account-id))))))
      (if (and dependent-tx chequing-p-changed-p)
          :chequing-p-cannot-change
          nil))))



;;; ------------------------------------------------------------
;;; Account tree
;;; ------------------------------------------------------------

;;; tree

(defclass account-tree (scrooge-tree)
  ((debit-p :accessor debit-p
            :initarg :debit-p
            :initform (error "While making an account-tree instance, debit-p slot is unbound")))
  (:default-initargs :item-class 'account-node))

(defmethod get-records ((tree account-tree))
  (query (:select 'id 'title 'parent-id
          :from 'account
          :where (:= 'debit-p (debit-p tree)))
         :plists))

(defmethod actions ((tree account-tree) &key key)
  (let* ((account-id key)
         (spec (if (and account-id
                        (eql (debit-p tree)
                             (debit-p (get-dao 'account account-id))))
                   `((:create . "Νέος Λογαριασμός") ,(config/account/create :debitp (debit-p tree)
                                                                            :parent-id account-id)
                     :update ,(config/account/update :account-id account-id)
                     :delete ,(if (chk-account-id/ref account-id)
                                  nil
                                  (config/account/delete :account-id account-id)))
                   nil)))
    (acti0ns-menu (make-menu-spcf spec)
                  (if account-id
                      '()
                      '(:create :update :delete)))))


;;; nodes

(defclass account-node (scrooge-node/plist)
  ((record-class :allocation :class :initform 'account)))

(defmethod selector ((node account-node) selected-p)
  (let ((account-id (key node)))
    (html ()
      (:a :href (if selected-p
                    (config/account)
                    (config/account :account-id account-id))
          (selector-img selected-p)))))

(defmethod payload ((node account-node) enabled-p)
  (make-instance 'textbox
                 :name 'title
                 :value (getf (record node) :title)
                 :disabled (not enabled-p)))

(defmethod controls ((node account-node) controls-p)
  (let ((account-id (key node)))
    (if controls-p
        (html ()
          (:div (display (make-instance 'ok-button))
                (display (make-instance 'cancel-button
                                        :href (config/account :account-id account-id)))))
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

(defmethod get-record ((form account-form))
  (if (key form)
      (get-dao 'account (key form))
      nil))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage account-page config/account ("config/account")
    ((account-id integer chk-account-id))
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
                 #|(for id-debit-p = (and (suppliedp account-id)
                                        (eql debit-p (debit-p (get-dao 'account (val account-id))))))|#
                 (for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
                 (for account-tree = (make-instance 'account-tree
                                                    :op op
                                                    :debit-p debit-p))
                 (let ()
                   (htm
                    (:div :class "grid_6"
                          (:div :id div-id :class "window"
                                (:div :class "title" (str window-title))
                                (notifications)
                                (actions account-tree :key (val account-id))
                                (display account-tree :key (val account-id) :hide-root-p t))))))
               (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage account-page config/account/create ("config/account/create")
    ((parent-id  integer chk-parent-acc-id)
     (debitp     boolean (chk-debitp debitp parent-id))
     (title      string  chk-account-title/create)
     (chequing-p boolean))
  (with-view-page
    (let* ((op :create)
           (account-form (make-instance 'account-form
                                        :op op
                                        :key nil
                                        :cancel-url (config/account))))
      (with-document ()
        (:head
         (:title "Λογαριασμός » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'account)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" "Λογαριασμός » Δημιουργία")
                           (notifications)
                           (actions account-form)
                           (with-form (actions/config/account/create :parent-id (val parent-id)
                                                                     :debitp (val debitp))
                             (display account-form :payload (params->payload)
                                                   :styles (params->styles)))))
               (footer)))))))

(defpage account-page actions/config/account/create ("actions/account/create"
                                                     :request-type :post)
    ((parent-id  integer chk-parent-acc-id)
     (title      string  chk-account-title/create      t)
     (debitp     boolean (chk-debitp debitp parent-id))
     (chequing-p boolean))
  (with-controller-page (config/account/create)
    (with-db ()
      (let ((new-dao (make-instance 'account
                                    :parent-id (or (val parent-id)
                                                   (and (val debitp)
                                                        (account-id *debit-accounts-root*))
                                                   (account-id *credit-accounts-root*))
                                    :title (val title)
                                    :debit-p (val debitp)
                                    :chequing-p (val chequing-p))))
        (insert-dao new-dao)
        (see-other (config/account :account-id (account-id new-dao)))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage account-page config/account/update ("config/account/update")
    ((account-id integer chk-account-id                              t)
     (title      string  (chk-account-title/update title account-id))
     (chequing-p boolean (chk-chequing-p chequing-p account-id)))
  (with-view-page
    (let* ((op :update)
           (account-form (make-instance 'account-form
                                        :op op
                                        :key (val account-id)
                                        :cancel-url (config/account :account-id (val account-id)))))
      (with-document ()
        (:head
         (:title "Λογαριασμός » Επεξεργασία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'account)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" "Λογαριασμός » Επεξεργασία")
                           (notifications)
                           (actions account-form)
                           (with-form (actions/config/account/update :account-id (val account-id))
                             (display account-form :payload (params->payload)
                                                   :styles (params->styles)))))
               (footer)))))))

(defpage account-page actions/config/account/update ("actions/config/account/update"
                                                     :request-type :post)
    ((account-id integer chk-account-id                              t)
     (title      string  (chk-account-title/update title account-id) t)
     (chequing-p boolean (chk-chequing-p chequing-p account-id)))
  (with-controller-page (config/account/update)
    (execute (:update 'account :set
                      :title (val title)
                      :chequing-p (val chequing-p)
                      :where (:= 'id (val account-id))))
    (see-other (config/account :account-id (val account-id)))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage account-page config/account/delete ("config/account/delete")
    ((account-id integer chk-account-id/ref t))
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
                 (for id-debit-p  = (eql debit-p (debit-p (get-dao 'account (val account-id)))))
                 (for div-id in '("debit-accounts" "credit-accounts"))
                 (for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
                 (for account-tree = (make-instance 'account-tree
                                                    :op op
                                                    :debit-p debit-p))
                 (htm
                  (:div :class "grid_6"
                        (:div :id div-id :class "window"
                              (:div :class "title" (str window-title))
                              (notifications)
                              (actions account-tree :key (val account-id))
                              (with-form (actions/config/account/delete :account-id (val account-id))
                                (display account-tree :key (val account-id) :hide-root-p t))))))
               (footer)))))))

(defpage account-page actions/config/account/delete ("actions/config/account/delete"
                                                     :request-type :post)
    ((account-id integer chk-account-id/ref t))
  (with-controller-page (config/account/delete)
    (delete-dao (get-dao 'account (val account-id)))
    (see-other (config/account))))
