(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass account-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id parent-id debit-p))
   (user-parameter-names
    :allocation :class
    :initform '(title chequing-p))
   (filter-parameter-names
    :allocation :class
    :initform '(debit-p))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))))



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

(defun chk-parent-acc-id (val)
  (if (or (null val) (acc-id-exists-p val))
      nil
      :parent-acc-id-unknown))

(defun chk-acc-id (id)
  (if (acc-id-exists-p id)
      nil
      :acc-id-unknown))

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

(defun account-crud-menu (id debitp &optional disabled)
  (let ((prefix (if debitp "debit" "credit")))
    (anchor-menu (crud-actions-spec (account :id id)
                                    (account/create :debitp debitp :parent-id id)
                                    (account/update :id id)
                                    (if (or (null id)
                                            (acc-referenced-p id))
                                        nil
                                        (account/delete :id id)))
                 :id (conc prefix "-account-actions")
                 :css-class "hmenu actions"
                 :disabled disabled)))

(defun config-account-data-form (op &key id data styles)
  (let ((disabled (eql op :details))
        (dependent-tx-p (if id (ref-transactions id) nil)))
    (with-html
      (:div :id "config-account-data-form" :class "data-form"
            (:div :class "data-form-first"
                  (label 'title "Τίτλος")
                  (input-text 'title
                              :value (getf data :title)
                              :disabled disabled
                              :css-class (getf styles :title)))
            (input-checkbox 'chequing-p t "Λογαριασμός επιταγών"
                            :css-class "inline"
                            :checked (getf data :chequing-p)
                            :disabled dependent-tx-p
                            :readonly dependent-tx-p)
            (:div :class "data-form-buttons"
                  (if disabled
                      (cancel-button (account :id id)
                                     :body "Επιστροφή στον Κατάλογο Λογαριασμών")
                      (progn
                        (ok-button :body (if (eql op :update) "Ανανέωση" "Δημιουργία"))
                        (cancel-button (account :id id) :body "Άκυρο"))))))))



;;; ------------------------------------------------------------
;;; Account tree
;;; ------------------------------------------------------------

;;; tree

(defclass account-tree (scrooge-tree)
  ()
  (:default-initargs :item-class 'account-node
                     :record-class 'plist))

(defmethod read-records ((tree account-tree))
  (with-db ()
    (query (:select 'id 'title 'parent-id
                    :from 'account
                    :where (:= 'debit-p (getf (filter tree) :debit-p)))
           :plists)))


;;; nodes

(defclass account-node (scrooge-node/plist)
  ())

(defmethod selector ((node account-node) selected-p)
  (let ((id (key node)))
    (html ()
      (:a :href (if selected-p
                    (account)
                    (account :id id))
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
                (display (make-instance 'cancel-button :href (account :id id)))))
        (list nil nil))))



;;; ------------------------------------------------------------
;;; Database interface
;;; ------------------------------------------------------------

(defun account-record (id)
  (with-db ()
    (query (:select 'id 'title 'chequing-p
            :from 'account
            :where (:= id 'id))
           :plist)))



;;; ------------------------------------------------------------
;;; Notifications
;;; ------------------------------------------------------------

(defun account-notifications ()
  (notifications '((title (:account-title-null "Το όνομα λογαριασμού είναι κενό."
                           :account-title-exists "Αυτό το όνομα λογαριασμού υπάρχει ήδη.")))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage account-page account ("config/account")
    ((id integer chk-acc-id))
  (with-view-page
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
               (for id-debit-p  = (and (val id)
                                       (eql debit-p (get-dao 'account (val id)))))
               (for div-id in '("debit-accounts" "credit-accounts"))
               (for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
               (for account-tree = (make-instance 'account-tree
                                                  :op :read
                                                  :filter `(:debit-p ,debit-p)))
               (htm
                (:div :id div-id :class "window grid_6"
                      (:div :class "title" (str window-title))
                      (account-crud-menu (if id-debit-p
                                             (val id)
                                             (key (root account-tree)))
                                         debit-p
                                         (if id-debit-p
                                             '(:read)
                                             '(:read :update :delete)))
                      (display account-tree :key (val id) :hide-root-p t)))))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage account-page account/create ("config/account/create")
    ((parent-id integer chk-parent-acc-id)
     (debitp    boolean (chk-debitp debitp parent-id))
     (title     string  chk-acc-title/create)
     (chequing-p   boolean))
  (with-view-page
    (with-document ()
      (:head
       (:title "Λογαριασμός » Δημιουργία")
       (config-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'config)
             (config-navbar 'account)
             (:div :class "window grid_6"
                   (:div :class "title" "Λογαριασμός » Δημιουργία")
                   (account-crud-menu (val parent-id)
                                      debitp
                                      '(:create :update :delete))
                   (with-form (actions/config/account/create :parent-id (or (val parent-id)
                                                                            )
                                                             :debitp (val debitp))
                     (config-account-data-form :create
                                               :data (params->plist parent-id
                                                                    title
                                                                    chequing-p)
                                               :styles (params->styles title)))))))))

(defpage account-page actions/config/account/create ("actions/config/account/create"
                                                     :request-type :post)
    ((parent-id integer chk-parent-acc-id)
     (title     string  chk-acc-title/create t)
     (debitp    boolean (chk-debitp debitp parent-id))
     (chequing-p   boolean))
  (with-controller-page (account/create)
    (with-db ()
      (let ((new-dao (make-instance 'account
                                    :parent-id (or (val parent-id) :null)
                                    :title (val title)
                                    :debit-p (val debitp)
                                    :chequing-p (val chequing-p))))
        (insert-dao new-dao)
        (see-other (account :id (id new-dao)))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage account-page account/update ("config/account/update")
    ((id      integer chk-acc-id t)
     (title   string  (chk-acc-title/update title id))
     (chequing-p boolean (chk-chequing-p chequing-p id)))
  (with-view-page
    (with-document ()
      (:head
       (:title "Λογαριασμός » Επεξεργασία")
       (config-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'config)
             (config-navbar 'account)
             (:div :class "window grid_6"
                   (:div :class "title" "Λογαριασμός » Επεξεργασία")
                   (account-crud-menu (val id)
                                      (debit-p (with-db ()
                                                 (get-dao 'account (val id))))
                                      '(:create :update :delete))
                   (with-form (actions/config/account/update :id (val id))
                     (config-account-data-form :update
                                               :id (val id)
                                               :data (plist-union
                                                      (params->plist title
                                                                     chequing-p)
                                                      (account-record (val id)))
                                               :styles (params->styles title)))))))))

(defpage account-page actions/config/account/update ("actions/config/account/update"
                                                     :request-type :post)
    ((id         integer chk-acc-id                     t)
     (title      string  (chk-acc-title/update title id)   t)
     (chequing-p boolean (chk-chequing-p chequing-p id)))
  (with-controller-page (account/update)
    (execute (:update 'account :set
                      :title (val title)
                      :chequing-p (val chequing-p)
                      :where (:= 'id (val id))))
    (see-other (account :id (val id)))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage account-page account/delete ("config/account/delete")
    ((id integer chk-acc-id/ref t))
  (with-view-page
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
               (for account-tree = (make-instance 'account-crud-tree
                                                  :op (if id-debit-p
                                                          :delete
                                                          :read)
                                                  :filter `(:debit-p ,debit-p)))
               (htm
                (:div :id div-id :class "window grid_6"
                      (:div :class "title" (str window-title))
                      (account-crud-menu (val id)
                                         debit-p
                                         (if id-debit-p
                                             '(:delete)
                                             '(:create :update :delete)))
                      (with-form (actions/config/account/delete :id (val id))
                        (display account-tree :key (val id) :hide-root-p t))))))))))

(defpage account-page actions/config/account/delete ("actions/config/account/delete"
                                                     :request-type :post)
    ((id integer chk-acc-id/ref t))
  (with-controller-page (account/delete)
    (delete-dao (get-dao 'account (val id)))
    (see-other (account))))
