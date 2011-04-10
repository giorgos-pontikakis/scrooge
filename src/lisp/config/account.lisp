(in-package :scrooge)



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

(define-existence-predicate acc-id-exists-p account id)
(define-existence-predicate acc-title-exists-p account title)
(define-uniqueness-predicate acc-title-unique-p account title id)

(defun chk-parent-acc-id (val)
  (if (or (null val) (acc-id-exists-p val))
      nil
      :parent-acc-id-unknown))

(defun chk-acc-id (id)
  (if (acc-id-exists-p id)
      nil
      :acc-id-unknown))

(defun chk-acc-id/ref (id)
  (if (and (null (chk-acc-id id))
           (null (acc-referenced-p id)))
      nil
      :acc-referenced))

(defun chk-new-acc-title (title &optional id)
  (cond ((eql :null title) :account-title-null)
        ((not (acc-title-unique-p title id)) :account-title-exists)
        (t nil)))

(defun chk-acc-title (title)
  (cond ((eql title :null) :account-title-null)
        ((not (acc-title-exists-p title)) :account-title-unknown)
        (t nil)))

(defun chk-acc-title-nc (title)
  (cond ((eql title :null) :account-title-null)
        ((not (acc-title-exists-p title)) :account-title-unknown)
        ((chequing-p title) :chequing-account)
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
;;; Accounts - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/config/account/create ("actions/config/account/create"
                                                    :request-type :post)
    ((parent-id integer chk-parent-acc-id)
     (title     string  chk-new-acc-title t)
     (debitp    boolean (chk-debitp debitp parent-id))
     (chequing-p   boolean))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (let ((new-dao (make-instance 'account
                                        :parent-id (or (val parent-id) :null)
                                        :title (val title)
                                        :debit-p (val debitp)
                                        :chequing-p (val chequing-p))))
            (insert-dao new-dao)
            (see-other (account :id (id new-dao)))))
        (if (and (validp parent-id) (validp debitp))
            ;; input error - go back to create page
            (see-other (account/create :parent-id (raw parent-id)
                                       :title (raw title)
                                       :debitp (raw debitp)
                                       :chequing-p (raw chequing-p)))
            ;; tampered URL - abort
            (see-other (notfound))))))

(define-dynamic-page actions/config/account/update ("actions/config/account/update"
                                                    :request-type :post)
    ((id        integer chk-acc-id t)
     (title     string  (chk-new-acc-title title id) t)
     (chequing-p   boolean (chk-chequing-p chequing-p id)))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (execute (:update 'account :set
                            :title (val title)
                            :chequing-p (val chequing-p)
                            :where (:= 'id (val id))))
          (see-other (account :id (val id))))
        (if (validp id)
            ;; user error -  go back to update page
            (see-other (account/update :id (raw id) :title (raw title)))
            ;; tampered URL - abort
            (see-other (notfound))))))

(define-dynamic-page actions/config/account/delete ("actions/config/account/delete"
                                                    :request-type :post)
    ((id integer chk-acc-id/ref t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'account (val id)))
          (see-other (account)))
        (see-other (notfound)))))



;;; ------------------------------------------------------------
;;; Account menu
;;; ------------------------------------------------------------

(defun account-crud-menu (id debitp &optional disabled-items)
  (let ((prefix (if debitp "debit" "credit")))
    (display (make-instance 'actions-menu
                            :id (conc prefix "-account-actions")
                            :style "hnavbar actions"
                            :spec (crud-actions-spec (account :id id)
                                                     (account/create :debitp debitp
                                                                     :parent-id id)
                                                     (account/update :id id)
                                                     (if (or (null id)
                                                             (acc-referenced-p id))
                                                         nil
                                                         (account/delete :id id))))
             :disabled-items disabled-items)))




;;; ------------------------------------------------------------
;;; Account tree
;;; ------------------------------------------------------------

;;; tree

(defclass account-crud-tree (crud-tree)
  ((item-key-field        :initform :id)
   (item-parent-key-field :initform :parent-id))
  (:default-initargs :item-class 'account-crud-node))

(defmethod read-records ((tree account-crud-tree))
  (with-db ()
    (query (:select 'id 'title 'parent-id
                    :from 'account
                    :where (:= 'debit-p (filter tree)))
           :plists)))


;;; nodes

(defclass account-crud-node (crud-node)
  ())

(defmethod cells ((node account-crud-node) &key)
  (let* ((id (key node))
         (record (record node)))
    (list :selector (make-instance 'selector-cell
                                   :states (list :on (account)
                                                 :off (account :id id)))
          :payload (make-instance 'textbox-cell
                                  :name 'title
                                  :value (getf record :title))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (account :id id))))))



;;; ------------------------------------------------------------
;;; Notifications
;;; ------------------------------------------------------------

(defun account-notifications ()
  (notifications '((title (:account-title-null "Το όνομα λογαριασμού είναι κενό."
                           :account-title-exists "Αυτό το όνομα λογαριασμού υπάρχει ήδη.")))))



;;; ------------------------------------------------------------
;;; Account pages
;;; ------------------------------------------------------------

(define-dynamic-page account ("config/account")
    ((id integer chk-acc-id))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-document ()
          (:head
           (:title "Λογαριασμοί")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-navbar 'account)
                 (iter
                   (for flag in (list t nil))
                   (for div-id in '("debit-accounts" "credit-accounts"))
                   (for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
                   (for account-tree = (make-instance 'account-crud-tree
                                                      :op 'catalogue
                                                      :filter flag))
                   (htm
                    (:div :id div-id :class "window grid_6"
                          (:div :class "title" (str window-title))
                          (account-crud-menu (val id)
                                             flag
                                             (if (and (val id) (eql flag (debit-p (val id))))
                                                 '(catalogue)
                                                 '(catalogue update delete)))
                          (display account-tree :selected-id (val* id))))))))
        (see-other (notfound)))))

(define-dynamic-page account/create ("config/account/create")
    ((parent-id integer chk-parent-acc-id)
     (debitp    boolean (chk-debitp debitp parent-id))
     (title     string  chk-new-acc-title)
     (chequing-p   boolean))
  (with-auth ("configuration")
    (no-cache)
    (if (and (validp parent-id) (validp debitp))
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
                                          (debit-p (with-db ()
                                                     (get-dao 'account (val parent-id))))
                                          '(create update delete))
                       (with-form (actions/config/account/create :parent-id (val parent-id)
                                                                 :debitp (val debitp))
                         (config-account-data-form 'create
                                                   :data (parameters->plist parent-id
                                                                            title
                                                                            chequing-p)
                                                   :styles (parameters->styles title)))))))
        (see-other (notfound)))))

(define-dynamic-page account/update ("config/account/update")
    ((id      integer chk-acc-id t)
     (title   string  (chk-new-acc-title title id))
     (chequing-p boolean (chk-chequing-p chequing-p id)))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
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
                                          '(create update delete))
                       (with-form (actions/config/account/update :id (val id))
                         (config-account-data-form 'update
                                                   :id (val id)
                                                   :data (plist-union
                                                          (parameters->plist title
                                                                             chequing-p)
                                                          (get-account-plist (val id)))
                                                   :styles (parameters->styles title)))))))
        (see-other (notfound)))))

(defun get-account-plist (id)
  (with-db ()
    (query (:select 'id 'title 'chequing-p
                    :from 'account
                    :where (:= id 'id))
           :plist)))

(define-dynamic-page account/delete ("config/account/delete")
    ((id integer chk-acc-id/ref t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-document ()
          (:head
           (:title "Λογαριασμός » Διαγραφή")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-navbar 'account)
                 (iter
                   (for flag in (list t nil))
                   (for div-id in '("debit-accounts" "credit-accounts"))
                   (for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
                   (for account-tree = (make-instance 'account-crud-tree
                                                      :op (if (eql flag (debit-p (val id)))
                                                              'delete
                                                              'catalogue)
                                                      :filter flag))
                   (htm
                    (:div :id div-id :class "window grid_6"
                          (:div :class "title" (str window-title))
                          (account-crud-menu (val id)
                                             flag
                                             (if flag
                                                 '()
                                                 '(create update delete)))
                          (with-form (actions/config/account/delete :id (val id))
                            (display account-tree :selected-id (val* id)))))))))
        (see-other (notfound)))))

(defun config-account-data-form (op &key id data styles)
  (let ((disabledp (eql op 'details))
        (dependent-tx-p (if id (ref-transactions id) nil)))
    (with-html
      (:div :id "config-account-data-form" :class "data-form"
            (:div :class "data-form-first"
                  (label 'title "Τίτλος")
                  (textbox 'title
                           :value (getf data :title)
                           :disabledp disabledp
                           :style (getf styles :title)))
            (checkbox 'chequing-p "Λογαριασμός επιταγών"
                      :style "inline"
                      :value t
                      :checked (getf data :chequing-p)
                      :disabledp  dependent-tx-p)
            (:div :class "data-form-buttons"
                  (if disabledp
                      (cancel-button (account :id id)
                                     "Επιστροφή στον Κατάλογο Λογαριασμών")
                      (progn
                        (ok-button (if (eql op 'update) "Ανανέωση" "Δημιουργία"))
                        (cancel-button (account :id id) "Άκυρο"))))))))