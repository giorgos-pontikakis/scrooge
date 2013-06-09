(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass account-family (family-mixin)
  ()
  (:default-initargs :parameter-groups '(:system (account-id parent-id debit-p)
                                         :payload (title chequing-p)
                                         :filter (debit-p))))

(defclass account-page (auth-dynamic-page account-family)
  ((messages
    :allocation :class
    :reader messages
    :initform '((title (:account-title-null "Το όνομα λογαριασμού είναι κενό."
                        :account-title-exists "Αυτό το όνομα λογαριασμού υπάρχει ήδη."))))))



;;; ------------------------------------------------------------
;;; Account - Validation
;;; ------------------------------------------------------------

(defun account-referenced-p (account-id)
  (or (referenced-by account-id 'account 'parent-id)
      (referenced-by account-id 'company 'expenses-account-id 'revenues-account-id)
      (referenced-by account-id 'account-role 'account-id)
      (referenced-by account-id 'temtx 'debit-account-id 'credit-account-id)
      (referenced-by account-id 'tx 'debit-account-id 'credit-account-id)))

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

(defun chk-parent-account-id (val)
  (if (or (null val) (account-id-exists-p val))
      nil
      :parent-account-id-unknown))

(defun chk-account-id (account-id)
  (cond ((eql :null account-id)
         :account-id-null)
        ((not (account-id-exists-p account-id))
         :account-id-unknown)))

(defun chk-account-id/ref (account-id)
  (cond ((chk-account-id account-id))
        ((account-referenced-p account-id) :account-referenced)))

(defun chk-account-title/create (title)
  (cond ((eql :null title) :account-title-null)
        ((account-title-exists-p title) :account-title-exists)))

(defun chk-account-title/update (title account-id)
  (cond ((eql title :null) :account-title-null)
        ((account-title-exists-p title account-id) :account-title-unknown)))

(defun chk-account-title (title)
  (cond ((eql title :null) :account-title-null)
        ((not (account-title-exists-p title)) :account-title-unknown)))

(defun chk-chq-account-title (title)
  (cond ((eql title :null) :account-title-null)
        ((not (chq-account-title-exists-p title)) :account-title-unknown)))

(defun chk-non-chq-account-title (title)
  (cond ((eql title :null) :account-title-null)
        ((not (non-chq-account-title-exists-p title)) :account-title-unknown)))

(defun chk-revenues-account-title (title)
  (cond ((eql title :null)
         nil)
        ((not (account-title-exists-p title))
         :account-title-unknown)
        ((not (member (account-id title) *revenue-accounts*))
         :not-revenues-account)))

(defun chk-expenses-account-title (title)
  (cond ((eql title :null)
         nil)
        ((not (account-title-exists-p title))
         :account-title-unknown)
        ((not (member (account-id title) *expense-accounts*))
         :not-expenses-account)))

(defun chk-debitp (debitp account-id)
  (with-db ()
    (if (or (null account-id)
            (eql debitp (debit-p (get-dao 'account account-id))))
        nil
        :invalid-debitp-account-id-combo)))

(defun chk-chequing-p (chequing-p account-id)
  (with-db ()
    (let ((referenced-p (account-referenced-p account-id))
          (chequing-p-changed-p (not (eql chequing-p
                                          (chequing-p (get-dao 'account account-id))))))
      (if (and referenced-p chequing-p-changed-p)
          :chequing-p-cannot-change
          nil))))



;;; ------------------------------------------------------------
;;; Account tree
;;; ------------------------------------------------------------

;;; tree

(defclass account-tree (scrooge-crud-tree)
  ((debit-p :accessor debit-p
            :initarg :debit-p
            :initform (error "While making an account-tree instance, debit-p slot is unbound")))
  (:default-initargs :record-class 'cons
                     :item-class 'account-node))

(defmethod get-records ((tree account-tree))
  (query (:select 'id 'title 'parent-id 'chequing-p 'debit-p
                  :from 'account
                  :where (:= 'debit-p (debit-p tree)))
         :plists))

(defmethod actions ((tree account-tree) &key)
  (let* ((account-id (selected-key tree))
         (spec (if (and account-id
                        (eql (debit-p tree)
                             (debit-p (get-dao 'account account-id))))
                   `(:create (,(config/account/create :debitp (debit-p tree)
                                                      :parent-id account-id)
                              "Νέος Λογαριασμός")
                     :update ,(config/account/update :account-id account-id)
                     :delete ,(if (account-referenced-p account-id)
                                  nil
                                  (config/account/delete :account-id account-id)))
                   nil)))
    (actions-menu (make-menu-spec spec)
                  (if account-id
                      '()
                      '(:create :update :delete)))))


;;; nodes

(defclass account-node (scrooge-node)
  ())

(defmethod selector ((node account-node) selected-p)
  (let ((account-id (key node)))
    (html ()
      (:a :href (if selected-p
                    (config/account)
                    (config/account :account-id account-id))
        (selector-img selected-p)))))

(defmethod payload ((node account-node) enabled-p)
  (make-instance 'textbox
                 :css-class (if (getf (record node) :chequing-p) "chequing" "")
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

(defclass account-form (scrooge-crud-form)
  ()
  (:default-initargs :record-class 'account))

(defmethod display ((form account-form) &key styles)
  (let* ((record (record form))
         (account-referenced-p (if (slot-boundp record 'id)
                                   (account-referenced-p (account-id record))
                                   nil)))
    (with-html
      (:div :id "config-account-data-form" :class "data-form"
            (:div :class "data-form-title"
                  (:label "Τίτλος"
                          (obj 'input-text
                               :value (title record)
                               :css-class (getf styles :title))))
            (obj 'input-checkbox :name 'chequing-p
                                 :value t
                                 :body "Λογαριασμός επιταγών"
                                 :css-class "inline"
                                 :checked (chequing-p record)
                                 :disabled account-referenced-p
                                 :readonly account-referenced-p))
      (:div :class "data-form-buttons"
            (ok-button :body (if (eql (op form) :update) "Ανανέωση" "Δημιουργία"))
            (cancel-button (cancel-url form) :body "Άκυρο")))))

(defmethod get-record ((form account-form))
  (get-dao 'account (key form)))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage account-page config/account ("config/account")
    ((account-id integer chk-account-id))
  (with-view-page
    (with-document ()
      (:head
        (:title "Λογαριασμοί")
        (config-headers))
      (:body
        (:div :id "container" :class "container_12"
          (header 'config)
          (navbar 'config 'account)
          (mapc (lambda (debit-p div-id window-title)
                  (let ((account-tree (make-instance 'account-tree
                                                     :op :catalogue
                                                     :selected-key (val account-id)
                                                     :debit-p debit-p)))
                    (htm
                     (:div :class "grid_6"
                       (:div :id div-id :class "window"
                         (:div :class "title" (str window-title))
                         (notifications)
                         (actions account-tree)
                         (display account-tree :hide-root-p t))))))
                '(t nil)
                '("debit-accounts" "credit-accounts")
                '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
          (footer))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage account-page config/account/create ("config/account/create")
    ((parent-id  integer chk-parent-account-id)
     (debitp     boolean (chk-debitp debitp parent-id))
     (title      string  chk-account-title/create)
     (chequing-p boolean))
  (with-view-page
    (let ((account-form (make-instance 'account-form
                                       :op :create
                                       :cancel-url (config/account))))
      (with-document ()
        (:head
          (:title "Λογαριασμός » Δημιουργία")
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (navbar 'config 'account)
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
    ((parent-id  integer chk-parent-account-id)
     (title      string  chk-account-title/create      t)
     (debitp     boolean (chk-debitp debitp parent-id))
     (chequing-p boolean))
  (with-controller-page (config/account/create)
    (with-db ()
      (let ((new-dao (make-instance 'account
                                    :parent-id (or (val parent-id)
                                                   (and (val debitp)
                                                        (account-id *debit-accounts-root-id*))
                                                   (account-id *credit-accounts-root-id*))
                                    :title (val title)
                                    :debit-p (val debitp)
                                    :chequing-p (val chequing-p))))
        (with-transaction ()
          (insert-dao new-dao)
          (update-account-globals))
        (see-other (config/account :account-id (account-id new-dao)))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage account-page config/account/update ("config/account/update")
    ((account-id integer chk-account-id                              t)
     (title      string  (chk-account-title/update title account-id))
     (chequing-p boolean (chk-chequing-p chequing-p account-id)))
  (with-view-page
    (let ((account-form (make-instance 'account-form
                                       :op :update
                                       :key (val account-id)
                                       :cancel-url (config/account :account-id (val account-id)))))
      (with-document ()
        (:head
          (:title "Λογαριασμός » Επεξεργασία")
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (navbar 'config 'account)
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
    (with-transaction ()
      (execute (:update 'account :set
                        :title (val title)
                        :chequing-p (val chequing-p)
                        :where (:= 'id (val account-id))))
      (update-account-globals))
    (see-other (config/account :account-id (val account-id)))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage account-page config/account/delete ("config/account/delete")
    ((account-id integer chk-account-id/ref t))
  (with-view-page
    (with-document ()
      (:head
        (:title "Λογαριασμός » Διαγραφή")
        (config-headers))
      (:body
        (:div :id "container" :class "container_12"
          (header 'config)
          (navbar 'config 'account)
          (mapc (lambda (debit-p div-id window-title)
                  (let ((account-tree (make-instance 'account-tree
                                                     :op :delete
                                                     :selected-key (val account-id)
                                                     :debit-p debit-p)))
                    (htm
                     (:div :class "grid_6"
                       (:div :id div-id :class "window"
                         (:div :class "title" (str window-title))
                         (notifications)
                         (actions account-tree)
                         (with-form (actions/config/account/delete :account-id (val account-id))
                           (display account-tree :hide-root-p t)))))))
                '(t nil)
                '("debit-accounts" "credit-accounts")
                '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
          (footer))))))

(defpage account-page actions/config/account/delete ("actions/config/account/delete"
                                                     :request-type :post)
    ((account-id integer chk-account-id/ref t))
  (with-controller-page (config/account/delete)
    (with-transaction ()
      (delete-dao (get-dao 'account (val account-id)))
      (update-account-globals))

    (see-other (config/account))))
