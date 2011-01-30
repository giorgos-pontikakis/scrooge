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

(defun chk-debitp (debitp id)
  (with-db ()
    (if (or (null id)
            (eql debitp (debit-p (get-dao 'account id))))
        nil
        :invalid-debitp-acc-id-combo)))



;;; ------------------------------------------------------------
;;; Accounts - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/config/account/create ("actions/config/account/create"
                                                    :request-type :post)
    ((parent-id integer chk-parent-acc-id)
     (title     string  chk-new-acc-title t)
     (debitp    boolean (chk-debitp debitp parent-id)))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (insert-dao (make-instance 'account
                                     :parent-id (or (val parent-id) :null)
                                     :title (val title)
                                     :debit-p (val debitp)))
          (see-other (account)))
        (if (and (validp parent-id) (validp debitp))
            ;; input error - go back to create page
            (see-other (account/create :parent-id (raw parent-id)
                                       :title (raw title)
                                       :debitp (raw debitp)))
            ;; tampered URL - abort
            (see-other (notfound))))))

(define-dynamic-page actions/config/account/update ("actions/config/account/update"
                                                    :request-type :post)
    ((id    integer chk-acc-id t)
     (title string (chk-new-acc-title title id) t))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (execute (:update 'account :set
                            :title (val title)
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

(defun account-menu (id debitp &optional disabled-items)
  (let ((prefix (if debitp "debit" "credit")))
    (display (make-instance 'actions-menu
                            :id (conc prefix "-account-actions")
                            :style "hnavbar actions grid_6 alpha"
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

(defclass account-tree (crud-tree)
  ())

(defmethod read-items ((tree account-tree))
  (with-db ()
    (let ((records (query (:select 'id 'title 'parent-id
                                   :from 'account
                                   :where (:= 'debit-p (filter tree)))
                          :plists)))
      (labels ((make-nodes (parent-key)
                 (mapcar (lambda (rec)
                           (let ((key (getf rec :id)))
                             (make-instance 'account-node
                                            :collection tree
                                            :key key
                                            :record rec
                                            :parent-key parent-key
                                            :children (make-nodes key))))
                         (remove-if-not (lambda (rec)
                                          (equal parent-key (getf rec :parent-id)))
                                        records))))
        (make-instance 'account-node
                       :collection tree
                       :key nil
                       :record nil
                       :parent-key nil
                       :children (make-nodes :null))))))

(defmethod insert-item ((tree account-tree) &key record parent-key)
  (let ((parent-node (find-node (root tree) parent-key))
        (new-node (make-instance 'account-node
                                 :key parent-key
                                 :record record
                                 :collection tree
                                 :parent-key parent-key
                                 :children ())))
    (push new-node (children parent-node))))


;;; nodes

(defclass account-node (crud-node)
  ())

(defmethod cells ((node account-node) &key)
  (let* ((id (key node))
         (record (record node))
         #|(filter (filter (collection node)))|#)
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

(defun account-notifications (&rest params)
  (notifications '((title (:account-title-null "Το όνομα λογαριασμού είναι κενό."
                           :account-title-exists "Αυτό το όνομα λογαριασμού υπάρχει ήδη.")))
                 params))




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
                   (for account-tree = (make-instance 'account-tree
                                                      :op 'catalogue
                                                      :filter flag))
                   (htm
                    (:div :id div-id :class "window grid_6"
                          (:div :class "title" (str window-title))
                          (account-menu (val id)
                                        flag
                                        (if (and (val id) (eql flag (debit-p (val id))))
                                            '(catalogue)
                                            '(catalogue update delete)))
                          (display account-tree :selected-id (val* id))))))))
        (see-other (notfound)))))

(define-dynamic-page account/create ("config/account/create")
    ((parent-id integer chk-acc-id)
     (debitp    boolean (chk-debitp debitp parent-id))
     (title     string  chk-new-acc-title))
  (with-auth ("configuration")
    (no-cache)
    (if (and (validp parent-id) (validp debitp))
        (with-document ()
          (:head
           (:title "Δημιουργία λογαριασμού")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-navbar 'account)
                 (iter
                   (for flag in (list t nil))
                   (for div-id in '("debit-accounts" "credit-accounts"))
                   (for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
                   (for account-tree = (make-instance 'account-tree
                                                      :op (if (eql flag (val debitp))
                                                              'create
                                                              'catalogue)
                                                      :filter flag))
                   (htm
                    (:div :id div-id :class "window grid_6"
                          (:div :class "title" (str window-title))
                          (account-menu (val parent-id)
                                        flag
                                        '(create update delete))
                          (with-form (actions/config/account/create :parent-id (val parent-id)
                                                                    :debitp (val debitp))
                            (display account-tree
                                     :selected-id (val* parent-id)
                                     :selected-data (list :title (val* title))))))))))
        (see-other (notfound)))))



(define-dynamic-page account/update ("config/account/update")
    ((id    integer chk-acc-id t)
     (title string  (chk-new-acc-title title id)))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-document ()
          (:head
           (:title "Επεξεργασία λογαριασμού")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-navbar 'account)
                 (iter
                   (for flag in (list t nil))
                   (for div-id in '("debit-accounts" "credit-accounts"))
                   (for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
                   (for account-tree = (make-instance 'account-tree
                                                      :op (if (eql flag (debit-p (val id)))
                                                              'update
                                                              'catalogue)
                                                      :filter flag))
                   (htm
                    (:div :id div-id :class "window grid_6"
                          (:div :class "title" (str window-title))
                          (account-menu (val id)
                                        flag
                                        '(create update delete))
                          (with-form (actions/config/account/update :id (val id))
                            (display account-tree
                                     :selected-id (val* id)
                                     :selected-data (list :title (val* title))))))))))
        (see-other (notfound)))))



(define-dynamic-page account/delete ("config/account/delete")
    ((id integer chk-acc-id/ref t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-document ()
          (:head
           (:title "Διαγραφή λογαριασμού")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-navbar 'account)
                 (iter
                   (for flag in (list t nil))
                   (for div-id in '("debit-accounts" "credit-accounts"))
                   (for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
                   (for account-tree = (make-instance 'account-tree
                                                      :op (if (eql flag (debit-p (val id)))
                                                              'delete
                                                              'catalogue)
                                                      :filter flag))
                   (htm
                    (:div :id div-id :class "window grid_6"
                          (:div :class "title" (str window-title))
                          (account-menu (val id)
                                        flag
                                        (if flag
                                            '()
                                            '(create update delete)))
                          (with-form (actions/config/account/delete :id (val id))
                            (display account-tree :selected-id (val* id)))))))))
        (see-other (notfound)))))
