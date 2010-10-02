(in-package :scrooge)

(declaim (optimize (speed 0) (safety 3) (debug 3)))



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
(define-uniqueness-predicate acc-title-unique-p account title id)

(defun chk-parent-acc-id (val)
  (if (or (null val) (acc-id-exists-p val))
      nil
      'parent-acc-id-unknown))

(defun chk-acc-id (id)
  (if (acc-id-exists-p id)
      nil
      'acc-id-unknown))

(defun chk-acc-id/ref (id)
  (if (and (null (chk-acc-id id))
           (null (acc-referenced-p id)))
      nil
      'acc-referenced))

(defun chk-acc-title (title &optional id)
  (cond ((eql :null title) 'acc-title-null)
        ((not (acc-title-unique-p title id)) 'acc-title-exists)
        (t nil)))

(defun chk-debitp (debitp id)
  (with-db ()
    (if (or (null id)
            (eql debitp (debit-p (get-dao 'account id))))
        nil
        'invalid-debitp-acc-id-combo)))

(defun acc-errorbar (params)
  (funcall (generic-errorbar)
           params
           '(title ((acc-title-null "Το όνομα του λογαριασμού είναι κενό.")
                    (acc-title-exists "Αυτό το όνομα λογαριασμού υπάρχει ήδη.")))))



;;; ------------------------------------------------------------
;;; Accounts - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/account/create ("actions/account/create" :request-type :post)
    ((parent-id integer chk-parent-acc-id)
     (title     string  chk-acc-title t)
     (debitp    boolean (chk-debitp debitp parent-id)))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (let ((debit-p (if parent-id (debit-p (val parent-id)) (val debitp))))
        (with-db ()
          (insert-dao (make-instance 'account
                                     :title (val title)
                                     :parent-id (or (val parent-id) :null)
                                     :debit-p debit-p))
          (see-other (account))))
      (if (and (validp parent-id) (validp debitp))
          ;; input error - go back to create page
          (see-other (account/create :parent-id (val parent-id)
                                     :title (val title)
                                     :debitp (val debitp)))
          ;; tampered URL - abort
          (see-other (notfound)))))

(define-dynamic-page actions/account/update ("actions/account/update" :request-type :post)
    ((id    integer chk-acc-id t)
     (title string  (chk-acc-title title id) t))
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
          (see-other (notfound)))))

(define-dynamic-page actions/account/delete ("actions/account/delete" :request-type :post)
    ((id integer chk-acc-id/ref t))
  (no-cache)
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'account (val id)))
        (see-other (account)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; Accounts table widget for recursive subtables
;;; ------------------------------------------------------------

(defun account-root-data (debit-p)
  (with-db ()
    (query (:select 'id 'title 'parent-id
                    :from 'account
                    :where (:and (:= 'debit-p debit-p)
                                 (:is-null 'parent-id)))
           :plists)))


(defun account-children-data (id)
  (when id
    (with-db ()
      (query (:select 'id 'title 'parent-id
                      :from 'account
                      :where (:= 'parent-id id))
             :plists))))



;;; ------------------------------------------------------------
;;; Account menus
;;; ------------------------------------------------------------

(defun account-menu (id enabled-items)
  (funcall (actions-menu)
           :item-specs (standard-actions-spec (account :id id)
                                              (account/create)
                                              (account/update :id id)
                                              (if (or (null id)
                                                      (acc-referenced-p id))
                                                  nil
                                                  (account/delete :id id)))
           :enabled-items enabled-items))



;;; ------------------------------------------------------------
;;; Account tree
;;; ------------------------------------------------------------

(defun mkfn-account-selector-states ()
  (lambda (id)
    `((t   ,(account))
      (nil ,(apply #'account id)))))

(defun account-tree (op active-id debit-table-p)
  (let* ((id-keys '(:id))
         (payload-keys '(:title))
         (db-table (account-root-data debit-table-p))
         (cancel-url (account :id active-id))
         (row-selected-p-fn (mkfn-row-selected-p id-keys))
         (selector-states-fn (mkfn-account-selector-states))
         ;; op-specific
         (row-controls-p-fn (mkfn-crud-row-controls-p op))
         (row-readonly-p-fn (mkfn-crud-row-readonly-p op))
         ;; id, payload and the row itself
         (row-id-fn (mkfn-row-id id-keys))
         (row-payload-fn (mkfn-row-payload payload-keys))
         (row-fn (mkfn-account-row row-id-fn
                                   row-payload-fn
                                   row-selected-p-fn
                                   row-controls-p-fn
                                   row-readonly-p-fn
                                   selector-states-fn
                                   cancel-url)))
    (html ()
      (:div :class "table-half forms-in-row"
            (:h2 (str (if debit-table-p "Χρεωστικοί λογαριασμοί" "Πιστωτικοί λογαριασμοί")))
            (account-subtree row-fn db-table)))))

(defun mkfn-account-row (row-id-fn row-payload-fn
                         row-selected-p-fn row-controls-p-fn row-readonly-p-fn
                         selector-states-fn cancel-url)
  (html (row-data)
    (let* ((id (funcall row-id-fn row-data))
           (row-selected-p (funcall row-selected-p-fn id))
           (row-controls-p (funcall row-controls-p-fn row-selected-p))
           (row-readonly-p (funcall row-readonly-p-fn row-selected-p)))
      (htm (:li :class (if row-selected-p "active" nil)
                (funcall (selector-link (funcall selector-states-fn id))
                         row-selected-p)
                (plist-map (lambda (key value)
                             (if row-readonly-p
                                 (htm (str value))
                                 (textbox (symbolicate key)
                                          :value value
                                          :style nil))) ;; todo -- style missing
                           (funcall row-payload-fn row-data row-readonly-p))
                (ok-link row-controls-p)
                (cancel-link cancel-url row-controls-p))))))

(defun account-subtree (row-fn db-table &optional style)
  (with-html
    (:ul :class style
         (iter (for db-row in db-table)
               ;; (when (and (eql row-id  id)
               ;;             (eql op 'create))
               ;;    (funcall row-fn nil))
               (funcall row-fn db-row)
               (account-subtree row-fn
                                (account-children-data (getf db-row :id))
                                "indent")))))

;;; ------------------------------------------------------------
;;; Account pages
;;; ------------------------------------------------------------

(define-dynamic-page account ("account/")
    ((id integer chk-acc-id))
  (no-cache)
  (if (validp id)
      (let ((id-debit-p (if (val id) (debit-p (val id)) nil)))
        (with-document ()
          (:head
           (:title "Λογαριασμοί")
           (head-config))
          (:body
           (config-header 'account)
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Κατάλογος λογαριασμών"))
                 (:div :id "debit-accounts" :class "window"
                       (account-menu (val id)
                                     (if (and (val id) id-debit-p)
                                         '(create update delete)
                                         '(create)))
                       (render (account-tree 'view (val* id) t)))
                 (:div :id "credit-accounts" :class "window"
                       (account-menu (val id)
                                     (if (and (val id) (not id-debit-p))
                                         '(create update delete)
                                         '(create)))
                       (render (account-tree 'view (val* id) nil)))
                 (footer)))))
      (see-other (notfound))))

(define-dynamic-page account/create ("account/create")
    ((id        integer chk-acc-id)
     (parent-id integer chk-parent-acc-id)
     (title     string  chk-acc-title)
     (debitp    boolean (chk-debitp debitp id)))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (let ((id-debit-p (if (val id) (debit-p (val id)) nil)))
        (with-document ()
          (:head
           (:title "Δημιουργία λογαριασμού")
           (head-config))
          (:body
           (config-header 'account)
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Δημιουργία λογαριασμού"))
                 (:div :id "debit-accounts" :class "window"
                       (account-menu (val id) (if id-debit-p
                                                  '(view)
                                                  '()))
                       (:h2 "Χρεωστικοί Λογαριασμοί")
                       (with-form (actions/account/create :parent-id parent-id :title title)
                         (account-tree 'create (val id) t)))
                 (:div :id "credit-accounts" :class "window"
                       (account-menu (val id) (if id-debit-p
                                                  '()
                                                  '(view)))
                       (:h2 "Πιστωτικοί Λογαριασμοί")
                       (with-form (actions/account/create :parent-id parent-id :title title)
                         (account-tree 'create (val id) nil)))
                 (footer)))))
      (see-other (notfound))))

(define-dynamic-page account/update ("account/update")
    ((id    integer chk-acc-id/ref t)
     (title string  chk-acc-title))
  (no-cache)
  (if (validp id)
      (let ((id-debit-p (if (val id) (debit-p (val id)) nil)))
        (with-document ()
          (:head
           (:title "Επεξεργασία λογαριασμού")
           (head-config))
          (:body
           (config-header 'account)
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Επεξεργασία λογαριασμού")
                       (acc-errorbar (list title)))
                 (:div :id "debit-accounts" :class "window"
                       (account-menu (val id)
                                     (if id-debit-p
                                         '(create view delete)
                                         '(create)))
                       (with-form (actions/account/update :id (val id) :title (val* title))
                         (account-tree 'update (val* id) t)))
                 (:div :id "credit-accounts" :class "window"
                       (account-menu (val id)
                                     (if id-debit-p
                                         '(create)
                                         '(create view delete)))
                       (with-form (actions/account/update :id (val id) :title (val* title))
                         (account-tree 'update (val* id) nil)))
                 (footer)))))
      (see-other (notfound))))

(define-dynamic-page account/delete ("account/delete")
    ((id integer chk-acc-id/ref t))
  (no-cache)
  (if (validp id)
      (let ((id-debit-p (if (val id) (debit-p (val id)) nil)))
        (with-document ()
          (:head
           (:title "Διαγραφή λογαριασμού")
           (head-config))
          (:body
           (config-header 'account)
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Διαγραφή λογαριασμού"))
                 (:div :id "debit-accounts" :class "window"
                       (account-menu (val id)
                                     (if (and (val id) id-debit-p)
                                         '(view update)
                                         '()))
                       (with-form (actions/account/delete :id (val id))
                         (account-tree 'delete (val* id) t)))
                 (:div :id "credit-accounts" :class "window"
                       (account-menu (val id)
                                     (if (and (val id) (not id-debit-p))
                                         '()
                                         '(view update)))
                       (with-form (actions/account/delete :id (val id))
                         (account-tree 'delete (val* id) nil)))
                 (footer)))))
      (see-other (notfound))))
