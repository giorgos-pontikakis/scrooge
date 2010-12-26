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



;;; ------------------------------------------------------------
;;; Accounts - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/account/create ("actions/account/create" :request-type :post)
    ((parent-id integer chk-parent-acc-id)
     (title     string  chk-acc-title t)
     (debitp    boolean (chk-debitp debitp parent-id)))
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
;;; Account menu
;;; ------------------------------------------------------------

(defun account-menu (id debitp &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "account-actions"
                          :style "hnavbar actions grid_6 alpha"
                          :spec (standard-actions-spec (account :id id)
                                                       (account/create :debitp debitp :parent-id id)
                                                       (account/update :id id)
                                                       (if (or (null id)
                                                               (acc-referenced-p id))
                                                           nil
                                                           (account/delete :id id))))
           :disabled-items disabled-items))




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
      (labels ((make-nodes (key)
                 (mapcar (lambda (rec)
                           (make-instance 'crud-node
                                          :collection tree
                                          :key key
                                          :record rec
                                          :parent-key key
                                          :children (make-nodes (getf rec :id))))
                         (remove-if-not (lambda (rec)
                                          (equal key (getf rec :parent-id)))
                                        records))))
        (make-instance 'crud-node
                       :collection tree
                       :key 'root
                       :record nil
                       :parent-key nil
                       :children (make-nodes :null))))))

(defmethod insert-item ((tree account-tree) &key record parent-key)
  (let ((parent-node (find-node (root tree) parent-key))
        (new-node (make-instance 'node
                                 :key (getf record :id)
                                 :record record
                                 :collection tree
                                 :children ())))
    (setf (children parent-node)
          (cons new-node (children parent-node)))
    new-node))


;;; nodes

(defclass account-node (crud-node)
  ())

(defmethod cells ((node account-node) &key)
  (let* ((id (key node))
         (record (record node))
         (collection (collection node)))
    (list :selector (make-instance 'selector-cell
                                   :style "selector"
                                   :states (list :on (account)
                                                 :off (account :id id :debitp (filter collection))))
          :payload (make-instance 'textbox-cell
                                  :name 'title
                                  :style "payload"
                                  :value (getf record :title))
          :controls (list
                     (make-instance 'ok-cell
                                    :style "control")
                     (make-instance 'cancel-cell
                                    :style "control"
                                    :href (account :id id))))))




;;; ------------------------------------------------------------
;;; Other areas
;;; ------------------------------------------------------------

(defun account-notifications (&rest params)
  (notifications '(title ((acc-title-null "Το όνομα λογαριασμού είναι κενό.")
                          (acc-title-exists "Αυτό το όνομα λογαριασμού υπάρχει ήδη.")))
                 params))




;;; ------------------------------------------------------------
;;; Account pages
;;; ------------------------------------------------------------

(define-dynamic-page account ("config/account")
    ((id integer chk-acc-id)
     (debitp boolean))
  (no-cache)
  (if (validp id)
      (let ((account-tree (make-instance 'account-tree
                                         :op 'view
                                         :filter (val* debitp))))
        (with-document ()
          (:head
           (:title "Λογαριασμοί")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (config-menu 'account)
                 (iter
                   (for flag = (list debitp (not debitp)))
                   (for div-id in '("debit-accounts" "credit-accounts"))
                   (for window-title in '("Πιστωτικοί λογαριασμοί" "Χρεωστικοί λογαριασμοί"))
                   (htm
                    (:div :id div-id :class "window grid_6"
                          (:div :class "title" (str window-title))
                          (account-menu (val id)
                                        flag
                                        (if (and (val id) flag)
                                            '(view)
                                            '(view update delete)))
                          (display account-tree :selected-id (val* id)))))))))
      (see-other (notfound))))

(define-dynamic-page account/create ("account/create")
    ((parent-id integer chk-acc-id)
     (debitp    boolean (chk-debitp debitp parent-id))
     (title     string  chk-acc-title))
  (no-cache)
  (if (and (validp parent-id) (validp debitp))
      (with-document ()
        (:head
         (:title "Δημιουργία λογαριασμού")
         (config-headers))
        #|(:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-menu 'account)
               (:div :id "debit-accounts" :class "window grid_6"
                     (:div :class "title" "Δημιουργία λογαριασμού")
                     (account-menu (val parent-id)
                                   t
                                   (if (and (val parent-id) (debit-p (val parent-id)))
                                       '()
                                       '(create update delete)))))

         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Δημιουργία λογαριασμού")
                     (acc-errorbar (list title)))
               (:div :id "debit-accounts" :class "window"
                     (account-menu (val parent-id)
                                   t
                                   (if (and (val parent-id) (debit-p (val parent-id)))
                                       '(view)
                                       '()))
                     (with-form (actions/account/create :parent-id (val parent-id)
                                                        :title (val* title)
                                                        :debitp (val debitp))
                       (account-tree 'create t (val parent-id) (val debitp))))
               (:div :id "credit-accounts" :class "window"
                     (account-menu (val parent-id)
                                   nil
                                   (if (and (val parent-id) (debit-p (val parent-id)))
                                       '()
                                       '(view)))
                     (with-form (actions/account/create :parent-id (val parent-id)
                                                        :title (val* title)
                                                        :debitp (val debitp))
                       (account-tree 'create nil (val parent-id) (val debitp))))
               (footer)))|#)
      (see-other (notfound))))

(define-dynamic-page account/update ("account/update")
    ((id    integer chk-acc-id t)
     (title string  chk-acc-title))
  (no-cache)
  (if (validp id)
      (with-document ()
        #|(:head
         (:title "Επεξεργασία λογαριασμού")
         (global-headers))|#
        #|(:body
         (config-header 'account)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Επεξεργασία λογαριασμού")
                     (acc-errorbar (list title)))
               (:div :id "debit-accounts" :class "window"
                     (account-menu (val id)
                                   t
                                   (if (debit-p (val id))
                                       '(create view delete)
                                       '(create)))
                     (with-form (actions/account/update :id (val id) :title (val* title))
                       (account-tree 'update t (val* id))))
               (:div :id "credit-accounts" :class "window"
                     (account-menu (val id)
                                   nil
                                   (if (debit-p (val id))
                                       '(create)
                                       '(create view delete)))
                     (with-form (actions/account/update :id (val id) :title (val* title))
                       (account-tree 'update nil (val* id))))
               (footer)))|#)
      (see-other (notfound))))

(define-dynamic-page account/delete ("account/delete")
    ((id integer chk-acc-id/ref t))
  (no-cache)
  (if (validp id)
      (with-document ()
        #|(:head
         (:title "Διαγραφή λογαριασμού")
         (global-headers))|#
        #|(:body
         (config-header 'account)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Διαγραφή λογαριασμού"))
               (:div :id "debit-accounts" :class "window"
                     (account-menu (val id)
                                   t
                                   (if (and (val id) (debit-p (val id)))
                                       '(view update)
                                       '()))
                     (with-form (actions/account/delete :id (val id))
                       (account-tree 'delete t (val* id))))
               (:div :id "credit-accounts" :class "window"
                     (account-menu (val id)
                                   nil
                                   (if (debit-p (val id))
                                       '()
                                       '(view update)))
                     (with-form (actions/account/delete :id (val id))
                       (account-tree 'delete nil (val* id))))
               (footer)))|#)
      (see-other (notfound))))




;;; ------------------------------------------------------------
;;; Account tree
;;; ------------------------------------------------------------

;; (defun mkfn-account-selector-states ()
;;   (lambda (id)
;;     `((t   ,(account))
;;       (nil ,(funcall #'account :id (getf id :id))))))

;; (defun account-tree (op debit-table-p id &optional debitp)
;;   (let* ((id-keys (if (eql op 'create) '(:id :parent-id) '(:id)))
;;          (payload-keys '(:title))
;;          (cancel-url (account :id id))
;;          (row-selected-p-fn (mkfn-row-selected-p id-keys))
;;          (selector-states-fn (mkfn-account-selector-states))
;;          ;; op-specific
;;          (row-controls-p-fn (mkfn-crud-row-controls-p op))
;;          (row-readonly-p-fn (mkfn-crud-row-readonly-p op))
;;          ;; id, payload and the row itself
;;          (row-id-fn (mkfn-row-id id-keys))
;;          (row-payload-fn (mkfn-row-payload payload-keys))
;;          (row-fn (mkfn-account-row row-id-fn
;;                                    row-payload-fn
;;                                    row-selected-p-fn
;;                                    row-controls-p-fn
;;                                    row-readonly-p-fn
;;                                    selector-states-fn
;;                                    cancel-url)))
;;     (html ()
;;       (:div :class "table-half forms-in-row"
;;             (:h2 (str (if debit-table-p "Χρεωστικοί λογαριασμοί" "Πιστωτικοί λογαριασμοί")))
;;             (account-subtree op
;;                              debit-table-p
;;                              row-fn
;;                              id
;;                              debitp
;;                              nil)))))

;; (defun account-subtree (op debit-table-p row-fn id debitp parent-id &optional style)
;;   (let ((db-table (if (null parent-id)
;;                       (account-root-data debit-table-p)
;;                       (account-children-data parent-id))))
;;     (with-html
;;       (:ul :class style
;;            (when (and (eql op 'create)
;;                       (eql id parent-id)
;;                       (eql debit-table-p debitp))
;;              (funcall row-fn (list :id nil :parent-id parent-id)))
;;            (iter (for db-row in db-table)
;;                  (funcall row-fn db-row)
;;                  (account-subtree op
;;                                   debit-table-p
;;                                   row-fn
;;                                   id
;;                                   debitp
;;                                   (getf db-row :id)
;;                                   "indent"))))))

;; (defun mkfn-account-row (row-id-fn row-payload-fn
;;                          row-selected-p-fn row-controls-p-fn row-readonly-p-fn
;;                          selector-states-fn cancel-url)
;;   (html (row-data)
;;     (let* ((id (funcall row-id-fn row-data))
;;            (row-selected-p (funcall row-selected-p-fn id))
;;            (row-controls-p (funcall row-controls-p-fn row-selected-p))
;;            (row-readonly-p (funcall row-readonly-p-fn row-selected-p)))
;;       (htm (:li :class (if row-selected-p "active" nil)
;;                 (funcall (selector-link (funcall selector-states-fn id))
;;                          row-selected-p)
;;                 (plist-map (lambda (key value)
;;                              (if row-readonly-p
;;                                  (htm (str value))
;;                                  (textbox (symbolicate key)
;;                                           :value value
;;                                           :style nil))) ;; todo -- style missing
;;                            (funcall row-payload-fn row-data row-readonly-p))
;;                 (ok-link row-controls-p)
;;                 (cancel-link cancel-url row-controls-p))))))


;;; ------------------------------------------------------------
;;; Accounts table widget for recursive subtables
;;; ------------------------------------------------------------

;; (defun account-root-data (debit-p)
;;   (with-db ()
;;     (query (:select 'id 'title 'parent-id
;;                     :from 'account
;;                     :where (:and (:= 'debit-p debit-p)
;;                                  (:is-null 'parent-id)))
;;            :plists)))

;; (defun account-children-data (id)
;;   (when id
;;     (with-db ()
;;       (query (:select 'id 'title 'parent-id
;;                       :from 'account
;;                       :where (:= 'parent-id id))
;;              :plists))))
