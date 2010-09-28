(in-package :scrooge)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

;;; ------------------------------------------------------------
;;; Accounts - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/account/create ("actions/account/create" :request-type :post)
    ((parent-id integer #'valid-parent-acc-id-p)
     (title     string  (complement #'acc-exists-p))
     (debp      boolean))
  :validators (((parent-id debp) (valid-debp-id-combo parent-id debp)))
  (no-cache) 
  (if (every #'validp (parameters *page*))
      (let ((debit-p (if parent-id (debit-p (val parent-id)) (val debp))))
        (with-db ()
          (insert-dao (make-instance 'account
                                     :title (val title)
                                     :parent-id (or (val parent-id) :null)
                                     :debit-p debit-p))
          (see-other (accounts))))
      (if (and (validp parent-id) (validp debp))
          ;; input error - go back to create page
          (see-other (account/create :parent-id (val parent-id)
                                     :title (val title)
                                     :debp (val debp)))
          ;; tampered URL - abort
          (see-other (notfound)))))

(define-dynamic-page actions/account/update ("actions/account/update" :request-type :post)
    ((id    integer #'valid-acc-id-p t) 
     (title string  (complement #'acc-exists-p)))
  (no-cache) 
  (if (every #'validp (parameters *page*))
      (with-db ()
        (execute (:update 'account :set
                          :title title
                          :where (:= 'id (val id))))
        (see-other (accounts :id (val id))))
      (if (validp id)
          ;; user error -  go back to update page
          (see-other (account/update :id id :title title))
          ;; tampered URL - abort
          (see-other (notfound)))))

(define-dynamic-page actions/account/delete ("actions/account/delete" :request-type :post)
    ((id integer #'valid-acc-id-no-subaccounts-p t))
  (no-cache)
  (if (validp id)
      (with-db ()
	(delete-dao (get-dao 'account (val id))) 
	(see-other (accounts)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; Accounts table widget for recursive subtables
;;; ------------------------------------------------------------

(defclass account-table (table-ul-crud) 
  ((name :initform "account-table")
   (header :initform nil)
   (styles :initform '(:active-row "active"
                       :inactive-row ""
                       :attention-row "attention"
                       :table "forms-in-row table-half"
                       :header (:selector "select"
                                :title "data"
                                :submit  "button"
                                :cancel "button"))) 
   ;; page interface
   (id-keys :initform '(:id))
   (payload-keys :initform '(:title))
   (filter-keys :initform '())
   (aux-keys :initform '())
   ;; crud mixin
   (main-page :initform 'accounts)
   (submit-pages :initform '(:create actions/account/create
                             :update actions/account/update
                             :delete actions/account/delete))
   (data-fn :initform nil) 
   (cells-fn :initform (config-cells-fn)) 
   ;; override defaults provided by table-ul-crud class
   (tbody-class :initform 'tbody-ul-crud)
   (row-class :initform 'account-row)))


;;; Special account row class, intended as a hook for the render method

(defclass account-row (row-ul-crud)
  ())

(defmethod render ((row account-row) &key)
  (let* ((table (table row))
         (cells-list (funcall (cells-fn table) row))
         (child-data (apply #'account-children-data (id-data row))))
    (with-html
      (:li :class (style row)
           (render cells-list)
           (when child-data 
             (render (make-instance 'tbody-ul
                                    :table table
                                    :data child-data
                                    :style "indent")))))))

(defun account-data-fn (debit-p)
  (lambda ()
    (with-db ()
      (query (:select 'id 'title 'parent-id :from 'account
                      :where (:and (:= 'debit-p debit-p)
                                   (:is-null 'parent-id)))
             :plists))))


(defun account-children-data (&key id)
  (when id ;; the row for 'create' may have id = NIL, thus this check
    (with-db ()
      (query (:select 'id 'title 'parent-id :from 'account
                      :where (:= 'parent-id id))
             :plists))))






;;; ------------------------------------------------------------
;;; Snippets
;;; ------------------------------------------------------------

(define-menu account-menu (id debp) (:div-style "actions" :ul-style "hmenu")
  (:create (with-html 
	     (:li (:a :href (account/create :id id :parent-id id :debp debp)
		      (:img :src (url "img/add.png")) "Δημιουργία"))))
  (:view (if id
	     (with-html
	       (:li (:a :href (accounts :id id)
			(:img :src (url "img/magnifier.png")) "Προβολή")))
	     nil))
  (:edit (if id
	     (with-html
	       (:li (:a :href (account/update :id id)
			(:img :src (url "img/pencil.png")) "Επεξεργασία")))
	     nil))
  (:delete (if (and id
		    (not (get-subaccounts id))
		    (not (get-transactions id)))
	       (with-html
		 (:li (:a :href (account/delete :id id)
			  (:img :src (url "img/delete.png")) "Διαγραφή")))
	       nil)))

(defun get-subaccounts (acc-id)
  (with-db ()
    (query (:select 'id :from 'account :where (:= 'parent-id acc-id))
	   :single)))

(defun get-transactions (acc-id)
  (with-db ()
    (query (:select 'id
                    :from 'tx
                    :where (:or (:= 'debit-acc-id acc-id)
                                (:= 'credit-acc-id acc-id))))))


;;; ------------------------------------------------------------
;;; Account tree
;;; ------------------------------------------------------------

(defun mkfn-account-selector-states ()
  (lambda (id)
    `((t   ,(account))
      (nil ,(apply #'account id)))))

(defun account-tree (op id)
  (let* ((id-keys '(:id))
         (payload-keys '(:title))
         (db-table )
         (cancel-url (account :id (val* id)))
         (row-selected-p-fn (mkfn-row-selected-p id-keys))
         (selector-states-fn (mkfn-account-selector-states))
         ;; op-specific
         (row-controls-p-fn (mkfn-crud-row-controls-p op))
         (row-readonly-p-fn (mkfn-crud-row-readonly-p op))
         ;; id, payload and the row itself
         (row-id-fn (mkfn-row-id id-keys))
         (row-payload-fn (mkfn-row-payload op payload-keys)) 
         (row (mkfn-crud-row row-id-fn
                             row-payload-fn 
                             row-selected-p-fn
                             row-controls-p-fn
                             row-readonly-p-fn
                             selector-states-fn
                             cancel-url)))
    (html ()
      (:table :class "table-half forms-in-row"
              (thead "" "Ονομασία τράπεζας" "" "")
              (:tbody
               (when (eql op 'create)
                 (funcall row nil))
               (iter (for db-row in db-table)
                     (funcall row db-row)))))))

;;; ------------------------------------------------------------
;;; Account pages
;;; ------------------------------------------------------------

(define-dynamic-page account ("account/")
    ((id integer #'valid-acc-id-p))
  (no-cache) 
  (if (validp id)
      (with-document ()
        (:head
         (:title "Λογαριασμοί")
         (head-config))
        (:body
         (config-header 'account)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Κατάλογος λογαριασμών"))
               (:div :id "debit-account" :class "window"
                     (account-menu ...)
                     (render (account-tree 'view id)))
               (:div :id "credit-account" :class "window"
                     (account-menu ...)
                     (render (account-tree 'view id))))))
      (let ((id-debit-p (if (val id) (debit-p (val id)) nil)))
        (standard-page
         :name 'accounts
         :title "Λογαριασμοί"
         :message "Λογαριασμοί"
         :body (list (window :name "debit-accounts"
                             :body (html ()
                                     (apply #'account-menu
                                            (if id-debit-p (val id) nil)
                                            t
                                            (if id-debit-p
                                                '(:create :edit :delete)
                                                '(:create)))
                                     (:h2 "Χρεωστικοί Λογαριασμοί") 
                                     (render (make-instance 'account-table
                                                            :name "debit-acc-table" 
                                                            :operation :view
                                                            :params params
                                                            :data-fn (account-data-fn t)))))
                     (window :name "credit-accounts"
                             :body (html ()
                                     (apply #'account-menu
                                            (if id-debit-p nil (val id))
                                            nil
                                            (if id-debit-p
                                                '(:create)
                                                '(:create :edit :delete)))
                                     (:h2 "Πιστωτικοί Λογαριασμοί") 
                                     (render (make-instance 'account-table
                                                            :name "credit-acc-table" 
                                                            :operation :view
                                                            :params params
                                                            :data-fn (account-data-fn nil))))))))
      (see-other (notfound))))



(define-dynamic-page account/create ("account/create")
    ((id integer #'valid-acc-id-p)
     (parent-id integer #'valid-parent-acc-id-p) 
     (title     string  (complement #'acc-exists-p))
     (debp      boolean))
  :validators (((parent-id debp) (valid-debp-id-combo parent-id debp)))
  (no-cache) 
  (with-parameter-list params
    (if (every #'validp params)
        (with-parameter-rebinding #'val
          (let ((debit-p (if parent-id (debit-p parent-id) debp)))
            (standard-page
             :name 'accounts
             :title "Δημιουργία λογαριασμού"
             :message "Δημιουργία λογαριασμού"
             :body (list
                    (window :name "debit-accounts"
                            :body (html ()
                                    (account-menu nil nil)
                                    (:h2 "Χρεωστικοί Λογαριασμοί")
                                    (render (make-instance 'account-table
                                                           :name "debit-acc-table" 
                                                           :operation (if debit-p
                                                                          :create
                                                                          :view)
                                                           :aux-keys '(:debp :parent-id)
                                                           :params params
                                                           :data-fn (account-data-fn t)))))
                    (window :name "credit-accounts"
                            :body (html ()
                                    (account-menu nil nil)
                                    (:h2 "Πιστωτικοί Λογαριασμοί")
                                    (render (make-instance 'account-table
                                                           :name "credit-acc-table" 
                                                           :operation (if debit-p
                                                                          :view
                                                                          :create)
                                                           :aux-keys '(:debp :parent-id)
                                                           :params params
                                                           :data-fn (account-data-fn nil))))))))) 
        (see-other (notfound)))))

(define-dynamic-page account/update ("account/update")
    ((id    integer #'valid-acc-id-p            t)
     (title string  (complement #'acc-exists-p)))
  (no-cache)
  (with-parameter-list params
    (if (validp id)
        (standard-page
         :name 'accounts
         :title "Επεξεργασία λογαριασμού"
         :message "Επεξεργασία λογαριασμού"
         :body (list (window :name "debit-accounts"
                             :body (html ()
                                     (account-menu (val id) nil :view :delete)
                                     (:h2 "Χρεωστικοί Λογαριασμοί")
                                     (render (make-instance 'account-table
                                                            :name "debit-acc-table" 
                                                            :operation :update
                                                            :params params
                                                            :data-fn (account-data-fn t)))))
                     (window :name "credit-accounts"
                             :body (html ()
                                     (account-menu (val id) nil :view :delete)
                                     (:h2 "Πιστωτικοί Λογαριασμοί")
                                     (render (make-instance 'account-table
                                                            :name "credit-acc-table"
                                                            :operation :update
                                                            :params params
                                                            :data-fn (account-data-fn nil))))))) 
        (see-other (notfound)))))

(define-dynamic-page account/delete ("account/delete")
    ((id integer #'valid-acc-id-no-subaccounts-p t))
  (no-cache)
  (if (validp id)
      (with-parameter-list params
        (standard-page
         :name 'accounts
         :title "Διαγραφή λογαριασμού"
         :message "Διαγραφή λογαριασμού"
         :body (list (window :name "debit-accounts"
                             :body (html ()
                                     (account-menu (val id) nil :view :edit)
                                     (:h2 "Χρεωστικοί λογαριασμοί")
                                     (render (make-instance 'account-table
                                                            :name "debit-acc-table" 
                                                            :operation :delete
                                                            :params params
                                                            :data-fn (account-data-fn t)))))
                     (window :name "credit-accounts"
                             :body (html ()
                                     (account-menu (val id) nil :view :edit)
                                     (:h2 "Χρεωστικοί λογαριασμοί")
                                     (render (make-instance 'account-table
                                                            :name "credit-acc-table" 
                                                            :operation :delete
                                                            :params params
                                                            :data-fn (account-data-fn nil))))))))
      (see-other (notfound))))


