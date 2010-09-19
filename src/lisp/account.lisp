(in-package :scrooge)

(declaim (optimize (speed 0) (safety 3) (debug 3)))

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
;;; Accounts - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/account/create ((parent-id    integer #'valid-parent-acc-id-p)
					     (title string  (complement #'acc-exists-p))
                                             (debp  boolean))
    ("actions/account/create" :request-type :post
                              :validators (((parent-id debp) (valid-debp-id-combo parent-id debp))))
  (no-cache) 
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
          (let ((debit-p (if parent-id (debit-p parent-id) debp)))
            (with-db ()
              (insert-dao (make-instance 'account
                                         :title title
                                         :parent-id (or parent-id :null)
                                         :debit-p debit-p))
              (see-other (accounts)))))
	(if (and (validp parent-id) (validp debp))
            ;; input error - go back to create page
            (see-other (account/create :parent-id (val parent-id)
                                       :title (val title)
                                       :debp (val debp)))
            ;; tampered URL - abory
            (see-other (notfound))))))

(define-dynamic-page actions/account/update ((id    integer #'valid-acc-id-p t) 
					     (title string  (complement #'acc-exists-p)))
    ("actions/account/update" :request-type :post)
  (no-cache) 
  (with-parameter-list params 
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db ()
	    (execute (:update 'account :set
			      :title title
			      :where (:= 'id id)))
	    (see-other (accounts :id id))))
	(if (validp id)
            ;; user error -  go back to update page
            (see-other (account/update :id id :title title))
            ;; tampered URL - abort
            (see-other (notfound))))))

(define-dynamic-page actions/account/delete ((id integer #'valid-acc-id-no-subaccounts-p t))
    ("actions/account/delete" :request-type :post)
  (no-cache)
  (if (validp id)
      (with-db ()
	(delete-dao (get-dao 'account (val id))) 
	(see-other (accounts)))
      (see-other (notfound))))



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
;;; Pages
;;; ------------------------------------------------------------

(define-dynamic-page accounts ((id integer #'valid-acc-id-p))
    ("accounts/")
  (no-cache) 
  (if (validp id)
      (with-parameter-list params
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
                                                              :data-fn (account-data-fn nil)))))))))
      (see-other (notfound))))



(define-dynamic-page account/create ((id integer #'valid-acc-id-p)
                                     (parent-id integer #'valid-parent-acc-id-p) 
                                     (title     string  (complement #'acc-exists-p))
				     (debp      boolean))
    ("account/create" :validators (((parent-id debp) (valid-debp-id-combo parent-id debp))))
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

(define-dynamic-page account/update ((id    integer #'valid-acc-id-p            t)
				     (title string  (complement #'acc-exists-p)))
    ("account/update")
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

(define-dynamic-page account/delete ((id integer #'valid-acc-id-no-subaccounts-p t))
    ("account/delete")
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


