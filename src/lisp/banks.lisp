(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; ------------------------------------------------------------
;;; Bank - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/bank/create ((title string (complement #'bank-exists-p)))
    ("actions/bank/create" :request-type :post)
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (insert-dao (make-instance 'bank :title (val title)))
        (see-other (bank :id (bank-id (val title)))))
      (see-other (bank/create :title (raw title)))))

(define-dynamic-page actions/bank/update ((id    integer #'bank-id-exists-p)
                                          (title string  (complement #'bank-exists-p)))
    ("actions/bank/update" :request-type :post)
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (execute (:update 'bank :set
                          'title (val title)
                          :where (:= 'id (val id))))
        (see-other (bank :id (val id))))
      (see-other (bank/update :id (raw id) :title (raw title)))))

(define-dynamic-page actions/bank/delete ((id integer #'bank-id-exists-p))
    ("actions/bank/delete" :request-type :post)
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'bank (val id)))
        (see-other (bank)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; Bank menus
;;; ------------------------------------------------------------

(defun bank-menu (id enabled-items)
  (funcall (actions-menu)
           :item-specs (standard-actions-spec (bank :id id)
                                              (bank/create)
                                              (bank/update :id id)
                                              (if (or (null id)
                                                      (bank-referenced-p id))
                                                  nil
                                                  (bank/delete :id id)))
           :enabled-items enabled-items))

(defun bank-referenced-p (id)
  (with-db ()
    (and id
         (query (:select 'id
                         :from 'cheque
                         :where (:= 'bank-id id))))))

(defun bank-errorbar (params)
  (funcall (generic-errorbar)
           params
           '((title "Αυτό το όνομα τράπεζας υπάρχει ήδη."))))



;;; ------------------------------------------------------------
;;; Bank table
;;; ------------------------------------------------------------

(defun mkfn-bank-selector-states ()
  (lambda (id)
    `((t   ,(bank))
      (nil ,(apply #'bank id)))))

(defun bank-table (op id)
  (let* ((id-keys '(:id))
         (payload-keys '(:title))
         (db-table (config-data 'bank))
         (cancel-url (bank :id (val* id)))
         (row-selected-p-fn (mkfn-row-selected-p id-keys))
         (selector-states-fn (mkfn-bank-selector-states))
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
               (when (eql op :create)
                 (funcall row nil))
               (iter (for db-row in db-table)
                     (funcall row db-row)))))))



;;; ------------------------------------------------------------
;;; Bank - Pages
;;; ------------------------------------------------------------

(define-dynamic-page bank ((id integer #'bank-id-exists-p))
    ("config/bank")
  (no-cache)
  (if (validp id) 
      (with-document ()
        (:head
         (:title "Τράπεζες")
         (head-config))
        (:body
         (config-header 'bank)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Κατάλογος τραπεζών"))
               (:div :id "bank" :class "window"
                     (bank-menu (val id) (if (val id)
                                             '(:create :update :delete)
                                             '(:create)))
                     (render (bank-table :view id)))
               (footer))))
      (see-other (notfound))))

(define-dynamic-page bank/create ((title string (complement #'bank-exists-p)))
    ("config/bank/create")
  (no-cache)
  (with-document ()
    (:head
     (:title "Δημιουργία τράπεζας")
     (head-config))
    (:body
     (config-header 'bank)
     (:div :id "body"
           (:div :class "message"
                 (:h2 :class "info" "Δημιουργία τράπεζας")
                 (bank-errorbar (list title)))
           (:div :id "bank" :class "window"
                 (bank-menu nil '(:view))
                 (with-form (actions/bank/create :title title)
                   (bank-table :create nil)))
           (footer)))))

(define-dynamic-page bank/update ((id    integer #'bank-id-exists-p)
                                  (title string  (complement #'bank-exists-p)))
    ("config/bank/update")
  (no-cache) 
  (if (validp id)
      (with-document ()
        (:head
         (:title "Επεξεργασία τράπεζας")
         (head-config))
        (:body
         (config-header 'bank)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Επεξεργασία τράπεζας")
                     (bank-errorbar (list title)))
               (:div :id "bank" :class "window"
                     (bank-menu (val id) '(:view :delete))
                     (with-form (actions/bank/update :id (val id))
                       (bank-table :update id)))
               (footer))))
      (see-other (notfound))))

(define-dynamic-page bank/delete ((id integer #'bank-id-exists-p))
    ("config/bank/delete")
  (no-cache)
  (if (validp id)
      (with-document ()
        (:head
         (:title "Διαγραφή τράπεζας")
         (head-config))
        (:body
         (config-header 'bank)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Διαγραφή τράπεζας"))
               (:div :id "bank" :class "window"
                     (bank-menu (val id) '(:view :update))
                     (with-form (actions/bank/delete :id (val id))
                       (bank-table :delete id)))
               (footer))))
      (see-other (notfound))))
