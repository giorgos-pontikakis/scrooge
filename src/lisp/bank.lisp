(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; ------------------------------------------------------------
;;; Bank - Validation
;;; ------------------------------------------------------------

(defun bank-referenced-p (id)
  (with-db ()
    (and id
         (query (:select 'id
                         :from 'cheque
                         :where (:= 'bank-id id))))))

(define-existence-predicate bank-id-exists-p bank id)
(define-uniqueness-predicate bank-title-unique-p bank title id)

(defun chk-bank-id (id)
  (if (bank-id-exists-p id)
      nil
      'bank-id-unknown))

(defun chk-bank-id/ref (id)
  (if (and (null (chk-bank-id id))
           (null (bank-referenced-p id)))
      nil
      'bank-referenced))

(defun chk-bank-title (title &optional id)
  (cond ((eql :null title) 'bank-title-null)
        ((not (bank-title-unique-p title id)) 'bank-title-exists)
        (t nil)))

(defun bank-errorbar (params)
  (funcall (generic-errorbar)
           params
           '(title ((bank-title-null "Το όνομα της τράπεζας είναι κενό.")
                    (bank-title-exists "Αυτό το όνομα τράπεζας υπάρχει ήδη.")))))



;;; ------------------------------------------------------------
;;; Bank - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/bank/create ("actions/bank/create" :request-type :post)
    ((title string chk-bank-title t))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (insert-dao (make-instance 'bank :title (val title)))
        (see-other (bank :id (bank-id (val title)))))
      (see-other (bank/create :title (raw title)))))

(define-dynamic-page actions/bank/update ("actions/bank/update" :request-type :post)
    ((id    integer chk-bank-id t)
     (title string  (chk-bank-title title id) t))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (execute (:update 'bank :set
                          'title (val title)
                          :where (:= 'id (val id))))
        (see-other (bank :id (val id))))
      (see-other (bank/update :id (raw id) :title (raw title)))))

(define-dynamic-page actions/bank/delete ("actions/bank/delete" :request-type :post)
    ((id integer chk-bank-id/ref t))
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'bank (val id)))
        (see-other (bank)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; Bank menu
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
         (cancel-url (bank :id id))
         (row-selected-p-fn (mkfn-row-selected-p id-keys))
         (selector-states-fn (mkfn-bank-selector-states))
         ;; op-specific
         (row-controls-p-fn (mkfn-crud-row-controls-p op))
         (row-readonly-p-fn (mkfn-crud-row-readonly-p op))
         ;; id, payload and the row itself
         (row-id-fn (mkfn-row-id id-keys))
         (row-payload-fn (mkfn-row-payload payload-keys))
         (row-fn (mkfn-crud-row row-id-fn
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
                 (funcall row-fn nil))
               (iter (for db-row in db-table)
                     (funcall row-fn db-row)))))))



;;; ------------------------------------------------------------
;;; Bank - Pages
;;; ------------------------------------------------------------

(define-dynamic-page bank ("config/bank")
    ((id integer chk-bank-id))
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
                     (bank-menu (val id)
                                (if (val id)
                                    '(create update delete)
                                    '(create)))
                     (render (bank-table 'view (val* id))))
               (footer))))
      (see-other (notfound))))

(define-dynamic-page bank/create ("config/bank/create")
    ((title string chk-bank-title))
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
                 (bank-menu nil '(view))
                 (with-form (actions/bank/create :title (val* title))
                   (bank-table 'create nil)))
           (footer)))))

(define-dynamic-page bank/update ("config/bank/update")
    ((id    integer chk-bank-id t)
     (title string  (chk-bank-title title id)))
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
                     (bank-menu (val id) '(view delete))
                     (with-form (actions/bank/update :id (val id))
                       (bank-table 'update (val* id))))
               (footer))))
      (see-other (notfound))))

(define-dynamic-page bank/delete ("config/bank/delete")
    ((id integer chk-bank-id/ref t))
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
                     (bank-menu (val id) '(view update))
                     (with-form (actions/bank/delete :id (val id))
                       (bank-table 'delete (val* id))))
               (footer))))
      (see-other (notfound))))
