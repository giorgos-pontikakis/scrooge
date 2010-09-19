(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; ------------------------------------------------------------
;;; TOF - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/tof/create ((title string (complement #'tof-exists-p)))
    ("actions/tof/create" :request-type :post)
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (insert-dao (make-instance 'tof :title (val title)))
        (see-other (tof :id (tof-id (val title)))))
      (see-other (tof/create :title (raw title)))))

(define-dynamic-page actions/tof/update ((id    integer #'tof-id-exists-p) 
                                         (title string  (complement #'tof-exists-p)))
    ("actions/tof/update" :request-type :post)
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (execute (:update 'tof :set 
                          'title (val title)
                          :where (:= 'id (val id))))
        (see-other (tof :id (val id))))
      (see-other (tof/update :id (raw id) :title (raw title)))))

(define-dynamic-page actions/tof/delete ((id integer #'tof-id-exists-p))
    ("actions/tof/delete" :request-type :post)
  (if (validp id)
      (with-db ()
	(delete-dao (get-dao 'tof (val id)))
	(see-other (tof)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; TOF menus
;;; ------------------------------------------------------------

(defun tof-menu (id enabled-items)
  (funcall (actions-menu)
           :item-specs (standard-actions-spec (tof :id id)
                                              (tof/create)
                                              (tof/update :id id)
                                              (if (or (null id)
                                                      (tof-referenced-p id))
                                                  nil
                                                  (tof/delete :id id)))
           :enabled-items enabled-items))

(defun tof-referenced-p (id)
  (with-db ()
    (and id
         (query (:select 'id
                         :from 'company
                         :where (:= 'tof-id id))))))

(defun tof-errorbar (params)
  (funcall (generic-errorbar)
           params
           '((title "Αυτό το όνομα Δ.Ο.Υ. υπάρχει ήδη"))))



;;; ------------------------------------------------------------
;;; TOF table
;;; ------------------------------------------------------------

(defun mkfn-tof-selector-states ()
  (lambda (id)
    `((t   ,(tof))
      (nil ,(apply #'tof id)))))

(defun tof-table (op id)
  (let* ((id-keys '(:id))
         (payload-keys '(:title))
         (db-table (config-data 'tof))
         (cancel-url (tof :id (val* id)))
         (row-selected-p-fn (mkfn-row-selected-p id-keys))
         (selector-states-fn (mkfn-tof-selector-states))
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
              (thead "" "Ονομασία Δ.Ο.Υ." "" "")
              (:tbody
               (when (eql op 'create)
                 (funcall row nil))
               (iter (for db-row in db-table)
                     (funcall row db-row)))))))



;;; ------------------------------------------------------------
;;; TOF - Pages
;;; ------------------------------------------------------------

(define-dynamic-page tof ((id integer #'tof-id-exists-p))
    ("config/tof")
  (no-cache)
  (if (validp id) 
      (with-document ()
        (:head
         (:title "Δ.Ο.Υ.")
         (head-config))
        (:body
         (config-header 'tof)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Κατάλογος Δ.Ο.Υ."))
               (:div :id "tof" :class "window"
                     (tof-menu (val id) (if (val id)
                                             '(create update delete)
                                             '(create)))
                     (render (tof-table 'view id)))
               (footer))))
      (see-other (notfound))))

(define-dynamic-page tof/create ((title string (complement #'tof-exists-p)))
    ("config/tof/create")
  (no-cache)
  (with-document ()
    (:head
     (:title "Δημιουργία Δ.Ο.Υ.")
     (head-config))
    (:body
     (config-header 'tof)
     (:div :id "body"
           (:div :class "message"
                 (:h2 :class "info" "Δημιουργία τράπεζας")
                 (tof-errorbar (list title)))
           (:div :id "tof" :class "window"
                 (tof-menu nil '(view))
                 (with-form (actions/tof/create :title title)
                   (tof-table 'create nil)))
           (footer)))))

(define-dynamic-page tof/update ((id integer #'tof-id-exists-p)
                                 (title string (complement #'tof-exists-p)))
    ("config/tof/update")
  (no-cache)
  (if (validp id)
      (with-document ()
        (:head
         (:title "Επεξεργασία Δ.Ο.Υ.")
         (head-config))
        (:body
         (config-header 'tof)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Επεξεργασία Δ.Ο.Υ.")
                     (tof-errorbar (list title)))
               (:div :id "tof" :class "window"
                     (tof-menu (val id) '(view delete))
                     (with-form (actions/tof/update :id (val id))
                       (tof-table 'update id)))
               (footer))))
      (see-other (notfound))))

(define-dynamic-page tof/delete ((id integer #'tof-id-exists-p))
    ("config/tof/delete")
  (no-cache)
  (if (validp id)
      (with-document ()
        (:head
         (:title "Διαγραφή Δ.Ο.Υ.")
         (head-config))
        (:body
         (config-header 'tof)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Διαγραφή Δ.Ο.Υ."))
               (:div :id "tof" :class "window"
                     (tof-menu (val id) '(view update))
                     (with-form (actions/tof/delete :id (val id))
                       (tof-table 'delete id)))
               (footer))))
      (see-other (notfound))))







