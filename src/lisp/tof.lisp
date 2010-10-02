(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; ------------------------------------------------------------
;;; TOF - Validation
;;; ------------------------------------------------------------

(defun tof-referenced-p (id)
  (with-db ()
    (and id
         (query (:select 'id
                         :from 'company
                         :where (:= 'tof-id id))))))

(define-existence-predicate tof-id-exists-p tof id)
(define-uniqueness-predicate tof-title-unique-p tof title id)

(defun chk-tof-id (id)
  (if (tof-id-exists-p id)
      nil
      'tof-id-unknown))

(defun chk-tof-title (title &optional id)
  (cond ((eql :null title) 'tof-title-null)
        ((not (tof-title-unique-p title id)) 'tof-title-exists)
        (t nil)))

(defun chk-tof-id/ref (id)
  (if (and (null (chk-tof-id id))
           (null (tof-referenced-p id)))
      nil
      'tof-referenced))

(defun tof-errorbar (params)
  (funcall (generic-errorbar)
           params
           '(title ((tof-title-null "Το όνομα της Δ.Ο.Υ. είναι κενό.")
                    (tof-title-exists "Αυτό το όνομα Δ.Ο.Υ. υπάρχει ήδη")))))



;;; ------------------------------------------------------------
;;; TOF - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/tof/create ("actions/tof/create" :request-type :post)
    ((title string chk-tof-title t))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (insert-dao (make-instance 'tof :title (val title)))
        (see-other (tof :id (tof-id (val title)))))
      (see-other (tof/create :title (raw title)))))

(define-dynamic-page actions/tof/update ("actions/tof/update" :request-type :post)
    ((id    integer chk-tof-id t)
     (title string (chk-tof-title title id) t))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (execute (:update 'tof :set
                          'title (val title)
                          :where (:= 'id (val id))))
        (see-other (tof :id (val id))))
      (see-other (tof/update :id (raw id) :title (raw title)))))

(define-dynamic-page actions/tof/delete ("actions/tof/delete" :request-type :post)
    ((id integer chk-tof-id/ref t))
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'tof (val id)))
        (see-other (tof)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; TOF menu
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
              (thead "" "Ονομασία Δ.Ο.Υ." "" "")
              (:tbody
               (when (eql op 'create)
                 (funcall row-fn nil))
               (iter (for db-row in db-table)
                     (funcall row-fn db-row)))))))



;;; ------------------------------------------------------------
;;; TOF - Pages
;;; ------------------------------------------------------------

(define-dynamic-page tof ("config/tof")
    ((id integer chk-tof-id))
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

(define-dynamic-page tof/create ("config/tof/create")
    ((title string chk-tof-title))
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
                 (with-form (actions/tof/create :title (val* title))
                   (tof-table 'create nil)))
           (footer)))))

(define-dynamic-page tof/update ("config/tof/update")
    ((id    integer chk-tof-id t)
     (title string  (chk-tof-title title id)))
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

(define-dynamic-page tof/delete ("config/tof/delete")
    ((id integer chk-tof-id t))
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
