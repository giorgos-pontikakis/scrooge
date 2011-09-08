(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass project-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id))
   (payload-parameter-names
    :allocation :class
    :initform '(company description location price vat status quote-date start-date end-date notes))
   (filter-parameter-names
    :allocation :class
    :initform '(search status))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))
   (messages
    :allocation :class
    :reader messages
    :initform
    '((description (:project-description-null
                    "Η περιγραφή του έργου πρέπει να μην είναι κενή."
                    :project-description-exists
                    "Υπάρχει ήδη έργο με αυτή την περιγραφή."))
      (company     (:company-title-unknown
                    "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία."
                    :company-title-null
                    "Η επωνυμία της εταιρίας πρέπει να μην είναι κενή"))
      (price       (:non-positive-amount
                    "Το ποσό της τιμής πρέπει να είναι θετικός αριθμός."
                    :parse-error
                    "Το ποσό της τιμής περιέχει άκυρους χαρακτήρες."))
      (vat         (:non-positive-amount
                    "Το ποσό του Φ.Π.Α. πρέπει να είναι θετικός αριθμός."
                    :parse-error
                    "Το ποσό του Φ.Π.Α. περιέχει άκυρους χαρακτήρες."))
      (quote-date  (:parse-error
                    "Η ημερομηνία προσφοράς είναι άκυρη."
                    :quote-date-null
                    "Η ημερομηνία προσφοράς είναι κενή."))
      (start-date  (:parse-error
                    "Η ημερομηνία έναρξης είναι άκυρη."
                    :start-date-null
                    "Η ημερομηνία έναρξης είναι κενή ενώ το έργο είναι σε εξέλιξη."
                    :start-date-nonnull
                    "Η ημερομηνία έναρξης δεν είναι κενή ενώ για το έργο έχει ξεκινήσει."))
      (end-date    (:parse-error
                    "Η ημερομηνία ολοκλήρωσης είναι άκυρη."
                    :end-date-null
                    "Η ημερομηνία ολοκλήρωσης είναι κενή ενώ το έργο έχει ολοκληρωθεί."
                    :end-date-nonnull
                    "Η ημερομηνία ολοκλήρωσης δεν είναι κενή ενώ το έργο δεν έχει ολοκληρωθεί."))))))



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(define-existence-predicate project-id-exists-p project id)
(define-existence-predicate* project-description-exists-p project description id)

(defun chk-project-id (id)
  (if (project-id-exists-p id)
      nil
      :project-id-unknown))

(defun chk-project-description/create (description)
  (cond ((eql :null description) :project-description-null)
        ((project-description-exists-p description) :project-description-exists)
        (t nil)))

(defun chk-project-description/update (description id)
  (cond ((eql :null description) :project-description-null)
        ((project-description-exists-p description id) :project-description-exists)
        (t nil)))

(defun chk-project-description (description)
  (cond ((eql :null description) :project-description-null)
        ((not (project-description-exists-p description)) :project-description-unknown)
        (t nil)))

(defun chk-quote-date (date status)
  (cond ((and (member status (list "quoted" "ongoing" "finished" "archived") :test #'string=)
              (eql date :null))
         :quote-date-null)
        (t
         nil)))

(defun chk-start-date (date status)
  (cond ((and (member status (list "ongoing" "finished" "archived") :test #'string=)
              (eql date :null))
         :start-date-null)
        ((and (not (member status (list "ongoing" "finished" "archived") :test #'string=))
              (not (eql date :null)))
         :start-date-nonnull)
        (t
         nil)))

(defun chk-end-date (date status)
  (cond ((and (member status (list "finished" "archived") :test #'string=)
              (eql date :null))
         :end-date-null)
        ((and (not (member status (list "finished" "archived") :test #'string=))
              (not (eql date :null)))
         :end-date-nonnull)
        (t
         nil)))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun project-menu (id filter &optional disabled)
  (anchor-menu (crud+details-actions-spec (apply #'project :id id filter)
                                          (apply #'project/create filter)
                                          (apply #'project/details :id id filter)
                                          (apply #'project/update :id id filter)
                                          (apply #'project/delete :id id filter))
               :id "project-actions"
               :css-class "hmenu actions"
               :disabled disabled))

(defun project-filters (status search)
  (let ((spec `((nil      ,(project :search search)                    "Όλα")
                (quoted   ,(project :search search :status "quoted")   "Προσφορές")
                (ongoing  ,(project :search search :status "ongoing")  "Σε εξέλιξη")
                (finished ,(project :search search :status "finished") "Ολοκληρωμένα")
                (archived ,(project :search search :status "archived") "Αρχειοθετημένα")
                (canceled ,(project :search search :status "canceled") "Άκυρα"))))
    (with-html
      (:div :id "filters" :class "filters"
            (:p :class "title" "Κατάσταση")
            (navbar spec
                    :id "project-filters"
                    :css-class "vnavbar"
                    :active status
                    :test #'string-equal)))))



;;; ------------------------------------------------------------
;;; Project form
;;; ------------------------------------------------------------

(defclass project-form (crud-form/plist)
  ())


(defmethod display ((form project-form) &key styles)
  (let ((disabled (eql (op form) :view))
        (record (record form)))
    (flet ((label-input-text (name label &optional extra-styles)
             (with-html
               (label name label)
               (input-text name
                           :value (getf record (make-keyword name))
                           :disabled disabled
                           :css-class (conc (getf styles (make-keyword name))
                                            " " extra-styles)))))
      (with-html
        (:div :id "project-data-form" :class "data-form grid_8"
              (:div :class "grid_5 alpha project-data-form-title"
                    (label-input-text 'description "Περιγραφή"))
              (:div :class "grid_2 omega project-data-form-title"
                    (label 'status "Κατάσταση")
                    (dropdown 'status *project-statuses*
                              :selected (or (getf record :status) *default-project-status*)
                              :disabled disabled))
              (:div :class "grid_5 alpha project-data-form-subtitle"
                    (label-input-text 'location "Τοποθεσία")
                    (label-input-text 'company "Εταιρία" "ac-company"))
              (:div :class "grid_4 alpha project-data-form-details"
                    (:fieldset
                     (:legend "Οικονομικά")
                     (:ul (:li (label-input-text 'price "Τιμή"))
                          (:li (label-input-text 'vat "Φ.Π.Α.")))))
              (:div :class "grid_4 omega project-data-form-details"
                    (:fieldset
                     (:legend "Χρονοδιάγραμμα")
                     (:ul (:li (label-input-text 'quote-date "Ημερομηνία προσφοράς" "datepicker"))
                          (:li (label-input-text 'start-date "Ημερομηνία έναρξης" "datepicker"))
                          (:li (label-input-text 'end-date "Ημερομηνία ολοκλήρωσης" "datepicker"))))))
        (:div :id "project-notes" :class "data-form grid_4"
              (:div :class "project-data-form-title"
                    (label 'notes "Σημειώσεις")
                    (:textarea :name 'notes
                               :cols 38 :rows 22 :disabled disabled
                               (str (lisp->html (or (getf record :notes) :null))))))
        (:div :class "grid_8 data-form-buttons"
              (if disabled
                  (cancel-button (cancel-url form)
                                 :body "Επιστροφή στον Κατάλογο Έργων")
                  (progn
                    (ok-button :body (if (eql (op form) :update) "Ανανέωση" "Δημιουργία"))
                    (cancel-button (cancel-url form) :body "Άκυρο"))))))))



;;; ------------------------------------------------------------
;;; Database interface
;;; ------------------------------------------------------------

(defmethod get-record ((type (eql 'project)) id)
  (declare (ignore type))
  (with-db ()
    (query (:select 'project.id (:as 'company.title 'company)
                    'description 'location 'status 'quote-date
                    'start-date 'end-date 'price 'vat 'project.notes
                    :from 'project
                    :left-join 'company
                    :on (:= 'project.company-id 'company.id)
                    :where (:= 'project.id id))
           :plist)))



;;; ------------------------------------------------------------
;;; Project table
;;; ------------------------------------------------------------

;;; table

(defclass project-table (scrooge-table)
  ((header-labels  :initform '("" "Περιγραφή" "Τοποθεσία" "Εταιρία"))
   (paginator      :initform (make-instance 'project-paginator
                                            :id "project-paginator"
                                            :css-class "paginator")))
  (:default-initargs :item-class 'project-row :id "project-table"))

(defmethod get-records ((table project-table))
  (let* ((search (getf (filter table) :search))
         (status (getf (filter table) :status))
         (base-query `(:select project.id (:as company.title company)
                               project.description location
                               project.notes
                               :from project
                               :left-join 'company
                               :on (:= project.company-id company.id)))
         (where-terms nil))
    (when search
      (push `(:or (:ilike project.description ,(ilike search))
                  (:ilike company.title ,(ilike search))
                  (:ilike project.location ,(ilike search))
                  (:ilike project.notes ,(ilike search)))
            where-terms))
    (when status
      (push `(:= project.status ,status)
            where-terms))
    (let* ((composite-query (if (or search status)
                                (append base-query
                                        `(:where (:and ,@where-terms)))
                                base-query))
           (final-query `(:order-by ,composite-query
                                    (:desc
                                     ,(cond ((member status
                                                     (list "quoted" nil) :test #'string=)
                                             'quote-date)
                                            ((string= status "ongoing")
                                             'start-date)
                                            ((member status
                                                     (list "finished" "archived") :test #'string=)
                                             'end-date))))))
      (with-db ()
        (query (sql-compile final-query)
               :plists)))))


;;; rows

(defclass project-row (scrooge-row/plist)
  ())

(defmethod selector ((row project-row) selected-p)
  (simple-selector row selected-p #'project))

(defmethod controls ((row project-row) controls-p)
  (simple-controls row controls-p #'project))

(defmethod payload ((row project-row) enabled-p)
  (let ((record (record row)))
    (mapcar (lambda (name)
              (make-instance 'textbox
                             :name name
                             :value (getf record (make-keyword name))
                             :disabled (not enabled-p)))
            '(description location company))))


;;; paginator

(defclass project-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg project-paginator) start)
  (apply #'project :start start (filter (table pg))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage project-page project ("admin/project")
    ((id     integer chk-project-id)
     (status string)
     (search string)
     (start  integer))
  (with-view-page
    (let* ((filter (params->filter))
           (project-table (make-instance 'project-table
                                         :op :read
                                         :filter filter
                                         :start-index (val start))))
      (with-document ()
        (:head
         (:title "Έργα » Κατάλογος")
         (admin-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'admin)
               (admin-navbar 'project)
               (:div :id "project-window" :class "window grid_10"
                     (:div :class "title" "Έργα » Κατάλογος")
                     (project-menu (val id)
                                   filter
                                   (if (val id)
                                       '(:read)
                                       '(:read :details :update :delete)))
                     (display project-table
                              :key (val id)
                              :start (val start)))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (project :status (val status)) (val search))
                     (project-filters (val status) (val search)))
               (footer)))))))

(defpage project-page project/details ("admin/project/details")
    ((search string)
     (status string)
     (id     integer chk-project-id t))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :view
                                        :record (get-record 'project (val id))
                                        :cancel-url (apply #'project :id (val id) filter))))
      (with-document ()
        (:head
         (:title "Έργο » Λεπτομέρειες")
         (admin-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'admin)
               (admin-navbar 'project)
               (:div :id "project-window" :class "window grid_12"
                     (:p :class "title" "Έργο » Λεπτομέρειες")
                     (project-menu (val id)
                                   filter
                                   '(:details :create)))
               (display project-form :payload (get-record 'project (val id)))))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage project-page project/create ("admin/project/create")
    ((search      string)
     (company     string  chk-company-title*)
     (description string  chk-project-description/create)
     (location    string)
     (price       float chk-amount*)
     (vat         float chk-amount*)
     (status      string)
     (quote-date  date  (chk-quote-date quote-date status))
     (start-date  date  (chk-start-date start-date status))
     (end-date    date  (chk-end-date end-date status))
     (notes       string))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :create
                                        :record nil
                                        :cancel-url (apply #'project filter))))
      (with-document ()
        (:head
         (:title "Έργο » Δημιουργία")
         (admin-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'admin)
               (admin-navbar 'project)
               (:div :id "project-window" :class "window grid_12"
                     (:div :class "title" "Έργο » Δημιουργία")
                     (project-menu nil
                                   filter
                                   '(:details :create :update :delete))
                     (notifications))
               (with-form (actions/project/create :search (val search))
                 (display project-form :payload (params->payload)
                                       :styles (params->styles)))
               (footer)))))))

(defpage project-page actions/project/create ("actions/admin/project/create"
                                              :request-type :post)
    ((search      string)
     (company     string chk-company-title)
     (description string chk-project-description/create)
     (location    string)
     (price       float chk-amount*)
     (vat         float chk-amount*)
     (status      string)
     (quote-date  date  (chk-quote-date quote-date status))
     (start-date  date  (chk-start-date start-date status))
     (end-date    date  (chk-end-date end-date status))
     (notes       string))
  (with-controller-page (project/create)
    (let* ((company-id (company-id (val company)))
           (new-project (make-instance 'project
                                       :company-id company-id
                                       :description (val description)
                                       :location (val location)
                                       :price (val price)
                                       :vat (val vat)
                                       :quote-date (val quote-date)
                                       :start-date (val start-date)
                                       :end-date (val end-date)
                                       :status (val status)
                                       :notes (val notes))))
      (insert-dao new-project)
      (see-other (project :id (id new-project) :status (val status))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage project-page project/update ("admin/project/update")
    ((search      string)
     (id          integer chk-project-id)
     (company     string  chk-company-title)
     (description string  (chk-project-description/update description id))
     (location    string)
     (price       float chk-amount*)
     (vat         float chk-amount*)
     (status      string)
     (quote-date  date  (chk-quote-date quote-date status))
     (start-date  date  (chk-start-date start-date status))
     (end-date    date  (chk-end-date end-date status))
     (notes       string))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :update
                                        :record (get-record 'project (val id))
                                        :cancel-url (apply #'project :id (val id) filter))))
      (with-document ()
        (:head
         (:title "Έργο » Επεξεργασία")
         (admin-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'admin)
               (admin-navbar 'project)
               (:div :id "project-window" :class "window grid_12"
                     (:p :class "title" "Έργο » Επεξεργασία")
                     (project-menu (val id)
                                   filter
                                   '(:create :update))
                     (notifications))
               (with-form (actions/project/update :id (val id) :search (val search))
                 (display project-form :payload (params->payload)
                                       :styles (params->styles)))
               (footer)))))))

(defpage project-page actions/project/update ("actions/admin/project/update"
                                              :request-type :post)
    ((search      string)
     (id          integer chk-project-id)
     (company     string  chk-company-title)
     (description string  (chk-project-description/update description id))
     (location    string)
     (price       float chk-amount*)
     (vat         float chk-amount*)
     (status      string)
     (quote-date  date  (chk-quote-date quote-date status))
     (start-date  date  (chk-start-date start-date status))
     (end-date    date  (chk-end-date end-date status))
     (notes       string))
  (with-controller-page (project/update)
    (let ((company-id (company-id (val company))))
      (execute (:update 'project :set
                        'company-id company-id
                        'description (val description)
                        'location (val location)
                        'price (val price)
                        'vat (val vat)
                        'quote-date (val quote-date)
                        'start-date (val start-date)
                        'end-date (val end-date)
                        'status (val status)
                        'notes (val notes)
                        :where (:= 'id (val id))))
      (see-other (project :id (val id) :status (val status))))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage project-page project/delete ("admin/project/delete")
    ((id     integer chk-project-id t)
     (search string)
     (status string))
  (with-view-page ()
    (let* ((filter (params->filter))
           (project-table (make-instance 'project-table
                                         :op :delete
                                         :filter filter)))
      (with-document ()
        (:head
         (:title "Έργο » Διαγραφή")
         (admin-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'admin)
               (admin-navbar 'project)
               (:div :id "project-window" :class "window grid_10"
                     (:div :class "title" "Έργο » Διαγραφή")
                     (project-menu (val id)
                                   filter
                                   '(:read :delete))
                     (with-form (actions/project/delete :id (val id)
                                                        :search (val search)
                                                        :status (val status))
                       (display project-table
                                :key (val id))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (project :status (val status)) (val search))
                     (project-filters (val status) (val search)))
               (footer)))))))

(defpage project-page actions/project/delete ("actions/admin/project/delete"
                                                   :request-type :post)
    ((id     integer chk-project-id)
     (search string)
     (status string))
  (with-controller-page (project/delete)
    (delete-dao (get-dao 'project (val id)))
    (see-other (project :search (val search) :status (val status)))))
