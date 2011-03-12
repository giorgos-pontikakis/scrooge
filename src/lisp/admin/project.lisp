(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(define-existence-predicate project-id-exists-p project id)
(define-uniqueness-predicate project-description-unique-p project description id)

(defun chk-vat (num)
  (if (or (eql :null num)
          (positive-int-p num))
      nil
      :invalid-vat))

(defun chk-price (num)
  (if (or (eql :null num)
          (positive-int-p num))
      nil
      :invalid-price))

(defun chk-project-id (id)
  (if (project-id-exists-p id)
      nil
      :project-id-unknown))

(defun chk-new-project-description (description &optional id)
  (cond ((eql :null description) :project-description-null)
        ((not (project-description-unique-p description id)) :company-description-exists)
        (t nil)))



;;; ----------------------------------------------------------------------
;;; Actions
;;; ----------------------------------------------------------------------

(define-dynamic-page actions/admin/project/create ("actions/admin/project/create"
                                                   :request-type :post)
    ((search      string)
     (company     string chk-company-title t)
     (description string chk-new-project-description)
     (location    string)
     (price       integer chk-price)
     (vat         integer chk-vat)
     (quote-date  date)
     (start-date  date)
     (end-date    date)
     (status      string)
     (notes       string))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
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
            (see-other (project :id (id new-project) :status (val status)))))
        (see-other (project/create :search (raw search)
                                   :company (raw company)
                                   :description (raw description)
                                   :location (raw location)
                                   :price (raw price)
                                   :vat (raw vat)
                                   :quote-date (raw quote-date)
                                   :start-date (raw start-date)
                                   :end-date (raw end-date)
                                   :status (raw status)
                                   :notes (raw notes))))))

(define-dynamic-page actions/admin/project/update ("actions/admin/project/update"
                                                   :request-type :post)
    ((search      string)
     (id          integer chk-project-id)
     (company     string  chk-company-title)
     (description string  (chk-new-project-description description id))
     (location    string)
     (price       integer chk-price)
     (vat         integer chk-vat)
     (quote-date  date)
     (start-date  date)
     (end-date    date)
     (status      string)
     (notes       string))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
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
            (see-other (project :id (val id) :status (val status)))))
        (see-other (project/update :search (raw search)
                                   :id (raw id)
                                   :company (raw company)
                                   :description (raw description)
                                   :location (raw location)
                                   :price (raw price)
                                   :vat (raw vat)
                                   :quote-date (raw quote-date)
                                   :start-date (raw start-date)
                                   :end-date (raw end-date)
                                   :status (raw status)
                                   :notes (raw notes))))))

(define-dynamic-page actions/admin/project/delete ("actions/admin/project/delete"
                                                   :request-type :post)
    ((id     integer chk-project-id)
     (search string)
     (status string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'project (val id)))
          (see-other (project :search (val search) :status (val status))))
        (see-other (notfound)))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun project-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "project-actions"
                          :style "hnavbar actions"
                          :spec (crud+details-actions-spec (apply #'project :id id filter)
                                                           (apply #'project/create filter)
                                                           (apply #'project/details :id id filter)
                                                           (apply #'project/update :id id filter)
                                                           (apply #'project/details :id id filter)
                                                           (apply #'project/delete :id id filter)))
           :disabled-items disabled-items))

(defun project-notifications ()
  ;; date errors missing, system is supposed to respond with the default (error-type param)
  (notifications
   '((description (:project-description-null "Το όνομα του έργου είναι κενό"
                   :project-description-exists "Υπάρχει ήδη έργο με αυτή την περιγραφή"))
     (company     (:company-title-unknown "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"
                   :company-title-null "Το όνομα της εταιρίας δεν πρέπει να είναι κενό"))
     (price       (:invalid-price  "Η τιμή πρέπει να είναι θετικός αριθμός ή μηδέν"))
     (vat         (:invalid-vat "Ο Φ.Π.Α. πρέπει να είναι θετικός αριθμός ή μηδέν")))))

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
            (display (make-instance 'vertical-navbar
                                    :id "project-filters"
                                    :style "vnavbar"
                                    :spec spec)
                     :active-page-name (intern (string-upcase status)))))))



;;; ------------------------------------------------------------
;;; Database interface
;;; ------------------------------------------------------------

(defun get-project-plist (id)
  (with-db ()
    (query (:select 'project.id (:as 'company.title 'company)
                    'description 'location 'status 'quote-date
                    'start-date 'end-date 'price 'vat 'notes
                    :from 'project
                    :left-join 'company
                    :on (:= 'project.company-id 'company.id)
                    :where (:= 'project.id id))
           :plist)))



;;; ------------------------------------------------------------
;;; Project table
;;; ------------------------------------------------------------

;;; table

(defclass project-table (crud-table)
  ((item-key-field :initform :id)
   (header-labels  :initform '("" "Περιγραφή" "Τοποθεσία" "Εταιρία" "Κατάσταση"))
   (paginator      :initform (make-instance 'paginator
                                            :id "project-paginator"
                                            :style "paginator"
                                            :delta 10
                                            :urlfn #'project)))
  (:default-initargs :item-class 'project-row :id "project-table"))

(defmethod read-records ((table project-table))
  (let* ((search (getf (filter table) :search))
         (status (getf (filter table) :status))
         (base-query `(:select project.id (:as company.title company)
                               project.description location
                               (:as project-status.description status-description)
                               project.notes
                               :from project
                               :left-join 'company
                               :on (:= project.company-id company.id)
                               :left-join project-status
                               :on (:= project-status.id project.status)))
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
                                    ,(cond ((member status
                                                    (list "quoted" nil) :test #'string=)
                                            'quote-date)
                                           ((string= status "ongoing")
                                            'start-date)
                                           ((member status
                                                    (list "finished" "archived") :test #'string=)
                                            'end-date)))))
      (with-db ()
        (query (sql-compile final-query)
               :plists)))))


;;; rows

(defclass project-row (crud-row)
  ())

(defmethod cells ((row project-row) &key start)
  (let* ((id (key row))
         (record (record row))
         (pg (paginator (collection row)))
         (filter (filter (collection row))))
    (list :selector (make-instance 'selector-cell
                                   :states (list
                                            :on (apply #'project
                                                       :start (page-start pg (index row) start)
                                                       filter)
                                            :off (apply #'project :id id filter)))
          :payload (mapcar (lambda (name)
                             (make-instance 'textbox-cell
                                            :name name
                                            :value (getf record (make-keyword name))))
                           '(description location company status-description))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (apply #'project :id id filter))))))



;;; ------------------------------------------------------------
;;; Project - Pages
;;; ------------------------------------------------------------

(define-dynamic-page project ("admin/project")
    ((id     integer chk-project-id)
     (status string)
     (search string)
     (start  integer))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter  (parameters->plist search status))
               (project-table (make-instance 'project-table
                                             :op 'catalogue
                                             :filter filter)))
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
                                           '(catalogue)
                                           '(catalogue details update delete)))
                         (display project-table
                                  :selected-id (val* id)
                                  :start (val* start)))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (project :status (val status)) (val search))
                         (project-filters (val status) (val search)))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page project/create ("admin/project/create")
    ((search      string)
     (company     string  chk-company-title)
     (description string  chk-new-project-description)
     (location    string)
     (price       integer chk-price)
     (vat         integer chk-vat)
     (quote-date  date)
     (start-date  date)
     (end-date    date)
     (status      string)
     (notes       string))
  (with-auth ("configuration")
    (no-cache)
    (let ((filter (parameters->plist search status)))
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
                                   '(details create update delete))
                     (project-notifications))
               (with-form (actions/admin/project/create :search (val search))
                 (project-data-form 'create
                                    :filter filter
                                    :data (plist-union (parameters->plist company
                                                                          description
                                                                          location
                                                                          price
                                                                          vat
                                                                          status
                                                                          quote-date
                                                                          start-date
                                                                          end-date
                                                                          notes)
                                                       (list :quote-date (today)))
                                    :styles (parameters->styles company
                                                                description
                                                                location
                                                                price
                                                                vat
                                                                status
                                                                quote-date
                                                                start-date
                                                                end-date
                                                                notes)))
               (footer)))))))

(define-dynamic-page project/update ("admin/project/update")
    ((search      string)
     (id          integer chk-project-id)
     (company     string  chk-company-title)
     (description string  (chk-new-project-description description id))
     (location    string)
     (price       integer chk-price)
     (vat         integer chk-vat)
     (quote-date  date)
     (start-date  date)
     (end-date    date)
     (status      string)
     (notes       string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((filter (parameters->plist status search)))
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
                                       '(create update))
                         (project-notifications))
                   (with-form (actions/admin/project/update :id (val id) :search (val search))
                     (project-data-form 'update
                                        :id (val id)
                                        :filter filter
                                        :data (plist-union (parameters->plist company
                                                                              description
                                                                              location
                                                                              price
                                                                              vat
                                                                              status
                                                                              quote-date
                                                                              start-date
                                                                              end-date
                                                                              notes)
                                                           (get-project-plist (val id)))
                                        :styles (parameters->styles company
                                                                    description
                                                                    location
                                                                    price
                                                                    vat
                                                                    status
                                                                    quote-date
                                                                    start-date
                                                                    end-date
                                                                    notes)))
                   (footer)))))
        (see-other (error-page)))))

(define-dynamic-page project/details ("admin/project/details")
    ((search string)
     (status string)
     (id     integer chk-project-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((filter (parameters->plist status search)))
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
                                       '(details create)))
                   (project-data-form 'details
                                      :filter filter
                                      :id (val id)
                                      :data (get-project-plist (val id)))))))
        (see-other (notfound)))))

(define-dynamic-page project/delete ("admin/project/delete")
    ((id integer chk-project-id t)
     (search string)
     (status string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (parameters->plist search status))
               (project-table (make-instance 'project-table
                                             :op 'delete
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
                                       '(catalogue delete))
                         (with-form (actions/admin/project/delete :id (val id)
                                                                  :search (val* search)
                                                                  :status (val status))
                           (display project-table
                                    :selected-id (val id))))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (project :status (val status)) (val search))
                         (project-filters (val status) (val search)))
                   (footer)))))
        (see-other (error-page)))))


(defun project-data-form (op &key id data styles filter)
  (let ((disabledp (eql op 'details)))
    (flet ((label+textbox (name label)
             (with-html
               (label name label)
               (textbox name
                        :id (string-downcase name)
                        :value (getf data (make-keyword name))
                        :disabledp disabledp
                        :style (getf styles (make-keyword name))))))
      (with-html
        (:div :id "project-data-form" :class "data-form grid_8"
              (:div :class "grid_5 alpha project-data-form-title"
                    (label+textbox 'description "Περιγραφή"))
              (:div :class "grid_2 omega project-data-form-title"
                    (label 'status "Κατάσταση")
                    (dropdown 'status
                              (with-db ()
                                (query (:select 'description 'id
                                                :from 'project-status)))
                              :selected (or (getf data :status) *default-project-status*)
                              :disabledp disabledp))
              (:div :class "grid_5 alpha project-data-form-subtitle"
                    (label+textbox 'location "Τοποθεσία")
                    (label+textbox 'company "Εταιρία"))
              (:div :class "grid_4 alpha project-data-form-details"
                    (:fieldset
                     (:legend "Οικονομικά")
                     (:ul (:li (label+textbox 'price "Τιμή"))
                          (:li (label+textbox 'vat "Φ.Π.Α.")))))
              (:div :class "grid_4 omega project-data-form-details"
                    (:fieldset
                     (:legend "Χρονοδιάγραμμα")
                     (:ul (:li (label+textbox 'quote-date "Ημερομηνία προσφοράς"))
                          (:li (label+textbox 'start-date "Ημερομηνία έναρξης"))
                          (:li (label+textbox 'end-date "Ημερομηνία ολοκλήρωσης"))))))
        (:div :id "project-notes" :class "data-form grid_4"
              (:div :class "project-data-form-title"
                    (label 'notes "Σημειώσεις")
                    (:textarea :name 'notes
                               :cols 34 :rows 22 :disabled disabledp
                               (str (lisp->html (or (getf data :notes) :null))))))
        (:div :class "grid_8 data-form-buttons"
              (if disabledp
                  (cancel-button (apply #'project :id id filter)
                                 "Επιστροφή στον Κατάλογο Έργων")
                  (progn
                    (ok-button (if (eql op 'update) "Ανανέωση" "Δημιουργία"))
                    (cancel-button (apply #'project :id id filter) "Άκυρο"))))))))
