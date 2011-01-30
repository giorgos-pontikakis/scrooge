(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(define-existence-predicate project-id-exists-p project id)
(define-uniqueness-predicate project-description-unique-p project description id)

(defun positive-int-p (num)
  (and (integerp num)
       (>= num 0)))

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
    ((filter string)
     (company     string chk-company-title t)
     (description string chk-new-project-description)
     (location    string)
     (price       integer chk-price)
     (vat         integer chk-vat)
     (quote-date  string)
     (start-date  string)
     (end-date    string)
     (status      string))
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
                                             :status (val status))))
            (insert-dao new-project)
            (see-other (project :id (id new-project) :filter (val filter)))))
        (see-other (project/create :filter (raw filter)
                                   :company (raw company)
                                   :description (raw description)
                                   :location (raw location)
                                   :price (raw price)
                                   :vat (raw vat)
                                   :quote-date (raw quote-date)
                                   :start-date (raw start-date)
                                   :end-date (raw end-date)
                                   :status (raw status))))))

(define-dynamic-page actions/admin/project/update ("actions/admin/project/update"
                                                   :request-type :post)
    ((filter      string)
     (id          integer chk-project-id)
     (company     string  chk-company-title)
     (description string  (chk-new-project-description description id))
     (location    string)
     (price       integer chk-price)
     (vat         integer chk-vat)
     (quote-date  string)
     (start-date  string)
     (end-date    string)
     (status      string))
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
                              :where (:= 'id (val id))))
            (see-other (project :id (val id) :filter (val filter)))))
        (see-other (project/update :filter (raw filter)
                                   :id (raw id)
                                   :company (raw company)
                                   :description (raw description)
                                   :location (raw location)
                                   :price (raw price)
                                   :vat (raw vat)
                                   :quote-date (raw quote-date)
                                   :start-date (raw start-date)
                                   :end-date (raw end-date)
                                   :status (raw status))))))

(define-dynamic-page actions/admin/project/delete ("actions/admin/project/delete"
                                                   :request-type :post)
    ((id     integer chk-project-id)
     (filter string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'project (val id)))
          (see-other (project :filter (val filter))))
        (see-other (notfound)))))



;;; ------------------------------------------------------------
;;; Project menu
;;; ------------------------------------------------------------

(defun project-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "project-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (crud+details+archive-actions-spec (project :id id
                                                                            :filter filter)
                                                                   (project/details :id id
                                                                                    :filter filter)
                                                                   (project/create :filter filter)
                                                                   (project/update :id id
                                                                                   :filter filter)
                                                                   (project/details :id id
                                                                                    :filter filter)
                                                                   (project/delete :id id
                                                                                   :filter filter)))
           :disabled-items disabled-items))



;;; ------------------------------------------------------------
;;; Database interface
;;; ------------------------------------------------------------

(defun get-project-plists (filter)
  (let* ((base-query `(:select project.id (:as company.title company)
                               project.description location
                               (:as project-status.description status)
                               :from project
                               :left-join 'company
                               :on (:= project.company-id company.id)
                               :left-join project-status
                               :on (:= project-status.id project.status)))
         (composite-query (if filter
                              (append base-query
                                      `(where (:or (:ilike project.description ,(ilike filter))
                                                   (:ilike company.title ,(ilike filter))
                                                   (:ilike project.location ,(ilike filter)))))
                              base-query))
         (final-query `(:order-by ,composite-query project.id start-date)))
    (with-db ()
      (query (sql-compile final-query)
             :plists))))

(defun get-project-plist (id)
  (with-db ()
    (query (:select 'project.id (:as 'company.title 'company)
                    'description 'location 'status
                    'start-date 'end-date 'price 'vat
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
  ((header-labels :initform '("" "Περιγραφή" "Εταιρία" "Τοποθεσία" "Κατάσταση"))
   (paginator     :initform (make-instance 'paginator
                                           :id "project-paginator"
                                           :style "paginator grid_9 alpha"
                                           :delta 10
                                           :urlfn (lambda (filter start)
                                                    (project :filter filter
                                                             :start start))))))


(defmethod read-items ((table project-table))
  (iter (for rec in (get-project-plists (filter table)))
        (for i from 0)
        (collect (make-instance 'project-row
                                :key (getf rec :id)
                                :record rec
                                :collection table
                                :index i))))


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
                                            :on (project :filter filter
                                                         :start (page-start pg (index row) start))
                                            :off (project :filter filter
                                                          :id id)))
          :payload (mapcar (lambda (name)
                             (make-instance 'textbox-cell
                                            :name name
                                            :value (getf record (make-keyword name))))
                           '(description company location status))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (project :id id :filter filter))))))



;;; ------------------------------------------------------------
;;; Notifications
;;; ------------------------------------------------------------

(defun project-notifications (&rest params)
  ;; date errors missing, system is supposed to respond with the default (error-type param)
  (notifications
   '((description (:project-description-null "Το όνομα του έργου είναι κενό"
                   :project-description-exists "Υπάρχει ήδη έργο με αυτή την περιγραφή"))
     (company     (:company-title-unknown "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"
                   :company-title-null "Το όνομα της εταιρίας δεν πρέπει να είναι κενό"))
     (price       (:invalid-price  "Η τιμή πρέπει να είναι θετικός αριθμός ή μηδέν"))
     (vat         (:invalid-vat "Ο Φ.Π.Α. πρέπει να είναι θετικός αριθμός ή μηδέν")))
   params))



;;; ------------------------------------------------------------
;;; Project - Pages
;;; ------------------------------------------------------------

(define-dynamic-page project ("admin/project")
    ((id integer chk-project-id)
     (filter string)
     (start integer))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((project-table (make-instance 'project-table
                                            :op 'catalogue
                                            :filter (val* filter))))
          (with-document ()
            (:head
             (:title "Έργα")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-menu 'project)
                   (:div :id "project-window" :class "window grid_9"
                         (:div :class "title" "Κατάλογος έργων")
                         (project-menu (val id)
                                       (val filter)
                                       (if (val id)
                                           '(catalogue create)
                                           '(catalogue details archive update delete)))
                         (display project-table
                                  :selected-id (val* id)
                                  :start (val* start)))
                   (:div :id "controls" :class "controls grid_3"
                         (filters (project) (val filter)))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page project/create ("admin/project/create")
    ((filter string)
     (company     string chk-company-title)
     (description string chk-new-project-description)
     (location    string)
     (price       integer chk-price)
     (vat         integer chk-vat)
     (quote-date  string)
     (start-date  string)
     (end-date    string)
     (status      string))
  (with-auth ("configuration")
    (no-cache)
    (with-document ()
      (:head
       (:title "Δημιουργία εταιρίας")
       (admin-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'admin)
             (admin-menu 'project)
             (:div :id "project-window" :class "window grid_9"
                   (:div :class "title" "Δημιουργία έργου")
                   (project-menu nil
                                 (val filter)
                                 '(details create update archive delete))
                   (with-form (actions/admin/project/create)
                     (project-data-form 'create
                                        :filter (val filter)
                                        :data (parameters->plist company
                                                                 description
                                                                 location
                                                                 price
                                                                 vat
                                                                 status
                                                                 quote-date
                                                                 start-date
                                                                 end-date)
                                        :styles (parameters->styles company
                                                                    description
                                                                    location
                                                                    price
                                                                    vat
                                                                    status
                                                                    quote-date
                                                                    start-date
                                                                    end-date))))
             (:div :id "controls" :class "controls grid_3"
                   (project-notifications description company price vat))
             (footer))))))

(define-dynamic-page project/update ("admin/project/update")
    ((filter      string)
     (id          integer chk-project-id)
     (company     string  chk-company-title)
     (description string  (chk-new-project-description description id))
     (location    string)
     (price       integer chk-price)
     (vat         integer chk-vat)
     (quote-date  string)
     (start-date  string)
     (end-date    string)
     (status      string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-document ()
          (:head
           (:title "Επεξεργασία έργου")
           (admin-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'admin)
                 (admin-menu 'project)
                 (:div :id "project-window" :class "window grid_9"
                       (:div :class "title" "Επεξεργασία έργου")
                       (project-menu (val id)
                                     (val filter)
                                     '(create update))
                       (with-form (actions/admin/project/update :id (val id))
                         (project-data-form 'update
                                            :id (val id)
                                            :filter (val filter)
                                            :data (plist-union (parameters->plist id
                                                                                  company
                                                                                  description
                                                                                  location
                                                                                  price
                                                                                  vat
                                                                                  status
                                                                                  quote-date
                                                                                  start-date
                                                                                  end-date)
                                                               (get-project-plist (val id)))
                                            :styles (parameters->styles id
                                                                        company
                                                                        description
                                                                        location
                                                                        price
                                                                        vat
                                                                        status
                                                                        quote-date
                                                                        start-date
                                                                        end-date))))
                 (:div :id "controls" :class "controls grid_3"
                       (project-notifications description company price vat))
                 (footer))))
        (see-other (error-page)))))

(define-dynamic-page project/details ("admin/project/details")
    ((filter     string)
     (id         integer chk-project-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-document ()
          (:head
           (:title "Λεπτομέρειες έργου")
           (admin-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'admin)
                 (admin-menu 'project)
                 (:div :id "project-window" :class "window grid_9"
                       (:div :class "title" "Λεπτομέρειες έργου")
                       (project-menu (val id)
                                     (val filter)
                                     '(details create))
                       (with-form (actions/admin/project/update :id (val id))
                         (project-data-form 'details
                                            :filter (val filter)
                                            :id (val id)
                                            :data (get-project-plist (val id)))))
                 (:div :id "controls" :class "controls grid_3"
                       "")
                 (error-page))))
        (see-other (notfound)))))

(define-dynamic-page project/delete ("admin/project/delete")
    ((id integer chk-project-id t)
     (filter string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((project-table (make-instance 'project-table
                                            :op 'delete
                                            :filter (val* filter))))
          (with-document ()
            (:head
             (:title "Διαγραφή έργου")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-menu 'project)
                   (:div :id "project-window" :class "window grid_9"
                         (:div :class "title" "Διαγραφή έργου")
                         (project-menu (val id)
                                       (val filter)
                                       '(catalogue create delete))
                         (with-form (actions/admin/project/delete :id (val id)
                                                                  :filter (val* filter))
                           (display project-table
                                    :selected-id (val id))))
                   (:div :id "controls" :class "controls grid_3"
                         (filters (project) (val filter)))
                   (footer)))))
        (see-other (error-page)))))


(defun project-data-form (op &key filter id data styles)
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
        (:div :id "project-data-form" :class "data-form"
              (:div :class "grid_6 alpha project-data-form-title"
                    (label+textbox 'description "Περιγραφή"))
              (:div :class "grid_3 alpha project-data-form-title"
                    (label 'status "Κατάσταση")
                    (dropdown 'status
                              (with-db ()
                                (query (:select 'description 'id
                                                :from 'project-status)))
                              :selected (or (getf data :status) *default-project-status*)
                              :disabledp disabledp))
              (:div :class "grid_6 alpha project-data-form-subtitle"
                    (label+textbox 'location "Τοποθεσία")
                    (label+textbox 'company "Εταιρία"))
              (:div :class "grid_4 alpha"
                    (:fieldset
                     (:legend "Οικονομικά")
                     (:ul (:li (label+textbox 'price "Τιμή"))
                          (:li (label+textbox 'vat "Φ.Π.Α.")))))
              (:div :class "grid_4 omega"
                    (:fieldset
                     (:legend "Χρονοδιάγραμμα")
                     (:ul (:li (label+textbox 'quote-date "Ημερομηνία προσφοράς"))
                          (:li (label+textbox 'start-date "Ημερομηνία έναρξης"))
                          (:li (label+textbox 'end-date "Ημερομηνία ολοκλήρωσης"))))))
        (unless disabledp
          (htm (:div :id "project-data-form-buttons" :class "grid_9"
                     (ok-button (if (eql op 'update) "Ανανέωση" "Δημιουργία"))
                     (cancel-button (project :id id :filter filter) "Άκυρο"))))))))