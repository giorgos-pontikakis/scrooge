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

(defun chk-date (string)
  nil)



;;; ----------------------------------------------------------------------
;;; Actions
;;; ----------------------------------------------------------------------

(define-dynamic-page actions/project/create ("actions/project/create" :request-type :post)
    ((filter string)
     (company     string chk-company-title t)
     (description string chk-new-project-description)
     (location    string)
     (price       integer chk-price)
     (vat         integer chk-vat)
     (start-date  string chk-date)
     (end-date    string chk-date)
     (status      string))
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
                                           :start-date (val start-date)
                                           :end-date (val end-date)
                                           :status (val status))))
          (insert-dao new-project)
          (see-other (project :id (id new-project)))))
      (see-other (project/create :filter (raw filter)
                                 :company (raw company)
                                 :description (raw description)
                                 :location (raw location)
                                 :price (raw price)
                                 :vat (raw vat)
                                 :start-date (raw start-date)
                                 :end-date (raw end-date)
                                 :status (raw status)))))

(define-dynamic-page actions/project/update ("actions/project/update" :request-type :post)
    ((filter      string)
     (id          integer chk-project-id)
     (company     string  chk-company-title)
     (description string  (chk-new-project-description description id))
     (location    string)
     (price       integer chk-price)
     (vat         integer chk-vat)
     (start-date  string chk-date)
     (end-date    string chk-date)
     (status      string))
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
                                 :start-date (raw start-date)
                                 :end-date (raw end-date)
                                 :status (raw status)))))

(define-dynamic-page actions/project/delete ("actions/project/delete" :request-type :post)
    ((id integer chk-project-id))
  (no-cache)
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'project (val id)))
        (see-other (project)))
      (see-other (notfound))))


;;; ------------------------------------------------------------
;;; Project menu
;;; ------------------------------------------------------------

(defun project-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "project-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (company-actions-spec (project :id id
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
;;; Other areas
;;; ------------------------------------------------------------

(defun project-filters (filter)
  (with-html
    (:div :id "filters"
          (:p :class "title" "Φίλτρα")
          (with-form (project)
            (htm
             (:p :class "search"
                 (textbox 'filter :value filter)
                 (submit (html ()
                           (img "magnifier.png")))))))))

(defun project-notifications (&rest params)
  ;; date errors missing, system is supposed to respond with the default (error-type param)
  (notifications
   '(description ((:project-description-null "Το όνομα του έργου είναι κενό")
                  (:project-description-exists "Υπάρχει ήδη έργο με αυτή την περιγραφή"))
     company     ((:company-title-invalid "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία"))
     price       ((:invalid-price  "Η τιμή πρέπει να είναι θετικός αριθμός ή μηδέν"))
     vat         ((:invalid-vat "Ο Φ.Π.Α. πρέπει να είναι θετικός αριθμός ή μηδέν")))
   params))



;;; ------------------------------------------------------------
;;; Project - Pages
;;; ------------------------------------------------------------

(define-dynamic-page project ("main/project")
    ((id integer chk-project-id)
     (filter string)
     (start integer))
  (no-cache)
  (if (validp id)
      (let ((project-table (make-instance 'project-table
                                          :op 'catalogue
                                          :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Έργα")
           (company-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'main)
                 (main-menu 'project)
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
                       (project-filters (val filter)))
                 (footer)))))
      (see-other (notfound))))

(define-dynamic-page project/create ("project/create")
    ((filter string)
     (company     string chk-company-title)
     (description string chk-new-project-description)
     (location    string)
     (price       integer chk-price)
     (vat         integer chk-vat)
     (start-date  string  chk-date)
     (end-date    string  chk-date)
     (status      string))
  (no-cache)
  (with-document ()
    (:head
     (:title "Δημιουργία εταιρίας")
     (company-headers))
    (:body
     (:div :id "container" :class "container_12"
           (header 'config)
           (main-menu 'project)
           (:div :id "project-window" :class "window grid_9"
                 (:div :class "title" "Δημιουργία έργου")
                 (project-menu nil
                               (val filter)
                               '(details create update archive delete))
                 (with-form (actions/project/create)
                   (project-data-form 'create
                                      :filter (val filter)
                                      :data (parameters->plist company
                                                               description
                                                               location
                                                               price
                                                               vat
                                                               status
                                                               start-date
                                                               end-date)
                                      :styles (parameters->styles company
                                                                  description
                                                                  location
                                                                  price
                                                                  vat
                                                                  status
                                                                  start-date
                                                                  end-date))))
           (:div :id "controls" :class "controls grid_3"
                 (project-notifications description company price vat))
           (footer)))))

(define-dynamic-page project/update ("project/update")
    ((filter      string)
     (id          integer chk-project-id)
     (company     string  chk-company-title)
     (description string  (chk-new-project-description description id))
     (location    string)
     (price       integer chk-price)
     (vat         integer chk-vat)
     (start-date  string chk-date)
     (end-date    string chk-date)
     (status      string))
  (no-cache)
  (if (validp id)
      (with-document ()
        (:head
         (:title "Επεξεργασία έργου")
         (company-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (main-menu 'project)
               (:div :id "project-window" :class "window grid_9"
                     (:div :class "title" "Επεξεργασία έργου")
                     (project-menu (val id)
                                   (val filter)
                                   '(create update))
                     (with-form (actions/project/update :id (val id))
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
                                                                      start-date
                                                                      end-date))))
               (:div :id "controls" :class "controls grid_3"
                     (project-notifications description company price vat))
               (footer))))
      (see-other (error-page))))

(define-dynamic-page project/details ("project/details")
    ((filter     string)
     (id         integer chk-project-id t))
  (no-cache)
  (if (validp id)
      (with-document ()
        (:head
         (:title "Επεξεργασία εταιρίας")
         (company-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (main-menu 'project)
               (:div :id "project-window" :class "window grid_9"
                     (:div :class "title" "Λεπτομέρειες εταιρίας")
                     (project-menu (val id)
                                   (val filter)
                                   '(details create))
                     (with-form (actions/project/update :id (val id))
                       (project-data-form 'details
                                          :filter (val filter)
                                          :id (val id)
                                          :data (get-project-plist (val id)))))
               (:div :id "controls" :class "controls grid_3"
                     "")
               (error-page))))
      (see-other (notfound))))

(define-dynamic-page project/delete ("project/delete")
    ((id integer chk-project-id t)
     (filter string))
  (no-cache)
  (if (validp id)
      (let ((project-table (make-instance 'project-table
                                          :op 'delete
                                          :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Διαγραφή έργου")
           (company-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (main-menu 'project)
                 (:div :id "project-window" :class "window grid_9"
                       (:div :class "title" "Διαγραφή έργου")
                       (project-menu (val id)
                                     (val filter)
                                     '(catalogue create delete))
                       (with-form (actions/project/delete :id (val id)
                                                          :filter (val* filter))
                         (display project-table
                                  :selected-id (val id))))
                 (:div :id "controls" :class "controls grid_3"
                       (project-filters (val filter)))
                 (footer)))))
      (see-other (error-page))))


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
              (:div :id "project-description" :class "grid_9 alpha"
                    (label+textbox 'description "Περιγραφή")
                    (label+textbox 'company "Εταιρία")
                    (label+textbox 'location "Τοποθεσία")
                    (label+textbox 'price "Τιμή")
                    (label+textbox 'vat "Φ.Π.Α.")
                    (label+textbox 'start-date "Ημερομηνία έναρξης")
                    (label+textbox 'end-date "Ημερομηνία ολοκλήρωσης")
                    (label 'status "Κατάσταση")
                    (dropdown 'status
                              (with-db ()
                                (query (:select 'description 'id
                                                :from 'project-status)))
                              :selected (or (getf data :status) *default-project-status*))))
        (unless disabledp
          (htm (:div :id "project-data-form-buttons" :class "grid_9"
                     (ok-button (if (eql op 'update) "Ανανέωση" "Δημιουργία"))
                     (cancel-button (project :id id :filter filter) "Άκυρο"))))))))
