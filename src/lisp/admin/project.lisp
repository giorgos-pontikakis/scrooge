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
            (see-other (project :id (id new-project) :search (val search)))))
        (see-other (project/create :search (raw search)
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
    ((search      string)
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
            (see-other (project :id (val id) :search (val search)))))
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
                                   :status (raw status))))))

(define-dynamic-page actions/admin/project/delete ("actions/admin/project/delete"
                                                   :request-type :post)
    ((id     integer chk-project-id)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'project (val id)))
          (see-other (project :search (val search))))
        (see-other (notfound)))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun project-menu (id search &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "project-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (crud+details+archive-actions-spec (project :id id
                                                                            :search search)
                                                                   (project/create :search search)
                                                                   (project/details :id id
                                                                                    :search search)
                                                                   (project/update :id id
                                                                                   :search search)
                                                                   (project/details :id id
                                                                                    :search search)
                                                                   (project/delete :id id
                                                                                   :search search)))
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


;;; ------------------------------------------------------------
;;; Database interface
;;; ------------------------------------------------------------

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
                                           :urlfn (lambda (search start)
                                                    (project :search search
                                                             :start start)))))
  (:default-initargs :item-class 'project-row))

(defmethod read-records ((table project-table))
  (let* ((search (filter table))
         (base-query `(:select project.id (:as company.title company)
                               project.description location
                               (:as project-status.description status)
                               :from project
                               :left-join 'company
                               :on (:= project.company-id company.id)
                               :left-join project-status
                               :on (:= project-status.id project.status)))
         (composite-query (if search
                              (append base-query
                                      `(:where (:or (:ilike project.description ,(ilike search))
                                                    (:ilike company.title ,(ilike search))
                                                    (:ilike project.location ,(ilike search)))))
                              base-query))
         (final-query `(:order-by ,composite-query project.id start-date)))
    (with-db ()
      (query (sql-compile final-query)
             :plists))))


;;; rows

(defclass project-row (crud-row)
  ())

(defmethod cells ((row project-row) &key start)
  (let* ((id (key row))
         (record (record row))
         (pg (paginator (collection row)))
         (search (filter (collection row))))
    (list :selector (make-instance 'selector-cell
                                   :states (list
                                            :on (project :search search
                                                         :start (page-start pg (index row) start))
                                            :off (project :search search
                                                          :id id)))
          :payload (mapcar (lambda (name)
                             (make-instance 'textbox-cell
                                            :name name
                                            :value (getf record (make-keyword name))))
                           '(description company location status))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (project :id id :search search))))))



;;; ------------------------------------------------------------
;;; Project - Pages
;;; ------------------------------------------------------------

(define-dynamic-page project ("admin/project")
    ((id integer chk-project-id)
     (search string)
     (start integer))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((project-table (make-instance 'project-table
                                            :op 'catalogue
                                            :filter (val* search))))
          (with-document ()
            (:head
             (:title "Έργα")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-navbar 'project)
                   (:div :id "project-window" :class "window grid_9"
                         (:div :class "title" "Κατάλογος έργων")
                         (project-menu (val id)
                                       (val search)
                                       (if (val id)
                                           '(catalogue)
                                           '(catalogue details archive update delete)))
                         (display project-table
                                  :selected-id (val* id)
                                  :start (val* start)))
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
                         (searchbox (project) (val search)))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page project/create ("admin/project/create")
    ((search string)
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
             (admin-navbar 'project)
             (:div :id "project-window" :class "window grid_9"
                   (:div :class "title" "Δημιουργία έργου")
                   (project-menu nil
                                 (val search)
                                 '(details create update archive delete))
                   (with-form (actions/admin/project/create)
                     (project-data-form 'create
                                        :search (val search)
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
             (:div :id "sidebar" :class "sidebar grid_3"
                   (project-notifications))
             (footer))))))

(define-dynamic-page project/update ("admin/project/update")
    ((search      string)
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
                 (admin-navbar 'project)
                 (:div :id "project-window" :class "window grid_9"
                       (:div :class "title" "Επεξεργασία έργου")
                       (project-menu (val id)
                                     (val search)
                                     '(create update))
                       (with-form (actions/admin/project/update :id (val id))
                         (project-data-form 'update
                                            :id (val id)
                                            :search (val search)
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
                 (:div :id "sidebar" :class "sidebar grid_3"
                       (project-notifications))
                 (footer))))
        (see-other (error-page)))))

(define-dynamic-page project/details ("admin/project/details")
    ((search     string)
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
                 (admin-navbar 'project)
                 (:div :id "project-window" :class "window grid_9"
                       (:div :class "title" "Λεπτομέρειες έργου")
                       (project-menu (val id)
                                     (val search)
                                     '(details create))
                       (with-form (actions/admin/project/update :id (val id))
                         (project-data-form 'details
                                            :search (val search)
                                            :id (val id)
                                            :data (get-project-plist (val id)))))
                 (:div :id "sidebar" :class "sidebar grid_3"
                       "")
                 (error-page))))
        (see-other (notfound)))))

(define-dynamic-page project/delete ("admin/project/delete")
    ((id integer chk-project-id t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((project-table (make-instance 'project-table
                                            :op 'delete
                                            :filter (val* search))))
          (with-document ()
            (:head
             (:title "Διαγραφή έργου")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-navbar 'project)
                   (:div :id "project-window" :class "window grid_9"
                         (:div :class "title" "Διαγραφή έργου")
                         (project-menu (val id)
                                       (val search)
                                       '(catalogue delete))
                         (with-form (actions/admin/project/delete :id (val id)
                                                                  :search (val* search))
                           (display project-table
                                    :selected-id (val id))))
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
                         (searchbox (project) (val search)))
                   (footer)))))
        (see-other (error-page)))))


(defun project-data-form (op &key search id data styles)
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
        (:div :class "grid_9 data-form-buttons"
              (if disabledp
                  (cancel-button (project :id id :search search)
                                 "Επιστροφή στον Κατάλογο Έργων")
                  (progn
                    (ok-button (if (eql op 'update) "Ανανέωση" "Δημιουργία"))
                    (cancel-button (project :id id :search search) "Άκυρο"))))))))
