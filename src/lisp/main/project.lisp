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
  (or (positive-int-p num)
      :invalid-vat))

(defun chk-price (num)
  (or (positive-int-p num)
      :invalid-price))

(defun chk-project-id (num)
  (or (project-id-exists-p id)
      (:project-id-unknown)))

(defun chk-new-project-description (text)
  (cond ((eql :null text) :project-description-null)
        ((not (project-description-unique-p description id)) :company-description-exists)
        (t nil)))

;;; ----------------------------------------------------------------------
;;; Actions
;;; ----------------------------------------------------------------------

(define-dynamic-page actions/project/create ("actions/project/create" :request-type :post)
    ((filter string)
     (company     string chk-company-title)
     (description string (chk-new-project-description description id))
     (location    string)
     (price       integer chk-price)
     (vat         integer chk-vat)
     (start-date  date)
     (end-date    date)
     (status      string))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (let* ((company-id (company-id company))
               (new-project (make-instance 'project
                                           :company-id (val company-id)
                                           :description (val description)
                                           :location (val location)
                                           :price (val price)
                                           :vat (val vat)
                                           :start-date (val start-date)
                                           :end-date (val end-date)
                                           :status (val status))))
          (insert-dao new-project)
          (see-other (project :id (id new-project)))))
      (see-other (project/create :company (raw company)
                                 :description (raw description)
                                 :location (raw location)
                                 :price (raw price)
                                 :vat (raw vat)
                                 :start (raw-date) start-date
                                 :end (raw-date) end-date
                                 :status (raw status)))))

(define-dynamic-page actions/project/update ((id          integer chk-project-id)
                                             (company     string  chk-company-title)
                                             (description string)
                                             (location    string)
                                             (price       integer chk-price)
                                             (vat         integer chk-vat)
                                             (start-date  date)
                                             (end-date    date)
                                             (status      string))
    ("actions/project/update" :request-type :post)
  (no-cache)
  (if (every #'validp params)
      (with-db ()
        (let ((company-id (company-id company)))
          (execute (:update 'project :set
                            'company-id (val company-id)
                            'description (val description)
                            'location (val location)
                            'price (val price)
                            'vat (val vat)
                            'start-date (val start-date)
                            'end-date (val end-date)
                            'status (val status)
                            :where (:= 'id (val id))))
          (see-other (projects :id (val id)))))
      (see-other (project/update :company (raw company)
                                 :description (raw description)
                                 :location (raw location)
                                 :price (raw price)
                                 :vat (raw vat)
                                 :start (raw-date) start-date
                                 :end (raw-date) end-date
                                 :status (raw status)))))

(define-dynamic-page actions/project/delete ((id integer chk-project-id))
    ("actions/project/delete" :request-type :post)
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
  (let* ((base-query `(:select project.id company.title
                               description location status
                               :from project
                               :left-join 'company
                               :on (:= project.company-id company.id)))
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
    (query (:select 'project.id 'company.title
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
  ((header-labels :initform '("" "α/α" "Εταιρία" "Περιγραφή" "Θέση" "Κατάσταση"))
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
                           '(title description location status))
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
     (start-date  date)
     (end-date    date)
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
                                      :data (parameters->plist id
                                                               company
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
                 (project-notifications title tof tin city pobox zipcode))
           (footer)))))

(define-dynamic-page project/update ("project/update")
    ((filter      string)
     (id          integer chk-project-id)
     (company     string  chk-company-title)
     (description string  (chk-new-project-description description id))
     (location    string)
     (price       integer chk-price)
     (vat         integer chk-vat)
     (start-date  date)
     (end-date    date)
     (status      string))
  (no-cache)
  (if (validp id)
      (with-document ()
        (:head
         (:title "Επεξεργασία έργου")
         (project-headers))
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
                                          :styles (parameters->styles company
                                                                      description
                                                                      location
                                                                      price
                                                                      vat
                                                                      status
                                                                      start-date
                                                                      end-date))))
               (:div :id "controls" :class "controls grid_3"
                     (project-notifications title tof tin city pobox zipcode))
               (footer))))
      (see-other (error-page))))

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












;; ;;; Snippets

;; (define-navbar projects-navbar () (:id "subnavbar" :ul-style "hmenu")
;;   (all      (projects) (:img :src (url "img/table.png")))
;;   (quoted   (projects) "Δόθηκε προσφορά")
;;   (ongoing  (projects) "Σε εξέλιξη")
;;   (finished (projects) "Ολοκληρώθηκε"))

;; (defun project-defaults (id)
;;   (with-db ()
;;     (query (:select 'description 'location 'price 'start-date 'end-date 'status 'vat
;;                     :from 'project
;;                     :inner-join 'company
;;                     :on (:= 'project.company-.id 'company.id)
;;                     :where (:= 'project.id id))
;;            :row)))

;; (define-errorbar project-errorbar ()
;;   (company    "Άκυρο όνομα εταιρίας")
;;   (price      "Η τιμή πρέπει να είναι μεγαλύτερη από το μηδέν")
;;   (start-date "Μη αποδεκτή ημερομηνία")
;;   (end-date   "Μη αποδεκτή ημερομηνία")
;;   (vat        "Ο Φ.Π.Α. πρέπει να είναι θετικός αριθμός ή μηδέν"))

;; (defun projects-table (active-id)
;;   (with-db ()
;;     (let ((projects (query (:select 'id 'title 'location 'price 'status
;;                                     :from 'project)))
;;           (header '("" "Περιγραφή" "Τοποθεσία" "Τιμή" "Κατάσταση")))
;;       (with-html
;;         (:table :id "projects-table" :class "forms-in-row"
;;                 (:thead
;;                  (:tr (iter (for label in header)
;;                             (htm (:th (str label))))))
;;                 (:tbody
;;                  (iter (for (id title location price status) in projects)
;;                        (let ((activep (and active-id (= active-id id))))
;;                          (htm
;;                           (:tr :class (if activep "active" nil)
;;                                (:td :class "select"
;;                                     (:a :href (projects :id id)
;;                                         (:img :src (url (if activep
;;                                                             "img/bullet_red.png"
;;                                                             "img/bullet_blue.png")))))
;;                                (:td (:a :href (project/view :id id)
;;                                         (str (lisp-to-html title))))
;;                                (:td (:p (str (lisp-to-html location))))
;;                                (:td (:p (str (lisp-to-html price))))
;;                                (:td (:p (str (lisp-to-html status))))))))))))))

;; (define-menu project-menu (id) ()
;;   (:create (lambda ()
;;              (with-html
;;                (:li (:a :href (project/create)
;;                         (:img :src (url "img/add.png")) "Δημιουργία")))))
;;   (:view (lambda ()
;;            (if id
;;                (with-html
;;                  (:li (:a :href (project/view :id id)
;;                           (:img :src (url "img/magnifier.png")) "Προβολή")))
;;                nil)))
;;   (:edit (lambda ()
;;            (if id
;;                (with-html
;;                  (:li (:a :href (project/update :id id)
;;                           (:img :src (url "img/pencil.png")) "Επεξεργασία")))
;;                nil)))
;;   (:delete (lambda ()
;;              (if id
;;                  (with-html
;;                    (:li (:a :href (project/delete :id id)
;;                             (:img :src (url "img/delete.png")) "Διαγραφή")))
;;                  nil))))

;; (defun project-data-form (&key params defaults readonlyp)
;;   (bind (((description location company status start-date end-date)
;;           (cond ((null params) defaults)
;;                 ((null defaults) (mapcar #'val* params))
;;                 (t (mapcar (lambda (param default)
;;                              (cond ((not (suppliedp param)) default)
;;                                    ((validp param) (val param))
;;                                    (t (raw param))))
;;                            params defaults))))
;;          ((description% location% company% status% start-date% end-date%)
;;           (if params
;;               (mapcar #'(lambda (p) (if (validp p) nil "attention")) params)
;;               (make-list (length defaults)))))
;;     (with-html
;;       (:fieldset
;;        (:legend "")
;;        (with-table2 (:style "formtable tax") ("tax-legend-col" "tax-field-col")
;;          ((label 'description "Περιγραφή:")
;;           (textbox 'description
;;                    :value description
;;                    :readonlyp readonlyp
;;                    :style description%))
;;          ((label 'location "Τοποθεσία:")
;;           (textbox 'location
;;                    :value location
;;                    :readonlyp readonlyp
;;                    :style location%))
;;          ((label 'company "")
;;           (textbox 'company
;;                    :value company
;;                    :readonlyp readonlyp
;;                    :style company%))))
;;       (:fieldset
;;        (:legend "")
;;        (with-table2 (:style "formtable status") ("addr-legend-col" "addr-field-col")
;;          ((label 'status "Κατάσταση:")
;;           (textbox 'status
;;                    :value status
;;                    :readonlyp readonlyp
;;                    :style status%))
;;          ((label 'start-date "Ημερομηνία έναρξης:")
;;           (textbox 'start-date
;;                    :value start-date
;;                    :readonlyp readonlyp
;;                    :style start-date%))
;;          ((label 'end-date "Ημερομηνία ολοκλήρωσης:")
;;           (textbox 'end-date
;;                    :value end-date
;;                    :readonlyp readonlyp
;;                    :style end-date%)))))))

;; (defun project-data-view (id defaults)
;;   (with-html
;;     (:div :id "project-data" :class "window"
;;           (project-menu id :edit :delete)
;;           (:h2 "Στοιχεία έργου")
;;           (project-data-form :defaults defaults :readonlyp t))))

;; (defun project-data-update (id params defaults)
;;   (with-html
;;     (:div :id "project-data" :class "window"
;;           (project-menu (val id) :view :delete)
;;           (:h2 "Επεξεργασία έργου")
;;           (with-form (actions/project/update :id (val id))
;;             (project-data-form :params params :defaults defaults)
;;             (:ul :class "prompt hmenu"
;;                  (:li (submit "Ενημέρωση"))
;;                  (:li (:a :href (project/view :id (val id)) "Ακύρωση")))))))

;; (defun project-data-delete (id defaults)
;;   (with-html
;;     (:div :id "project-data" :class "window"
;;           (project-menu id  :view :edit)
;;           (:h2 "Διαγραφή έργου")
;;           (with-form (actions/project/delete :id id)
;;             (project-data-form :defaults defaults :readonlyp t)
;;             (:ul :class "prompt hmenu"
;;                  (:li (submit "Διαγραφή"))
;;                  (:li (:a :href (project/view :id id) "Ακύρωση")))))))

;; ;;; Pages

;; (define-dynamic-page projects ((id integer #'valid-project-id-p)) ("projects")
;;   (no-cache)
;;   (if (validp id)
;;       (with-parameter-rebinding #'val
;;         (with-page ()
;;           (:head
;;            (:title "Έργα")
;;            (head-css-std))
;;           (:body
;;            (:div :id "header"
;;                  (logo)
;;                  (primary-navbar 'projects)
;;                  (projects-navbar 'all))
;;            (:div :id "body"
;;                  (:div :id "msg" :class "message"
;;                        (:h2 "Κατάλογος Έργων"))
;;                  (:div :id "companies" :class "window"
;;                        (project-menu id :create :view :edit :delete)
;;                        (projects-table id))
;;                  (footer)))))
;;       (see-other (project/notfound))))

;; (define-dynamic-page project/create ((company     string  #'valid-company-p)
;;                                      (description string)
;;                                      (location    string)
;;                                      (price       integer #'positive-p)
;;                                      (start-date  date)
;;                                      (end-date    date)
;;                                      (status      string)
;;                                      (vat         integer #'positive-p))
;;     ("project/create")
;;   (no-cache)
;;   (with-parameter-list params
;;     (with-page ()
;;       (:head
;;        (:title "Εισαγωγή έργου")
;;        (head-css-std)
;;        (head-js-std))
;;       (:body
;;        (:div :id "header"
;;              (logo)
;;              (primary-navbar 'projects))
;;        (:div :id "body"
;;              (:div :id "msg"
;;                    (:h2 "Εισαγωγή έργου"))
;;              (project-errorbar company price start-date end-date vat)
;;              (:div :id "content" :class "window"
;;                    (with-form (actions/project/create)
;;                      (project-data-form :params params)
;;                      (:ul :class "prompt hmenu"
;;                           (:li (submit "Δημιουργία"))
;;                           (:li (:a :href (projects) "Ακύρωση")))))
;;              (footer))))))

;; (define-dynamic-page project/view ((id integer #'valid-project-id-p))
;;     ("project/view")
;;   (no-cache)
;;   (if (validp id)
;;       (let ((defaults (project-defaults (val id))))
;;         (with-page ()
;;           (:head
;;            (:title "Έργο: " (str (getf defaults 'title)))
;;            (head-css-std))
;;           (:body
;;            (:div :id "header"
;;                  (logo)
;;                  (primary-navbar 'projects)
;;                  #|(project-navbar 'overview (val id))|#)
;;            (:div :id "body"
;;                  (project-data-view (val id) defaults)
;;                  (footer)))))
;;       (see-other (project/notfound))))

;; (define-dynamic-page project/update ((id          integer #'valid-project-id-p)
;;                                      (company     string  #'valid-company-p)
;;                                      (description string)
;;                                      (location    string)
;;                                      (price       integer #'positive-p)
;;                                      (start-date  date)
;;                                      (end-date    date)
;;                                      (status      string)
;;                                      (vat         integer #'positive-p))
;;     ("project/update")
;;   (no-cache)
;;   (if (validp id)
;;       (with-parameter-list params
;;         (let ((defaults (project-defaults (val id))))
;;           (with-page ()
;;             (:head
;;              (:title "Επεξεργασία έργρου: " (str (getf defaults 'project)))
;;              (head-css-std)
;;              (head-js-std))
;;             (:body
;;              (:div :id "header"
;;                    (logo)
;;                    (primary-navbar 'projects)
;;                    #|(project-navbar 'overview (val id))|#)
;;              (:div :id "body"
;;                    (project-errorbar company price start-date end-date vat)
;;                    (project-data-update id (rest params) defaults)
;;                    (footer))))))
;;       (see-other (project/notfound))))

;; (define-dynamic-page project/delete ((id integer #'valid-project-id-p))
;;     ("project/delete")
;;   (no-cache)
;;   (if (validp id)
;;       (with-parameter-rebinding #'val
;;         (let ((defaults (project-defaults id)))
;;           (with-page ()
;;             (:head
;;              (:title "Διαγραφή έργου:" (str (getf defaults 'title)))
;;              (head-css-std))
;;             (:body
;;              (:div :id "header"
;;                    (logo)
;;                    (primary-navbar 'projects)
;;                    #|(project-navbar 'overview id)|#)
;;              (:div :id "body"
;;                    (project-data-delete id defaults)
;;                    (contact-data-form id :view)
;;                    (footer))))))
;;       (see-other (project/notfound))))

;; (define-dynamic-page project/notfound () ("project/notfound")
;;   (no-cache)
;;   (with-page ()
;;     (:head
;;      (:title "Άγνωστο έργο")
;;      (head-css-std))
;;     (:body
;;      (:div :id "header"
;;            (logo)
;;            (primary-navbar 'projects))
;;      (:div :id "body"
;;            (:div :id "content" :class "window"
;;                  (:p "Το έργο που προσπαθείτε να προσπελάσετε δεν υπάρχει.")
;;                  (:p "Επιστρέψτε στο μενού των έργων και προσπαθήστε ξανά."))))))
