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
    :initform '(company description location price vat state quote-date start-date end-date notes))
   (filter-parameter-names
    :allocation :class
    :initform '(search cstate))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))
   (messages
    :allocation :class
    :reader messages
    :initform
    '((description (:project-description-null
                    "Η περιγραφή του έργου είναι κενή."
                    :project-description-exists
                    "Υπάρχει ήδη έργο με αυτή την περιγραφή."))
      (company     (:company-title-unknown
                    "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία."
                    :company-title-null
                    "Η επωνυμία της εταιρίας είναι κενή"))
      (price       (:non-positive-amount
                    "Το ποσό της τιμής δεν είναι θετικός αριθμός."
                    :parse-error
                    "Το ποσό της τιμής περιέχει άκυρους χαρακτήρες."))
      (vat         (:non-positive-amount
                    "Το ποσό του Φ.Π.Α. δεν είναι θετικός αριθμός."
                    :parse-error
                    "Το ποσό του Φ.Π.Α. περιέχει άκυρους χαρακτήρες."))
      (quote-date  (:parse-error
                    "Η ημερομηνία προσφοράς είναι άκυρη."
                    :quote-date-null
                    "Δεν καταχωρήθηκε ημερομηνία προσφοράς."))
      (start-date  (:parse-error
                    "Η ημερομηνία έναρξης είναι άκυρη."
                    :start-date-null
                    "Δεν καταχωρήθηκε ημερομηνία έναρξης ενώ το έργο είναι σε εξέλιξη."
                    :start-date-nonnull
                    "Καταχωρήθηκε ημερομηνία έναρξης ενώ το έργο δεν είναι σε εξέλιξη."))
      (end-date    (:parse-error
                    "Η ημερομηνία ολοκλήρωσης είναι άκυρη."
                    :end-date-null
                    "Δεν καταχωρήθηκε ημερομηνία ολοκλήρωσης ενώ το έργο έχει ολοκληρωθεί."
                    :end-date-nonnull
                    "Καταχωρήθηκε ημερομηνία ολοκλήρωσης ενώ το έργο δεν έχει ολοκληρωθεί."))))))



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(define-existence-predicate project-id-exists-p project id)
(define-existence-predicate bill-id-exists-p bill id)
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

(defun chk-quote-date (date state)
  (cond ((and (member state (list "quoted" "ongoing" "finished" "archived") :test #'string=)
              (eql date :null))
         :quote-date-null)
        (t
         nil)))

(defun chk-start-date (date state)
  (cond ((and (member state (list "ongoing" "finished" "archived") :test #'string=)
              (eql date :null))
         :start-date-null)
        ((and (not (member state (list "ongoing" "finished" "archived") :test #'string=))
              (not (eql date :null)))
         :start-date-nonnull)
        (t
         nil)))

(defun chk-end-date (date state)
  (cond ((and (member state (list "finished" "archived") :test #'string=)
              (eql date :null))
         :end-date-null)
        ((and (not (member state (list "finished" "archived") :test #'string=))
              (not (eql date :null)))
         :end-date-nonnull)
        (t
         nil)))

(defun chk-bill-id (project-id bill-id)
  (if (and (project-id-exists-p project-id)
           (bill-id-exists-p bill-id))
      nil
      :bill-id-invalid))


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

(defun project-filters (cstate search)
  (let ((spec `((nil      ,(project :search search)                    "Όλα")
                (quoted   ,(project :search search :cstate "quoted")   "Προσφορές")
                (ongoing  ,(project :search search :cstate "ongoing")  "Σε εξέλιξη")
                (finished ,(project :search search :cstate "finished") "Ολοκληρωμένα")
                (archived ,(project :search search :cstate "archived") "Αρχειοθετημένα")
                (canceled ,(project :search search :cstate "canceled") "Άκυρα"))))
    (with-html
      (:div :id "filters" :class "filters"
            (:p :class "title" "Κατάσταση")
            (navbar spec
                    :id "project-filters"
                    :css-class "vnavbar"
                    :active cstate
                    :test #'string-equal)))))



;;; ------------------------------------------------------------
;;; Project form
;;; ------------------------------------------------------------

(defclass project-form (crud-form/plist)
  ())


(defmethod display ((form project-form) &key styles)
  (let* ((disabled (eql (op form) :read))
         (record (record form))
         (lit (label-input-text disabled record styles)))
    (with-html
      (:div :class "data-form project-form"
            (:div :class "data-form-title"
                  (display lit 'description "Περιγραφή")
                  (display lit 'company "Εταιρία" "ac-company"))
            (:div :class "grid_3 alpha project-form-details"
                  (:fieldset
                   (:legend "Οικονομικά")
                   (:ul
                    (:li (label 'state "Κατάσταση")
                         (dropdown 'state *project-states*
                                   :selected (or (getf record :state)
                                                 *default-project-state*)
                                   :disabled disabled))
                    (:li (display lit 'price "Τιμή"))
                    (:li (display lit 'vat "Φ.Π.Α.")))))
            (:div :class "grid_3 omega project-form-details"
                  (:fieldset
                   (:legend "Χρονοδιάγραμμα")
                   (:ul
                    (:li (display lit 'quote-date "Ημερομηνία προσφοράς" "datepicker"))
                    (:li (display lit 'start-date "Ημερομηνία έναρξης" "datepicker"))
                    (:li (display lit 'end-date "Ημερομηνία ολοκλήρωσης" "datepicker")))))
            (:div :class "clear" "")
            (:div :id "project-notes"
                  (label 'notes "Σημειώσεις")
                  (:textarea :name 'notes :disabled disabled
                             (str (lisp->html (or (getf record :notes) :null)))))
            (:div :class "data-form-buttons"
                  (if disabled
                      (cancel-button (cancel-url form)
                                     :body "Επιστροφή στον Κατάλογο Έργων")
                      (progn
                        (ok-button :body (if (eql (op form) :update) "Ανανέωση" "Δημιουργία"))
                        (cancel-button (cancel-url form) :body "Άκυρο"))))))))

(defmethod get-record ((type (eql 'project)) id)
  (declare (ignore type))
  (with-db ()
    (query (:select 'project.id (:as 'company.title 'company)
                    'description 'location 'state 'quote-date
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
  ((header-labels  :initform '("" "Περιγραφή" "Εταιρία" "Τιμή"))
   (paginator      :initform (make-instance 'project-paginator
                                            :id "project-paginator"
                                            :css-class "paginator")))
  (:default-initargs :item-class 'project-row :id "project-table"))

(defmethod get-records ((table project-table))
  (let* ((search (getf (filter table) :search))
         (cstate (getf (filter table) :cstate))
         (base-query `(:select project.id project.description project.notes price
                               (:as company.id company-id) (:as company.title company)
                               :from project
                               :left-join 'company
                               :on (:= project.company-id company.id)))
         (where-terms nil))
    (when search
      (push `(:or (:ilike project.description ,(ilike search))
                  (:ilike company.title ,(ilike search))
                  (:ilike project.notes ,(ilike search)))
            where-terms))
    (when cstate
      (push `(:= project.state ,cstate)
            where-terms))
    (let* ((composite-query (if (or search cstate)
                                (append base-query
                                        `(:where (:and ,@where-terms)))
                                base-query))
           (final-query `(:order-by ,composite-query
                                    (:desc
                                     ,(cond ((member cstate
                                                     (list "quoted" nil) :test #'string=)
                                             'quote-date)
                                            ((string= cstate "ongoing")
                                             'start-date)
                                            ((member cstate
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
    (list
     (html ()
       (str (lisp->html (getf record :description))))
     (html ()
       (:a :href (company/details :id (getf record :company-id))
           (str (getf record :company))))
     (html ()
       (str (lisp->html (getf record :price)))))))


;;; paginator

(defclass project-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg project-paginator) start)
  (apply #'project :start start (filter (table pg))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage project-page project ("project")
    ((id     integer chk-project-id)
     (cstate string)
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
                                       '(:read :update)
                                       '(:read :details :update :delete)))
                     (display project-table
                              :key (val id)
                              :start (val start)))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (project :cstate (val cstate)) (val search))
                     (project-filters (val cstate) (val search)))
               (footer)))))))

(defpage project-page project/details ("project/details")
    ((search  string)
     (cstate  string)
     (id      integer chk-project-id           t)
     (bill-id integer (chk-bill-id id bill-id)))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :read
                                        :record (get-record 'project (val id))
                                        :cancel-url (apply #'project :id (val id) filter)))
           (bill-table (make-instance 'bill-table
                                      :op :read
                                      :project-id (val id))))
      (with-document ()
        (:head
         (:title "Έργο » Λεπτομέρειες")
         (admin-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'admin)
               (admin-navbar 'project)
               (:div :id "project-window" :class "window grid_6"
                     (:p :class "title" "Έργο » Λεπτομέρειες")
                     (project-menu (val id)
                                   filter
                                   '(:details))
                     (display project-form :payload (get-record 'project (val id))))
               (:div :id "bill-window" :class "window grid_6"
                     (:div :class "title" "Κοστολόγηση")
                     (bill-menu (val id)
                                (val bill-id)
                                filter
                                (if (suppliedp bill-id)
                                    '(:read)
                                    '(:read :update :delete :rank-up :rank-down)))
                     (display bill-table
                              :key (val bill-id)))
               (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage project-page project/create ("project/create")
    ((search      string)
     (cstate      string)
     (company     string chk-company-title)
     (description string chk-project-description/create)
     (location    string)
     (price       float  chk-amount*)
     (vat         float  chk-amount*)
     (state       string)
     (quote-date  date   (chk-quote-date quote-date state))
     (start-date  date   (chk-start-date start-date state))
     (end-date    date   (chk-end-date end-date state))
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
                     (notifications)
                     (with-form (actions/project/create :search (val search) :cstate (val cstate))
                       (display project-form :payload (params->payload)
                                             :styles (params->styles))))
               (footer)))))))

(defpage project-page actions/project/create ("actions/project/create"
                                              :request-type :post)
    ((search      string)
     (cstate      string)
     (company     string chk-company-title)
     (description string chk-project-description/create)
     (location    string)
     (price       float  chk-amount*)
     (vat         float  chk-amount*)
     (state       string)
     (quote-date  date   (chk-quote-date quote-date state))
     (start-date  date   (chk-start-date start-date state))
     (end-date    date   (chk-end-date end-date state))
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
                                       :state (val state)
                                       :notes (val notes))))
      (insert-dao new-project)
      (see-other (apply #'project/details :id (project-id new-project)
                        (params->filter))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage project-page project/update ("project/update")
    ((search      string)
     (cstate      string)
     (id          integer chk-project-id)
     (bill-id     integer (chk-bill-id id bill-id))
     (company     string  chk-company-title)
     (description string  (chk-project-description/update description id))
     (location    string)
     (price       float   chk-amount*)
     (vat         float   chk-amount*)
     (state       string)
     (quote-date  date    (chk-quote-date quote-date state))
     (start-date  date    (chk-start-date start-date state))
     (end-date    date    (chk-end-date end-date state))
     (notes       string))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :update
                                        :record (get-record 'project (val id))
                                        :cancel-url (apply #'project/details :id (val id) filter)))
           (bill-table (make-instance 'bill-table
                                      :op :read
                                      :project-id (val id))))
      (with-document ()
        (:head
         (:title "Έργο » Επεξεργασία")
         (admin-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'admin)
               (admin-navbar 'project)
               (:div :id "project-window" :class "window grid_6"
                     (:p :class "title" "Έργο » Επεξεργασία")
                     (project-menu (val id)
                                   filter
                                   '(:create :update))
                     (notifications)
                     (with-form (actions/project/update :id (val id) :search (val search))
                       (display project-form :payload (params->payload)
                                             :styles (params->styles))))
               (:div :id "bill-window" :class "window grid_6"
                     (:div :class "title" "Κοστολόγηση")
                     (bill-menu (val id)
                                (val bill-id)
                                filter
                                (if (val bill-id)
                                    '(:read)
                                    '(:read :update :delete :rank-up :rank-down)))
                     (display bill-table
                              :key (val bill-id)))
               (footer)))))))

(defpage project-page actions/project/update ("actions/project/update"
                                              :request-type :post)
    ((search      string)
     (cstate      string)
     (id          integer chk-project-id)
     (company     string  chk-company-title)
     (description string  (chk-project-description/update description id))
     (location    string)
     (price       float   chk-amount*)
     (vat         float   chk-amount*)
     (state       string)
     (quote-date  date    (chk-quote-date quote-date state))
     (start-date  date    (chk-start-date start-date state))
     (end-date    date    (chk-end-date end-date state))
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
                        'state (val state)
                        'notes (val notes)
                        :where (:= 'id (val id))))
      (see-other (apply #'project/details :id (val id)
                        (params->filter))))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage project-page project/delete ("project/delete")
    ((id     integer chk-project-id t)
     (search string)
     (cstate string))
  (with-view-page
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
                                                        :cstate (val cstate))
                       (display project-table
                                :key (val id))))
               (:div :id "sidebar" :class "sidebar grid_2"
                     (searchbox (project :cstate (val cstate)) (val search))
                     (project-filters (val cstate) (val search)))
               (footer)))))))

(defpage project-page actions/project/delete ("actions/project/delete"
                                              :request-type :post)
    ((id     integer chk-project-id)
     (search string)
     (cstate string))
  (with-controller-page (project/delete)
    (delete-dao (get-dao 'project (val id)))
    (see-other (apply #'project (params->filter)))))
