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

(defun project-actions (op id filter)
  (actions-menu (crud+details-actions-spec (apply #'project/create filter)
                                           (if id
                                               (apply #'project/details :id id filter)
                                               nil)
                                           (apply #'project/update :id id filter)
                                           (apply #'project/delete :id id filter))
                (crud+details-actions-enabled/disabled op id)))

(defun project-filters (cstate search)
  (filters-navbar `((nil      ,(project :search search)                    "Όλα")
                    (quoted   ,(project :search search :cstate "quoted")   "Προσφορές")
                    (ongoing  ,(project :search search :cstate "ongoing")  "Σε εξέλιξη")
                    (finished ,(project :search search :cstate "finished") "Ολοκληρωμένα")
                    (archived ,(project :search search :cstate "archived") "Αρχειοθετημένα")
                    (canceled ,(project :search search :cstate "canceled") "Άκυρα"))
                  cstate))

(defun project-subnavbar (op id cstate search)
  (with-html
    (:div :class "section-subnavbar grid_12"
          (if (member op '(:catalogue :delete))
              (project-filters cstate search)
              (htm (:div :class "options"
                         (:ul (:li (:a :href (project :id id :cstate cstate :search search)
                                       "Κατάλογος"))))))
          (searchbox #'project
                     search
                     :hidden `(:cstate ,cstate)
                     :css-class "ac-project"))))



;;; ------------------------------------------------------------
;;; Project form
;;; ------------------------------------------------------------

(defclass project-form (crud-form/plist)
  ())


(defmethod display ((form project-form) &key styles)
  (let* ((disabled (eql (op form) :details))
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
                  (unless disabled
                    (ok-button :body (if (eql (op form) :update) "Ανανέωση" "Δημιουργία"))
                    (cancel-button (cancel-url form) :body "Άκυρο")))))))

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
       (:a :href (apply #'project/details
                        :id (key row)
                        (filter (collection row)))
           (str (lisp->html (getf record :description)))))
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
    (let* ((op :catalogue)
           (filter (params->filter))
           (project-table (make-instance 'project-table
                                         :op op
                                         :filter filter
                                         :start-index (val start))))
      (if (or (null (rows project-table))
              (cdr (rows project-table)))
          (with-document ()
            (:head
             (:title "Έργα » Κατάλογος")
             (main-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header)
                   (main-navbar 'project)
                   (project-subnavbar op (val id) (val cstate) (val search))
                   (:div :id "project-window" :class "window grid_12"
                         (:div :class "title" "Κατάλογος")
                         (project-actions op (val id) filter)
                         (display project-table
                                  :key (val id)
                                  :start (val start)))
                   (footer))))
          (redirect (apply #'project/details :id (key (first (rows project-table)))
                           filter))))))

(defpage project-page project/details ("project/details")
    ((search  string)
     (cstate  string)
     (id      integer chk-project-id           t)
     (bill-id integer (chk-bill-id id bill-id)))
  (with-view-page
    (let* ((op :details)
           (filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op op
                                        :record (get-record 'project (val id))
                                        :cancel-url (apply #'project :id (val id) filter)))
           (bill-table (make-instance 'bill-table
                                      :op :catalogue
                                      :project-id (val id))))
      (with-document ()
        (:head
         (:title "Έργο » Λεπτομέρειες")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'project)
               (project-subnavbar op (val id) (val cstate) (val search))
               (:div :id "project-window" :class "window grid_6"
                     (:p :class "title" "Λεπτομέρειες")
                     (project-actions op (val id) filter)
                     (display project-form :payload (get-record 'project (val id))))
               (:div :id "bill-window" :class "window grid_6"
                     (:div :class "title" "Κοστολόγηση")
                     (bill-actions :catalogue (val id) (val bill-id) filter)
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
    (let* ((op :create)
           (filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op op
                                        :record nil
                                        :cancel-url (apply #'project filter))))
      (with-document ()
        (:head
         (:title "Έργο » Δημιουργία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'project)
               (project-subnavbar op nil (val cstate) (val search))
               (:div :id "project-window" :class "window grid_12"
                     (:div :class "title" "Δημιουργία")
                     (project-actions op nil filter)
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
    (let* ((op :update)
           (filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :update
                                        :record (get-record 'project (val id))
                                        :cancel-url (apply #'project/details :id (val id) filter)))
           (bill-table (make-instance 'bill-table
                                      :op :catalogue
                                      :project-id (val id))))
      (with-document ()
        (:head
         (:title "Έργο » Επεξεργασία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'project)
               (project-subnavbar op (val id) (val cstate) (val search))
               (:div :id "project-window" :class "window grid_6"
                     (:p :class "title" "Επεξεργασία")
                     (project-actions op (val id) filter)
                     (notifications)
                     (with-form (actions/project/update :id (val id) :search (val search))
                       (display project-form :payload (params->payload)
                                             :styles (params->styles))))
               (:div :id "bill-window" :class "window grid_6"
                     (:div :class "title" "Κοστολόγηση")
                     (bill-actions :catalogue (val id) (val bill-id) filter)
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
    (let* ((op :delete)
           (filter (params->filter))
           (project-table (make-instance 'project-table
                                         :op :delete
                                         :filter filter)))
      (with-document ()
        (:head
         (:title "Έργο » Διαγραφή")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'project)
               (project-subnavbar op (val id) (val cstate) (val search))
               (:div :id "project-window" :class "window grid_12"
                     (:div :class "title" "Έργο » Διαγραφή")
                     (project-actions op (val id) filter)
                     (with-form (actions/project/delete :id (val id)
                                                        :search (val search)
                                                        :cstate (val cstate))
                       (display project-table
                                :key (val id))))
               (footer)))))))

(defpage project-page actions/project/delete ("actions/project/delete"
                                              :request-type :post)
    ((id     integer chk-project-id)
     (search string)
     (cstate string))
  (with-controller-page (project/delete)
    (delete-dao (get-dao 'project (val id)))
    (see-other (apply #'project (params->filter)))))
