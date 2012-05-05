(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass project-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(project-id))
   (payload-parameter-names
    :allocation :class
    :initform '(company description location price vat state-id quote-date start-date end-date notes))
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
                    "Για την εταιρία αυτή, υπάρχει ήδη έργο με την ίδια περιγραφή."))
      (company     (:company-title-unknown
                    "Δεν έχει καταχωρηθεί εταιρία με αυτή την επωνυμία."
                    :company-title-null
                    "Η επωνυμία της εταιρίας είναι κενή"))
      (price       (:price-missing-for-archived-project
                    "Δεν μπορεί να αρχειοθετηθεί έργο χωρίς τιμή."
                    :non-positive-amount
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

;;; This one should be moved to project-stran (if project-stran is used)
(define-existence-predicate project-state-id-exists-p project-state id)

(define-existence-predicate project-id-exists-p project id)
(define-existence-predicate bill-id-exists-p bill id)

(defun project-description-exists-p (description company &optional project-id)
  (unless  (chk-company-title company)
    ;; Don't even bother to check without a valid company title
    (let ((company-id (company-id company)))
      (with-db ()
        (if project-id
            (query
             (:select 1 :from 'project :where
                      (:and (:= 'description description)
                            (:= company-id 'company-id)
                            (:not (:= 'id project-id))))
             :single)
            (query (:select 1 :from 'project
                    :where (:and (:= 'description description)
                                 (:= company-id 'company-id)))
                   :single))))))

(defun chk-project-id (project-id)
  (if (project-id-exists-p project-id)
      nil
      :project-id-unknown))

(defun chk-project-description/create (description company)
  (cond ((eql :null description) :project-description-null)
        ((project-description-exists-p description company) :project-description-exists)
        (t nil)))

(defun chk-project-description/update (description company project-id)
  (cond ((eql :null description) :project-description-null)
        ((project-description-exists-p description company project-id) :project-description-exists)
        (t nil)))

(defun chk-quote-date (date state-id)
  (let ((state-ids (list "quoted" "ongoing" "finished" "archived")))
    (cond ((and (member state-id state-ids :test #'string=)
                (eql date :null))
           :quote-date-null)
          (t
           nil))))

(defun chk-start-date (date state-id)
  (let ((state-ids (list "ongoing" "finished" "archived")))
    (cond ((and (member state-id state-ids :test #'string=)
                (eql date :null))
           :start-date-null)
          ((and (not (member state-id state-ids :test #'string=))
                (not (eql date :null)))
           :start-date-nonnull)
          (t
           nil))))

(defun chk-end-date (date state-id)
  (let ((state-ids (list "finished" "archived")))
    (cond ((and (member state-id state-ids :test #'string=)
                (eql date :null))
           :end-date-null)
          ((and (not (member state-id state-ids :test #'string=))
                (not (eql date :null)))
           :end-date-nonnull)
          (t
           nil))))

(defun chk-project-state-id (state-id)
  (if (project-state-id-exists-p state-id)
      nil
      :project-state-id-invalid))

(defun chk-bill-id (project-id bill-id)
  (if (and (project-id-exists-p project-id)
           (bill-id-exists-p bill-id))
      nil
      :bill-id-invalid))

(defun chk-price (price state-id)
  (or (chk-amount* price)
      (if (and (string-equal state-id "archived")
               (eql price :null))
          :price-missing-for-archived-project
          nil)))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun project-top-actions (op project-id filter)
  (top-actions (make-instance 'menu
                              :spec `((catalogue
                                       ,(html ()
                                          (:a :href (apply #'project :project-id project-id filter)
                                              (:img :src "/scrooge/img/application_view_list.png")
                                              "Κατάλογος")))
                                      (create
                                       ,(html ()
                                          (:a :href (apply #'project/create filter)
                                              (:img :src "/scrooge/img/add.png")
                                              "Νέο Έργο"))))
                              :css-class "hmenu"
                              :disabled (cond ((member op '(:catalogue :delete))
                                               '(catalogue))
                                              ((eql op :create)
                                               '(create))
                                              (t
                                               nil)))
               (searchbox #'actions/project/search
                          #'(lambda (&rest args)
                              (apply #'project :project-id project-id args))
                          filter
                          "ac-project")))
(defun project-actions (op project-id filter)
  (actions-menu (make-menu-spec
                 (action-anchors/crud+details (apply #'project/details :project-id project-id filter)
                                              (apply #'project/update :project-id project-id filter)
                                              (apply #'project/delete :project-id project-id filter)))
                (enabled-actions/crud+details op project-id)))

(defun project-filters (filter)
  (let ((filter* (remove-from-plist filter :cstate)))
    (filter-area (filter-navbar
                  `((nil      ,(project)                                    "Όλα")
                    (quoted   ,(apply #'project :cstate "quoted" filter*)   "Προσφορές")
                    (ongoing  ,(apply #'project :cstate "ongoing" filter*)  "Σε εξέλιξη")
                    (finished ,(apply #'project :cstate "finished" filter*) "Ολοκληρωμένα")
                    (archived ,(apply #'project :cstate "archived" filter*) "Αρχειοθετημένα")
                    (canceled ,(apply #'project :cstate "canceled" filter*) "Άκυρα"))
                  :active (getf filter :cstate)))))

(defun project-tabs (project-id content)
  (with-html
    (:div :class "grid_12"
          (:div :id "project-area"
                (when project-id
                  (htm
                   (:div :id "project-title"
                         (:h3 :class "grid_8 alpha"
                              (str (description (get-dao 'project project-id))))
                         (clear))))
                (display content)
                (clear)))))

(defpage project-page actions/project/search ("actions/project/search" :request-type :get)
    ((search string)
     (cstate string  chk-project-state-id))
  (with-db ()
    (let* ((filter (params->filter))
           (rows (rows (make-instance 'project-table :filter filter))))
      (if (single-item-list-p rows)
          (see-other (apply #'project/details
                            :project-id (key (first rows))
                            filter))
          (see-other (apply #'project filter))))))



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
                  (display lit 'company "Εταιρία" :extra-styles "ac-company"))
            (:div :class "project-form-details"
                  (:fieldset
                   (:legend "Οικονομικά")
                   (:ul
                    (:li (label 'state-id "Κατάσταση")
                         (dropdown 'state-id *project-state-ids*
                                   :selected (or (getf record :state-id)
                                                 *default-project-state-id*)
                                   :disabled disabled))
                    (:li (display lit 'price "Τιμή"))
                    (:li (display lit 'vat "Φ.Π.Α.")))))
            (:div :class "project-form-details"
                  (:fieldset
                   (:legend "Χρονοδιάγραμμα")
                   (:ul
                    (:li (display lit 'quote-date "Ημερομηνία προσφοράς"
                                  :extra-styles "datepicker"
                                  :default-value (today)))
                    (:li (display lit 'start-date "Ημερομηνία έναρξης"
                                  :extra-styles "datepicker"))
                    (:li (display lit 'end-date "Ημερομηνία ολοκλήρωσης"
                                  :extra-styles "datepicker")))))
            (clear)
            (:div :id "project-notes"
                  (label 'notes "Σημειώσεις")
                  (:textarea :name 'notes :disabled disabled
                             (str (lisp->html (or (getf record :notes) :null)))))
            (:div :class "data-form-buttons"
                  (unless disabled
                    (ok-button :body (if (eql (op form) :update) "Ανανέωση" "Δημιουργία"))
                    (cancel-button (cancel-url form) :body "Άκυρο")))))))

(defmethod get-record ((type (eql 'project)) project-id)
  (declare (ignore type))
  (query (:select 'project.id (:as 'company.title 'company)
                  'description 'location 'state-id 'quote-date
                  'start-date 'end-date 'price 'vat 'project.notes
          :from 'project
          :left-join 'company
          :on (:= 'project.company-id 'company.id)
          :where (:= 'project.id project-id))
         :plist))



;;; ------------------------------------------------------------
;;; Project table
;;; ------------------------------------------------------------

;;; table

(defclass project-table (scrooge-table)
  ((header-labels  :initform '("" "Περιγραφή" "Εταιρία" "Τιμή" "" ""))
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
      (push `(:= project.state-id ,cstate)
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
      (query (sql-compile final-query)
             :plists))))


;;; rows

(defclass project-row (scrooge-row/plist)
  ())

(defmethod selector ((row project-row) selected-p)
  (simple-selector row selected-p #'project :project-id))

(defmethod controls ((row project-row) controls-p)
  (simple-controls row controls-p #'project :project-id))

(defmethod payload ((row project-row) enabled-p)
  (let ((record (record row)))
    (list
     (html ()
       (:a :href (apply #'project/details
                        :project-id (key row)
                        (filter (collection row)))
           (str (lisp->html (getf record :description)))))
     (html ()
       (:a :href (company/details :company-id (getf record :company-id))
           (str (getf record :company))))
     (html ()
       (str (lisp->html (fmt-amount (getf record :price) 0)))))))


;;; paginator

(defclass project-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg project-paginator) start)
  (apply #'project :start start (filter (table pg))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage project-page project ("project")
    ((project-id integer chk-project-id)
     (cstate     string)
     (search     string)
     (start      integer))
  (with-view-page
    (let* ((op :catalogue)
           (filter (params->filter))
           (project-table (make-instance 'project-table
                                         :op op
                                         :filter filter
                                         :start-index (val start))))
      ;; if project-id exists and is not found among records, ignore search term
      (when (and (val project-id)
                 (not (find (val project-id) (rows project-table) :key #'key)))
        (see-other (project :project-id (val project-id) :cstate (state-id (get-dao 'project (val project-id))))))
      (with-document ()
        (:head
         (:title "Έργα » Κατάλογος")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'project)
               (project-top-actions op (val project-id) filter)
               (project-filters filter)
               (:div :class "grid_12"
                     (:div :id "project-window" :class "window"
                           (:div :class "title" "Κατάλογος")
                           (project-actions op (val project-id) filter)
                           (display project-table
                                    :key (val project-id))))
               (footer)))))))

(defpage project-page project/details ("project/details")
    ((search     string)
     (cstate     string)
     (project-id integer chk-project-id                   t)
     (bill-id    integer (chk-bill-id project-id bill-id)))
  (with-view-page
    (let* ((op :details)
           (filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op op
                                        :record (get-record 'project (val project-id))))
           (bill-table (make-instance 'bill-table
                                      :op :catalogue
                                      :project-id (val project-id))))
      (with-document ()
        (:head
         (:title "Έργο » Λεπτομέρειες")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'project)
               (project-top-actions op (val project-id) filter)
               (project-tabs (val project-id)
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "project-window" :class "window"
                                           (:p :class "title" "Λεπτομέρειες")
                                           (project-actions op (val project-id) filter)
                                           (display project-form
                                                    :payload (get-record 'project (val project-id)))))
                               (:div :class "grid_6 omega"
                                     (:div :id "bill-window" :class "window"
                                           (:div :class "title" "Κοστολόγηση")
                                           (bill-actions :catalogue (val project-id) (val bill-id) filter)
                                           (display bill-table
                                                    :key (val bill-id))))))
               (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage project-page project/create ("project/create")
    ((search      string)
     (cstate      string chk-project-state-id)
     (company     string chk-company-title)
     (description string (chk-project-description/create description company))
     (location    string)
     (price       float  (chk-price price state-id))
     (vat         float  chk-amount*)
     (state-id    string)
     (quote-date  date   (chk-quote-date quote-date state-id))
     (start-date  date   (chk-start-date start-date state-id))
     (end-date    date   (chk-end-date end-date state-id))
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
               (project-top-actions op nil filter)
               (project-tabs nil
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "project-window" :class "window"
                                           (:div :class "title" "Νέο Έργο")
                                           (project-actions op nil filter)
                                           (notifications)
                                           (with-form (actions/project/create :search (val search)
                                                                              :cstate (val cstate))
                                             (display project-form :payload (params->payload)
                                                                   :styles (params->styles)))))))
               (footer)))))))

(defpage project-page actions/project/create ("actions/project/create"
                                              :request-type :post)
    ((search      string)
     (cstate      string chk-project-state-id)
     (company     string chk-company-title)
     (description string (chk-project-description/create description company))
     (location    string)
     (price       float  (chk-price price state-id))
     (vat         float  chk-amount*)
     (state-id    string)
     (quote-date  date   (chk-quote-date quote-date state-id))
     (start-date  date   (chk-start-date start-date state-id))
     (end-date    date   (chk-end-date end-date state-id))
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
                                       :state-id (val state-id)
                                       :notes (val notes))))
      (insert-dao new-project)
      (see-other (apply #'project/details :project-id (project-id new-project)
                        (params->filter))))))



;;; ------------------------------------------------------------
;;; UPDATE
;;; ------------------------------------------------------------

(defpage project-page project/update ("project/update")
    ((search      string)
     (cstate      string  chk-project-state-id)
     (project-id  integer chk-project-id)
     (bill-id     integer (chk-bill-id project-id bill-id))
     (company     string  chk-company-title)
     (description string  (chk-project-description/update description company project-id))
     (location    string)
     (price       float   (chk-price price state-id))
     (vat         float   chk-amount*)
     (state-id    string)
     (quote-date  date    (chk-quote-date quote-date state-id))
     (start-date  date    (chk-start-date start-date state-id))
     (end-date    date    (chk-end-date end-date state-id))
     (notes       string))
  (with-view-page
    (let* ((op :update)
           (filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :update
                                        :record (get-record 'project (val project-id))
                                        :cancel-url (apply #'project/details :project-id (val project-id) filter)))
           (bill-table (make-instance 'bill-table
                                      :op :catalogue
                                      :project-id (val project-id))))
      (with-document ()
        (:head
         (:title "Έργο » Επεξεργασία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'project)
               (project-top-actions op (val project-id) filter)
               (project-tabs (val project-id)
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "project-window" :class "window"
                                           (:p :class "title" "Επεξεργασία")
                                           (project-actions op (val project-id) filter)
                                           (notifications)
                                           (with-form (actions/project/update :project-id (val project-id)
                                                                              :search (val search)
                                                                              :cstate (val cstate))
                                             (display project-form :payload (params->payload)
                                                                   :styles (params->styles)))))
                               (:div :class "grid_6 omega"
                                     (:div :id "bill-window" :class "window"
                                           (:div :class "title" "Κοστολόγηση")
                                           (bill-actions :catalogue (val project-id) (val bill-id) filter)
                                           (display bill-table
                                                    :key (val bill-id))))))
               (footer)))))))

(defpage project-page actions/project/update ("actions/project/update"
                                              :request-type :post)
    ((search      string)
     (cstate      string  chk-project-state-id)
     (project-id  integer chk-project-id)
     (company     string  chk-company-title)
     (description string  (chk-project-description/update description company project-id))
     (location    string)
     (price       float   (chk-price price state-id))
     (vat         float   chk-amount*)
     (state-id    string)
     (quote-date  date    (chk-quote-date quote-date state-id))
     (start-date  date    (chk-start-date start-date state-id))
     (end-date    date    (chk-end-date end-date state-id))
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
                        'state-id (val state-id)
                        'notes (val notes)
                        :where (:= 'id (val project-id))))
      (see-other (apply #'project/details :project-id (val project-id)
                        (params->filter))))))



;;; ------------------------------------------------------------
;;; DELETE
;;; ------------------------------------------------------------

(defpage project-page project/delete ("project/delete")
    ((project-id integer chk-project-id       t)
     (search     string)
     (cstate     string  chk-project-state-id))
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
               (project-top-actions op (val project-id) filter)
               (project-filters filter)
               (:div :class "grid_12"
                     (:div :id "project-window" :class "window"
                           (:div :class "title" "Έργο » Διαγραφή")
                           (project-actions op (val project-id) filter)
                           (with-form (actions/project/delete :project-id (val project-id)
                                                              :search (val search)
                                                              :cstate (val cstate))
                             (display project-table
                                      :key (val project-id)))))
               (footer)))))))

(defpage project-page actions/project/delete ("actions/project/delete"
                                              :request-type :post)
    ((project-id integer chk-project-id)
     (search     string)
     (cstate     string  chk-project-state-id))
  (with-controller-page (project/delete)
    (with-transaction ()
      (execute (:delete-from 'bill
                :where (:= 'project-id (val project-id))))
      (delete-dao (get-dao 'project (val project-id))))
    (see-other (apply #'project (params->filter)))))
