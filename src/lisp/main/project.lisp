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

(defun project-description-exists-p (description company &optional id)
  (unless  (chk-company-title company)
    ;; Don't even bother to check without a valid company title
    (let ((company-id (company-id company)))
      (with-db ()
        (if id
            (query
             (:select 1 :from 'project :where
                      (:and (:= 'description description)
                            (:= company-id 'company-id)
                            (:not (:= 'id id))))
             :single)
            (query (:select 1 :from 'project
                    :where (:and (:= 'description description)
                                 (:= company-id 'company-id)))
                   :single))))))

(defun chk-project-id (id)
  (if (project-id-exists-p id)
      nil
      :project-id-unknown))

(defun chk-project-description/create (description company)
  (cond ((eql :null description) :project-description-null)
        ((project-description-exists-p description company) :project-description-exists)
        (t nil)))

(defun chk-project-description/update (description company id)
  (cond ((eql :null description) :project-description-null)
        ((project-description-exists-p description company id) :project-description-exists)
        (t nil)))

(defun chk-quote-date (date state)
  (let ((states (list "quoted" "ongoing" "finished" "archived")))
    (cond ((and (member state states :test #'string=)
                (eql date :null))
           :quote-date-null)
          (t
           nil))))

(defun chk-start-date (date state)
  (let ((states (list "ongoing" "finished" "archived")))
    (cond ((and (member state states :test #'string=)
                (eql date :null))
           :start-date-null)
          ((and (not (member state states :test #'string=))
                (not (eql date :null)))
           :start-date-nonnull)
          (t
           nil))))

(defun chk-end-date (date state)
  (let ((states (list "finished" "archived")))
    (cond ((and (member state states :test #'string=)
                (eql date :null))
           :end-date-null)
          ((and (not (member state states :test #'string=))
                (not (eql date :null)))
           :end-date-nonnull)
          (t
           nil))))

(defun chk-project-state (state)
  (if (project-state-id-exists-p state)
      nil
      :project-state-invalid))

(defun chk-bill-id (project-id bill-id)
  (if (and (project-id-exists-p project-id)
           (bill-id-exists-p bill-id))
      nil
      :bill-id-invalid))

(defun chk-price (price state)
  (or (chk-amount* price)
      (if (and (string-equal state "archived")
               (eql price :null))
          :price-missing-for-archived-project
          nil)))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun project-top-actions (op id filter)
  (top-actions (html ()
                 (menu `((catalogue ,(html ()
                                       (:a :href (apply #'project :id id filter)
                                           (:img :src "/scrooge/img/application_view_list.png")
                                           "Κατάλογος")))
                         (create ,(html ()
                                    (:a :href (apply #'project/create filter)
                                        (:img :src "/scrooge/img/add.png")
                                        "Νέα Εταιρία"))))
                       :css-class "hmenu"
                       :disabled (cond ((member op '(:catalogue :delete))
                                        '(catalogue))
                                       ((eql op :create)
                                        '(create))
                                       (t
                                        nil))))
               (html ()
                 (searchbox #'actions/project/search
                            #'(lambda (&rest args)
                                (apply #'project :id id args))
                            filter
                            "ac-project"))))
(defun project-actions (op id filter)
  (actions-menu (make-menu-spec
                 (action-anchors/crud+details (apply #'project/details :id id filter)
                                              (apply #'project/update :id id filter)
                                              (apply #'project/delete :id id filter)))
                (enabled-actions/crud+details op id)))

(defun project-filters (filter)
  (let ((filter* (remove-from-plist filter :cstate)))
    (filter-area (filter-navbar
                  `((nil      ,(project)                                    "Όλα")
                    (quoted   ,(apply #'project :cstate "quoted" filter*)   "Προσφορές")
                    (ongoing  ,(apply #'project :cstate "ongoing" filter*)  "Σε εξέλιξη")
                    (finished ,(apply #'project :cstate "finished" filter*) "Ολοκληρωμένα")
                    (archived ,(apply #'project :cstate "archived" filter*) "Αρχειοθετημένα")
                    (canceled ,(apply #'project :cstate "canceled" filter*) "Άκυρα"))
                  (getf filter :cstate)))))

(defun project-tabs (id filter active content)
  (declare (ignore filter active))
  (with-html
    (:div :class "grid_12"
          (:div :class "tabbar"
                (:div :class "tabbar-title"
                      (if id
                          (htm
                           (:h3 :class "grid_8 alpha"
                                (str (description (get-dao 'project id)))))
                          (htm
                           (:h3 :class "grid_8 alpha" "")))
                      (clear))
                (display content)
                (clear)))))

(defpage project-page actions/project/search ("actions/project/search" :request-type :get)
    ((search string)
     (cstate string  chk-project-state))
  (with-db ()
    (let* ((filter (params->filter))
           (records (get-records (make-instance 'project-table
                                                :filter filter))))
      (if (or (not records)
              (and records (cdr records)))
          (see-other (apply #'project filter))
          (see-other (apply #'project/details :id (getf (first records) :id) filter))))))



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
                    (:li (label 'state "Κατάσταση")
                         (dropdown 'state *project-states*
                                   :selected (or (getf record :state)
                                                 *default-project-state*)
                                   :disabled disabled))
                    (:li (display lit 'price "Τιμή"))
                    (:li (display lit 'vat "Φ.Π.Α.")))))
            (:div :class "project-form-details"
                  (:fieldset
                   (:legend "Χρονοδιάγραμμα")
                   (:ul
                    (:li (display lit 'quote-date "Ημερομηνία προσφοράς"
                                  :extra-styles "datepicker"))
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

(defmethod get-record ((type (eql 'project)) id)
  (declare (ignore type))
  (query (:select 'project.id (:as 'company.title 'company)
                  'description 'location 'state 'quote-date
                  'start-date 'end-date 'price 'vat 'project.notes
                  :from 'project
                  :left-join 'company
                  :on (:= 'project.company-id 'company.id)
                  :where (:= 'project.id id))
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
      (query (sql-compile final-query)
             :plists))))


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
      ;; if id exists and is not found among records, ignore search term
      (when (and (val id)
                 (not (find (val id) (rows project-table) :key #'key)))
        (see-other (project :id (val id) :cstate (state (get-dao 'project (val id))))))
      (with-document ()
        (:head
         (:title "Έργα » Κατάλογος")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'project)
               (project-top-actions op (val id) filter)
               (project-filters filter)
               (:div :class "grid_12"
                     (:div :id "project-window" :class "window"
                           (:div :class "title" "Κατάλογος")
                           (project-actions op (val id) filter)
                           (display project-table
                                    :key (val id))))
               (footer)))))))

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
               (project-top-actions op (val id) filter)
               (project-tabs (val id) filter nil
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "project-window" :class "window"
                                           (:p :class "title" "Λεπτομέρειες")
                                           (project-actions op (val id) filter)
                                           (display project-form
                                                    :payload (get-record 'project (val id)))))
                               (:div :class "grid_6 omega"
                                     (:div :id "bill-window" :class "window"
                                           (:div :class "title" "Κοστολόγηση")
                                           (bill-actions :catalogue (val id) (val bill-id) filter)
                                           (display bill-table
                                                    :key (val bill-id))))))
               (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage project-page project/create ("project/create")
    ((search      string)
     (cstate      string chk-project-state)
     (company     string chk-company-title)
     (description string (chk-project-description/create description company))
     (location    string)
     (price       float  (chk-price price state))
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
               (project-top-actions op nil filter)
               (project-tabs nil filter nil
                             (html ()
                               (:div :class "grid_12"
                                     (:div :id "project-window" :class "window"
                                           (:div :class "title" "Δημιουργία")
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
     (cstate      string chk-project-state)
     (company     string chk-company-title)
     (description string (chk-project-description/create description company))
     (location    string)
     (price       float  (chk-price price state))
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
     (cstate      string chk-project-state)
     (id          integer chk-project-id)
     (bill-id     integer (chk-bill-id id bill-id))
     (company     string  chk-company-title)
     (description string  (chk-project-description/update description company id))
     (location    string)
     (price       float   (chk-price price state))
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
               (project-top-actions op (val id) filter)
               (project-tabs (val id) filter nil
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "project-window" :class "window"
                                           (:p :class "title" "Επεξεργασία")
                                           (project-actions op (val id) filter)
                                           (notifications)
                                           (with-form (actions/project/update :id (val id)
                                                                              :search (val search)
                                                                              :cstate (val cstate))
                                             (display project-form :payload (params->payload)
                                                                   :styles (params->styles)))))
                               (:div :class "grid_6 omega"
                                     (:div :id "bill-window" :class "window"
                                           (:div :class "title" "Κοστολόγηση")
                                           (bill-actions :catalogue (val id) (val bill-id) filter)
                                           (display bill-table
                                                    :key (val bill-id))))))
               (footer)))))))

(defpage project-page actions/project/update ("actions/project/update"
                                              :request-type :post)
    ((search      string)
     (cstate      string  chk-project-state)
     (id          integer chk-project-id)
     (company     string  chk-company-title)
     (description string  (chk-project-description/update description company id))
     (location    string)
     (price       float   (chk-price price state))
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
     (cstate string chk-project-state))
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
               (project-top-actions op (val id) filter)
               (project-filters filter )
               (:div :class "grid_12"
                     (:div :id "project-window" :class "window"
                           (:div :class "title" "Έργο » Διαγραφή")
                           (project-actions op (val id) filter)
                           (with-form (actions/project/delete :id (val id)
                                                              :search (val search)
                                                              :cstate (val cstate))
                             (display project-table
                                      :key (val id)))))
               (footer)))))))

(defpage project-page actions/project/delete ("actions/project/delete"
                                              :request-type :post)
    ((id     integer chk-project-id)
     (search string)
     (cstate string chk-project-state))
  (with-controller-page (project/delete)
    (with-transaction ()
      (execute (:delete-from 'bill
                :where (:= 'project-id (val id))))
      (delete-dao (get-dao 'project (val id))))
    (see-other (apply #'project (params->filter)))))
