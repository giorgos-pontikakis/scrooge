(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass project-family (family-mixin)
  ()
  (:default-initargs :parameter-groups '(:system (project-id bill-id)
                                         :payload (company description price vat
                                                   state-id quote-date start-date end-date notes)
                                         :filter (search cstate))))


(defclass project-page (auth-dynamic-page project-family)
  ((messages
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

(defun project-referenced-p (project-id)
  (declare (ignore project-id))
  nil)

(defun project-description-exists-p (description company &optional project-id)
  ;; Don't even bother to check without a valid company title
  (unless (chk-company-title company)
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
  (cond ((eql :null project-id)
         :project-id-null)
        ((not (project-id-exists-p project-id))
         :project-id-unknown)))

(defun chk-project-id/ref (project-id)
  (cond ((chk-project-id project-id))
        ((project-referenced-p project-id) :project-referenced)))

(defun chk-project-description/create (description company)
  (cond ((eql :null description) :project-description-null)
        ((project-description-exists-p description company) :project-description-exists)))

(defun chk-project-description/update (description company project-id)
  (cond ((eql :null description) :project-description-null)
        ((project-description-exists-p description company project-id) :project-description-exists)))

(defun chk-quote-date (date state-id)
  (let ((state-ids (list "quoted" "ongoing" "finished" "archived")))
    (cond ((and (member state-id state-ids :test #'string=)
                (eql date :null))
           :quote-date-null))))

(defun chk-start-date (date state-id)
  (let ((state-ids (list "ongoing" "finished" "archived")))
    (cond ((and (member state-id state-ids :test #'string=)
                (eql date :null))
           :start-date-null)
          ((and (not (member state-id state-ids :test #'string=))
                (not (string= state-id "canceled"))
                (not (eql date :null)))
           :start-date-nonnull))))

(defun chk-end-date (date state-id)
  (let ((state-ids (list "finished" "archived")))
    (cond ((and (member state-id state-ids :test #'string=)
                (eql date :null))
           :end-date-null)
          ((and (not (member state-id state-ids :test #'string=))
                (not (string= state-id "canceled"))
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

(defun project-top-actions (op)
  (top-actions-area
   (make-instance 'scrooge-menu
                  :spec (make-menu-spec
                         `(:catalogue ,(family-url 'project :system :filter)
                           :create (,(family-url 'project/create :filter) "Νέο Έργο")))
                  :css-class "hmenu"
                  :disabled (list op))
   (searchbox (family-url-fn 'actions/project/search)
              (family-url-fn 'project :system)
              (family-params 'project :filter)
              "ac-project")))

(defun project-tabs (project-id content)
  (with-html
    (:div :class "grid_12"
      (:div :id "project-area"
        (when project-id
          (htm
           (:div :id "project-title"
             (:h2 :class "grid_8 alpha"
               (str (description (get-dao 'project project-id))))
             (clear))))
        (display content)
        (clear)))))



;;; ------------------------------------------------------------
;;; Project form
;;; ------------------------------------------------------------

(defclass project-form (crud-form)
  ()
  (:default-initargs :record-class 'cons))

(defmethod display ((form project-form) &key styles)
  (let* ((disabled (eql (op form) :details))
         (record (record form))
         (ldfn (label-datum disabled record styles)))
    (with-html
      (:div :class "data-form project-form"
        (:div :class "data-form-title"
          (display ldfn 'description "Περιγραφή")
          (display ldfn 'company "Εταιρία"
                   :enabled-styles "ac-company"
                   :href (company/details :company-id (getf record :company-id))))
        (:div :class "project-form-details"
          (:fieldset
            (:legend "Οικονομικά")
            (:ul
              (:li (label 'state-id "Κατάσταση")
                (dropdown 'state-id *project-state-ids*
                          :selected (or (getf record :state-id)
                                        *default-project-state-id*)
                          :disabled disabled))
              (:li (display ldfn 'price "Τιμή"))
              (:li (display ldfn 'vat "Φ.Π.Α.")))))
        (:div :class "project-form-details"
          (:fieldset
            (:legend "Χρονοδιάγραμμα")
            (:ul
              (:li (display ldfn 'quote-date "Ημερομηνία προσφοράς"
                            :enabled-styles "datepicker"
                            :default-value (today)))
              (:li (display ldfn 'start-date "Ημερομηνία έναρξης"
                            :enabled-styles "datepicker"))
              (:li (display ldfn 'end-date "Ημερομηνία ολοκλήρωσης"
                            :enabled-styles "datepicker")))))
        (clear)
        (:div :id "project-notes"
          (label 'notes "Σημειώσεις")
          (:textarea :name 'notes :disabled disabled
            (str (lisp->html (or (getf record :notes) :null))))))
      (:div :class "data-form-buttons"
        (unless disabled
          (ok-button :body (if (eql (op form) :update) "Ανανέωση" "Δημιουργία"))
          (cancel-button (cancel-url form) :body "Άκυρο"))))))

(defmethod get-record ((form project-form))
  (let ((project-id (key form)))
    (query (:select 'project.id (:as 'company.title 'company) 'company-id
                    'description 'state-id 'quote-date
                    'start-date 'end-date 'price 'vat 'project.notes
                    :from 'project
                    :left-join 'company
                    :on (:= 'project.company-id 'company.id)
                    :where (:= 'project.id project-id))
           :plist)))

(defmethod actions ((form project-form) &key filter)
  (let* ((project-id (key form))
         (spec (if project-id
                   (list :update (apply #'project/update :project-id project-id filter)
                         :delete (if (project-referenced-p project-id)
                                     nil
                                     (apply #'project/delete :project-id project-id filter)))
                   nil)))
    (actions-menu (make-menu-spec spec)
                  (disabled-actions form))))



;;; ------------------------------------------------------------
;;; Project table
;;; ------------------------------------------------------------

;;; table

(defclass project-table (scrooge-crud-table)
  ()
  (:default-initargs :record-class 'cons
                     :item-class 'project-row
                     :paginator (make-instance 'project-paginator)
                     :id "project-table"))

(defmethod initialize-instance :after ((table project-table) &key)
  (if (getf (filter table) :cstate)
      (setf (header-labels table) '("" "Εταιρία" "Περιγραφή" "Τιμή" "" ""))
      (setf (header-labels table) '("" "Εταιρία" "Περιγραφή" "Τιμή" "Κατάσταση" "" ""))))

(defmethod get-records ((table project-table))
  (let* ((search (getf (filter table) :search))
         (cstate (getf (filter table) :cstate))
         (base-query `(:select project.id project.description project.notes
                               price
                               (:as project-state.description project-state-description)
                               (:as company.id company-id) (:as company.title company)
                       :from project
                       :left-join company
                       :on (:= project.company-id company.id)
                       :left-join project-state
                       :on (:= project-state.id project.state-id)))
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
                                    company (:desc
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

(defmethod actions ((tbl project-table) &key)
  (let* ((project-id (selected-key tbl))
         (filter (filter tbl))
         (hrefs (if project-id
                    (list :details (apply #'project/details :project-id project-id filter)
                          :delete (if (project-referenced-p project-id)
                                      nil
                                      (apply #'project/delete :project-id project-id filter)))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))

(defmethod filters ((tbl project-table))
  (let ((filter* (remove-from-plist (filter tbl) :cstate)))
    (filter-area (filter-navbar
                  `((nil      ,(apply #'project filter*)                    "Όλα")
                    (quoted   ,(apply #'project :cstate "quoted" filter*)   "Προσφορές")
                    (ongoing  ,(apply #'project :cstate "ongoing" filter*)  "Σε εξέλιξη")
                    (finished ,(apply #'project :cstate "finished" filter*) "Ολοκληρωμένα")
                    (archived ,(apply #'project :cstate "archived" filter*) "Αρχειοθετημένα")
                    (canceled ,(apply #'project :cstate "canceled" filter*) "Άκυρα"))
                  :active (getf (filter tbl) :cstate)))))


;;; rows

(defclass project-row (scrooge-row)
  ())

(defmethod selector ((row project-row) selected-p)
  (simple-selector row selected-p #'project :project-id))

(defmethod controls ((row project-row) controls-p)
  (simple-controls row controls-p #'project :project-id))

(defmethod payload ((row project-row) enabled-p)
  (let* ((record (record row))
         (head (mapcar (textbox-maker record enabled-p)
                       `((company :href ,(company/details :company-id (getf record :company-id)))
                         (description :href ,(apply #'project/details
                                                    :project-id (key row)
                                                    (filter (collection row))))
                         (price :format-fn ,#'(lambda (arg) (fmt-amount arg 0)))))))
    (if (getf (filter (collection row)) :cstate)
        head
        (nconc head (list (html ()
                            (str (getf record :project-state-description))))))))


;;; paginator

(defclass project-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg project-paginator) start)
  (apply #'project :start start (filter (table pg))))



;;; ------------------------------------------------------------
;;; SEARCH
;;; ------------------------------------------------------------

(defpage project-page actions/project/search ("actions/project/search" :request-type :get)
    ((search string)
     (cstate string  chk-project-state-id))
  (with-db ()
    (let* ((filter (params->filter))
           (records (records (make-instance 'project-table :op :catalogue
                                                           :filter filter))))
      (if (single-item-list-p records)
          (see-other (apply #'project/details
                            :project-id (get-key (first search))
                            filter))
          (see-other (apply #'project filter))))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage project-page project ("project")
    ((project-id integer chk-project-id)
     (cstate     string)
     (search     string)
     (start      integer)) ()
  (with-view-page
    (let* ((filter (params->filter))
           (project-table (make-instance 'project-table
                                         :op :catalogue
                                         :selected-key (val project-id)
                                         :filter filter
                                         :start-index (val start))))
      ;; if project-id exists and is not found among records, ignore search term
      (when (and (val project-id)
                 (not (find (val project-id) (records project-table) :key #'get-key)))
        (see-other (project :project-id (val project-id)
                            :cstate (state-id (get-dao 'project (val project-id))))))
      (with-document ()
        (:head
          (:title "Έργα » Κατάλογος")
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'project)
            (project-top-actions :catalogue)
            (filters project-table)
            (:div :class "grid_12"
              (:div :id "project-window" :class "window"
                (:div :class "title" "Κατάλογος")
                (actions project-table)
                (display project-table)))
            (footer)))))))

(defpage project-page project/details ("project/details")
    ((project-id integer chk-project-id                   t)
     (bill-id    integer (chk-bill-id project-id bill-id))
     (search     string)
     (cstate     string))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :details
                                        :key (val project-id)))
           (bill-table (make-instance 'bill-table
                                      :op :catalogue
                                      :selected-key (val bill-id)
                                      :project-id (val project-id))))
      (with-document ()
        (:head
          (:title "Έργο » Λεπτομέρειες")
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'project)
            (project-top-actions :details)
            (project-tabs (val project-id)
                          (html ()
                            (:div :class "grid_6 alpha"
                              (:div :id "project-window" :class "window"
                                (:p :class "title" "Λεπτομέρειες")
                                (actions project-form :filter filter)
                                (display project-form)))
                            (:div :class "grid_6 omega"
                              (:div :id "bill-window" :class "window"
                                (:div :class "title" "Κοστολόγηση")
                                (actions bill-table)
                                (display bill-table)))))
            (footer)))))))



;;; ------------------------------------------------------------
;;; CREATE
;;; ------------------------------------------------------------

(defpage project-page project/create ("project/create")
    ((company     string chk-company-title)
     (description string (chk-project-description/create description company))
     (price       float  (chk-price price state-id))
     (vat         float  chk-amount*)
     (state-id    string)
     (quote-date  date   (chk-quote-date quote-date state-id))
     (start-date  date   (chk-start-date start-date state-id))
     (end-date    date   (chk-end-date end-date state-id))
     (notes       string)
     (search      string)
     (cstate      string chk-project-state-id))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :create
                                        :cancel-url (apply #'project filter))))
      (with-document ()
        (:head
          (:title "Έργο » Δημιουργία")
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'project)
            (project-top-actions :create)
            (project-tabs nil
                          (html ()
                            (:div :class "grid_6 alpha"
                              (:div :id "project-window" :class "window"
                                (:div :class "title" "Νέο Έργο")
                                (actions project-form)
                                (notifications)
                                (with-form (actions/project/create :search (val search)
                                                                   :cstate (val cstate))
                                  (display project-form :payload (params->payload)
                                                        :styles (params->styles)))))))
            (footer)))))))

(defpage project-page actions/project/create ("actions/project/create"
                                              :request-type :post)
    ((company     string chk-company-title)
     (description string (chk-project-description/create description company))
     (price       float  (chk-price price state-id))
     (vat         float  chk-amount*)
     (state-id    string)
     (quote-date  date   (chk-quote-date quote-date state-id))
     (start-date  date   (chk-start-date start-date state-id))
     (end-date    date   (chk-end-date end-date state-id))
     (notes       string)
     (search      string)
     (cstate      string chk-project-state-id))
  (with-controller-page (project/create)
    (let* ((company-id (company-id (val company)))
           (new-project (make-instance 'project
                                       :company-id company-id
                                       :description (val description)
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
    ((project-id  integer chk-project-id                                                  t)
     (bill-id     integer (chk-bill-id project-id bill-id))
     (company     string  chk-company-title)
     (description string  (chk-project-description/update description company project-id))
     (price       float   (chk-price price state-id))
     (vat         float   chk-amount*)
     (state-id    string)
     (quote-date  date    (chk-quote-date quote-date state-id))
     (start-date  date    (chk-start-date start-date state-id))
     (end-date    date    (chk-end-date end-date state-id))
     (notes       string)
     (search      string)
     (cstate      string  chk-project-state-id))
  (with-view-page
    (let* ((filter (params->filter))
           (project-form (make-instance 'project-form
                                        :op :update
                                        :key (val project-id)
                                        :cancel-url (apply #'project/details
                                                           :project-id (val project-id) filter)))
           (bill-table (make-instance 'bill-table
                                      :op :catalogue
                                      :selected-key (val bill-id)
                                      :project-id (val project-id))))
      (with-document ()
        (:head
          (:title "Έργο » Επεξεργασία")
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'project)
            (project-top-actions :update)
            (project-tabs (val project-id)
                          (html ()
                            (:div :class "grid_6 alpha"
                              (:div :id "project-window" :class "window"
                                (:p :class "title" "Επεξεργασία")
                                (actions project-form :filter filter)
                                (notifications)
                                (with-form
                                    (actions/project/update :project-id (val project-id)
                                                            :search (val search)
                                                            :cstate (val cstate))
                                  (display project-form :payload (params->payload)
                                                        :styles (params->styles)))))
                            (:div :class "grid_6 omega"
                              (:div :id "bill-window" :class "window"
                                (:div :class "title" "Κοστολόγηση")
                                (actions bill-table)
                                (display bill-table)))))
            (footer)))))))

(defpage project-page actions/project/update ("actions/project/update"
                                              :request-type :post)
    ((project-id  integer chk-project-id)
     (company     string  chk-company-title)
     (description string  (chk-project-description/update description company project-id))
     (price       float   (chk-price price state-id))
     (vat         float   chk-amount*)
     (state-id    string)
     (quote-date  date    (chk-quote-date quote-date state-id))
     (start-date  date    (chk-start-date start-date state-id))
     (end-date    date    (chk-end-date end-date state-id))
     (notes       string)
     (search      string)
     (cstate      string  chk-project-state-id))
  (with-controller-page (project/update)
    (let ((company-id (company-id (val company))))
      (execute (:update 'project :set
                        'company-id company-id
                        'description (val description)
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
    (let* ((filter (params->filter))
           (project-table (make-instance 'project-table
                                         :op :delete
                                         :selected-key (val project-id)
                                         :filter filter)))
      (with-document ()
        (:head
          (:title "Έργο » Διαγραφή")
          (main-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'main)
            (main-navbar 'project)
            (project-top-actions :delete)
            (filters project-table)
            (:div :class "grid_12"
              (:div :id "project-window" :class "window"
                (:div :class "title" "Έργο » Διαγραφή")
                (actions project-table)
                (with-form (actions/project/delete :project-id (val project-id)
                                                   :search (val search)
                                                   :cstate (val cstate))
                  (display project-table))))
            (footer)))))))

(defpage project-page actions/project/delete ("actions/project/delete"
                                              :request-type :post)
    ((project-id integer chk-project-id/ref   t)
     (search     string)
     (cstate     string  chk-project-state-id))
  (with-controller-page (project/delete)
    (with-transaction ()
      (execute (:delete-from 'bill
                :where (:= 'project-id (val project-id))))
      (delete-dao (get-dao 'project (val project-id))))
    (see-other (apply #'project (params->filter)))))
