(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass company-family (family-mixin)
  ()
  (:default-initargs
   :parameter-groups '(:system (company-id tx-id start)
                       :payload (title occupation tof tin address city pobox zipcode notes)
                       :filter (search subset))))

(defclass company-page (auth-dynamic-page company-family)
  ((messages
    :allocation :class
    :reader messages
    :initform '((title
                 (:company-title-null
                  "Η επωνυμία της εταιρίας είναι κενή"
                  :company-title-exists
                  "Υπάρχει ήδη εταιρία με αυτή την επωνυμία"))
                (tof
                 (:tof-title-unknown
                  "Η Δ.Ο.Υ. αυτή δεν έχει οριστεί."))
                (city
                 (:city-title-unknown
                  "Η πόλη αυτή δεν έχει οριστεί."))
                (tin
                 (:tin-exists
                  "Υπάρχει ήδη εταιρία με αυτόν τον Α.Φ.Μ."
                  :tin-invalid
                  "Άκυρος Α.Φ.Μ."))
                (pobox
                 (:parse-error
                  "Άκυροι χαρακτήρες στο αριθμό ταχυδρομικής θυρίδας"
                  :pobox-invalid
                  "Μη αποδεκτός αριθμός ταχυδρομικής θυρίδας."))
                (zipcode
                 (:parse-error
                  "Άκυροι χαρακτήρες στον ταχυδρομικό κωδικό"
                  :zipcode-invalid
                  "Μη αποδεκτός ταχυδρομικός κωδικός."))))))

(defun params->company-filter ()
  (params->filter :page (find-page 'company)))


;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(defun company-referenced-p (company-id)
  (with-db ()
    (or (query (:select 1 :from 'project
                :where (:= 'company-id company-id)))
        (query (:select 1 :from 'cheque
                :where (:= 'company-id company-id)))
        (query (:select 1 :from 'tx
                :where (:= 'company-id company-id))))))

(define-existence-predicate company-id-exists-p company id)
(define-existence-predicate contact-id-exists-p contact id)
(define-existence-predicate* company-title-exists-p company title id)
(define-existence-predicate* tin-exists-p company tin id)


(defun chk-company-id (company-id)
  (if (company-id-exists-p company-id)
      nil
      :company-id-unknown))

(defun chk-company-id/ref (company-id)
  (if (and (not (null company-id))
           (not (chk-company-id company-id))
           (not (company-referenced-p company-id)))
      nil
      :company-referenced))

(defun chk-company-title/create (title)
  (cond ((eql :null title) :company-title-null)
        ((company-title-exists-p title) :company-title-exists)
        (t nil)))

(defun chk-company-title/update (title company-id)
  (cond ((eql :null title) :company-title-null)
        ((company-title-exists-p title company-id) :company-title-exists)
        (t nil)))

(defun chk-company-title (title)
  (cond ((eql :null title) :company-title-null)
        ((not (company-title-exists-p title)) :company-title-unknown)
        (t nil)))

(defun chk-tin/create (tin)
  (cond ((eql :null tin) nil)
        ((tin-exists-p tin)
         :tin-exists)
        ((not (valid-tin-p tin))
         :tin-invalid)
        (t nil)))

(defun chk-tin/update (tin company-id)
  (cond ((eql :null tin) nil)
        ((tin-exists-p tin company-id)
         :tin-exists)
        ((not (valid-tin-p tin))
         :tin-invalid)
        (t nil)))

(defun chk-pobox (pobox)
  (if (or (eql :null pobox)
          (positive-integer-p pobox))
      nil
      :pobox-invalid))

(defun chk-zipcode (zipcode)
  (if (or (eql :null zipcode)
          (int-5digits-p zipcode))
      nil
      :zipcode-invalid))

(defun chk-contact-id (company-id contact-id)
  (if (and (company-id-exists-p company-id)
           (contact-id-exists-p contact-id))
      nil
      :contact-id-invalid))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun company-top-actions (op filter)
  (top-actions (make-instance 'menu
                              :spec (make-menu-spec
                                     `(:catalogue ,(gurl 'company :system :filter)
                                       :create ,(gurl 'company/create :filter)))
                              :css-class "hmenu"
                              :css-disabled "invisible"
                              :disabled (list op))
               (searchbox (gurl-fn 'actions/company/search)
                          (gurl-fn 'company)
                          filter
                          "ac-company")))

(defun company-tabs (company-id filter active content)
  (with-html
    (:div :class "grid_12"
          (:div :id "company-area"
                (when company-id
                  (htm (:div :id "company-title"
                             (:h3 :class "grid_12 alpha"
                                  (str (title (get-dao 'company company-id))))
                             (navbar
                              `((data ,(apply #'company/details
                                              :company-id company-id
                                              filter)
                                      "Στοιχεία")
                                (tx ,(apply #'company/tx
                                            :company-id company-id
                                            filter)
                                    "Συναλλαγές")
                                (cheque ,(apply #'company/cheque "receivable"
                                                :company-id company-id
                                                filter)
                                        "Επιταγές"))
                              :css-class "hnavbar grid_5 prefix_7"
                              :active active
                              :id "company-tabs"
                              :css-class "hnavbar")
                             (clear))))
                (display content)
                (clear)))))



;;; ------------------------------------------------------------
;;; Company form
;;; ------------------------------------------------------------

(defclass company-form (crud-form/plist company-family)
  ())

(defmethod display ((form company-form) &key styles)
  (let* ((disabled (eql (op form) :details))
         (record (record form))
         (lit (label-input-text disabled record styles)))
    (with-html
      (:div :class "data-form company-form"
            (:div :class "data-form-title"
                  (display lit 'title "Επωνυμία"))
            (:fieldset
             (:legend "Φορολογικά στοιχεία")
             (display lit 'occupation "Επάγγελμα" :extra-styles "ac-occupation")
             (:div :id "tin"
                   (display lit 'tin "Α.Φ.Μ."))
             (:div :id "tof-div"
                   (display lit 'tof "Δ.Ο.Υ." :extra-styles "ac-tof")))
            (:fieldset
             (:legend "Διεύθυνση")
             (:div :id "address"
                   (display lit 'address "Οδός"))
             (:div :id "city"
                   (display lit 'city "Πόλη" :extra-styles "ac-city"))
             (:div :id "zipcode"
                   (display lit 'zipcode "Ταχυδρομικός κωδικός"))
             (:div :id "pobox"
                   (display lit 'pobox "Ταχυδρομική θυρίδα")))
            (:div :id "company-notes"
                  (label 'notes "Σημειώσεις")
                  (:textarea :name 'notes :disabled disabled
                             (str (lisp->html (or (getf record :notes) :null))))))
      (:div :class "data-form-buttons"
            (unless disabled
              (ok-button :body (if (eql (op form) :update) "Ανανέωση" "Δημιουργία"))
              (cancel-button (cancel-url form) :body "Άκυρο"))))))

(defmethod get-record ((form company-form))
  (let ((company-id (key form)))
    (if company-id
        (query (:select 'company.title 'occupation
                        'tin (:as 'tof.title 'tof)
                        'address (:as 'city.title 'city)
                        'zipcode 'pobox 'notes
                :from 'company
                :left-join 'city
                :on (:= 'company.city-id 'city.id)
                :left-join 'tof
                :on (:=  'company.tof-id 'tof.id)
                :where (:= 'company.id company-id))
               :plist)
        nil)))

(defmethod actions ((form company-form) &key filter)
  (let* ((company-id (key form))
         (spec (if company-id
                   (list :update (apply #'company/update :company-id company-id filter)
                         :delete (if (chk-company-id/ref company-id)
                                     nil
                                     (apply #'company/delete :company-id company-id filter))
                         :create (cons "Νέο Έργο"
                                       (project/create :company (title (get-dao 'company company-id)))))
                   nil)))
    (actions-menu (make-menu-spec spec)
                  (disabled-actions form))))

(defmethod disabled-actions ((form company-form) &key)
  (ecase (op form)
    (:details '())
    ((:create :update :delete) '(:create-project :update :delete))))



;;; ------------------------------------------------------------
;;; Company table
;;; ------------------------------------------------------------

;;; table

(defclass company-table (scrooge-table)
  ((header-labels  :initform '("" "Επωνυμία" "Α.Φ.Μ." "Δ.Ο.Υ." "" ""))
   (paginator      :initform (make-instance 'company-paginator
                                            :id "company-paginator"
                                            :css-class "paginator")))
  (:default-initargs :item-class 'company-row :id "company-table"))

(defmethod get-records ((table company-table))
  (let* ((search (getf (filter table) :search))
         (subset (getf (filter table) :subset))
         (ilike-term (ilike search))
         (base-query `(:select company.id company.title tin
                               (:as tof.title tof)
                               address occupation
                               (:as city.title city-name)
                       :distinct
                       :from company
                       :left-join city
                       :on (:= city.id company.city-id)
                       :left-join tof
                       :on (:= tof.id company.tof-id)
                       :left-join contact
                       :on (:= contact.company-id company.id)
                       :left-join project
                       :on (:= project.company-id company.id)))
         (where nil))
    (when search
      (push `(:or (:ilike company.title ,ilike-term)
                  (:ilike tin ,ilike-term)
                  (:ilike address ,ilike-term)
                  (:ilike city.title ,ilike-term)
                  (:ilike occupation ,ilike-term)
                  (:ilike company.notes ,ilike-term)
                  (:ilike contact.tag ,ilike-term)
                  (:ilike contact.phone ,ilike-term))
            where))
    (when subset
      (cond
        ((string= subset "projects")
         (push `(:= project.state-id "ongoing")
               where))
        ((member subset (list "credit" "debt") :test #'string=)
         (push `(,(if (string= subset "credit") :< :>)
                 (:select (coalesce (sum tx.amount) 0)
                  :from tx
                  :left-join cheque-event
                  :on (:= cheque-event.tx-id tx.id)
                  :where (:and
                          (:= tx.company-id company.id)
                          (:or (:= tx.credit-acc-id ,*cash-acc-id*)
                               (:in tx.credit-acc-id
                                    (:set ,@*revenues-accounts*))
                               (:and (:= tx.credit-acc-id ,*cheque-payable-acc-id*)
                                     (:not (:exists (:select 1
                                                     :from (:as tx tx2)
                                                     :inner-join (:as cheque-event cheque-event2)
                                                     :on (:= cheque-event2.id tx2.id)
                                                     :where (:and
                                                             (:= cheque-event2.cheque-id
                                                                 cheque-event.cheque-id)
                                                             (:= tx2.debit-acc-id
                                                                 ,*cheque-payable-acc-id*)))))))))
                 (:select (coalesce (sum tx.amount) 0)
                  :from tx
                  :left-join cheque-event
                  :on (:= cheque-event.tx-id tx.id)
                  :where (:and
                          (:= tx.company-id company.id)
                          (:or (:= tx.debit-acc-id ,*cash-acc-id*)
                               (:in tx.debit-acc-id
                                    (:set ,@*expense-accounts*))
                               (:and (:= tx.debit-acc-id ,*cheque-receivable-acc-id*)
                                     (:not (:exists (:select 1
                                                     :from (:as tx tx2)
                                                     :inner-join (:as cheque-event cheque-event2)
                                                     :on (:= cheque-event2.tx-id tx2.id)
                                                     :where (:and
                                                             (:= cheque-event2.cheque-id
                                                                 cheque-event.cheque-id)
                                                             (:= tx2.credit-acc-id
                                                                 ,*cheque-receivable-acc-id*))))))))))
               where))
        (t nil)))
    (let ((sql `(:order-by (,@base-query :where
                                         (:and t
                                               ,@where))
                           company.title)))
      (query (sql-compile sql)
             :plists))))

(defmethod actions ((tbl company-table) &key)
  (let* ((company-id (selected-key tbl))
         (filter (filter tbl))
         (hrefs (if company-id
                    (list :details (apply #'company/details :company-id company-id filter)
                          :delete (if (chk-company-id/ref company-id)
                                      nil
                                      (apply #'company/delete :company-id company-id filter))
                          :create-project (html ()
                                            (:a :href (project/create
                                                       :company (title (get-dao 'company company-id)))
                                                :class "create"
                                                "Νέο Έργο")))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))

(defmethod disabled-actions ((tbl company-table) &key)
  (ecase (op tbl)
    (:catalogue '())
    (:delete '(:details :delete :create-project))))

(defmethod filters ((tbl company-table))
  (let* ((filter* (remove-from-plist (filter tbl) :subset))
         (filter-spec `(("nil"      ,(apply #'company filter*)
                                    "Όλες")
                        ("projects" ,(apply #'company :subset "projects" filter*)
                                    "Με ενεργά έργα")
                        ("debt"     ,(apply #'company :subset "debt" filter*)
                                    "Με χρεωστικό υπόλοιπο")
                        ("credit"   ,(apply #'company :subset "credit" filter*)
                                    "Με πιστωτικό υπόλοιπο"))))
    (filter-area (filter-navbar filter-spec
                                :active (getf (filter tbl) :subset)))))

;;; rows

(defclass company-row (scrooge-row/plist)
  ())

(defmethod selector ((row company-row) selected-p)
  (simple-selector row selected-p #'company :company-id))

(defmethod controls ((row company-row) controls-p)
  (simple-controls row controls-p #'company :company-id))

(defmethod payload ((row company-row) enabled-p)
  (let ((record (record row)))
    (list*
     (html ()
       (:a :href (apply #'company/details
                        :company-id (key row)
                        (filter (collection row)))
           (str (lisp->html (getf record :title)))))
     (mapcar (lambda (name)
               (make-instance 'textbox
                              :name name
                              :value (getf record (make-keyword name))
                              :disabled (not enabled-p)))
             '(tin tof)))))


;;; paginator

(defclass company-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg company-paginator) start)
  (apply #'company :start start (filter (table pg))))



;;; ----------------------------------------------------------------------
;;; SEARCH
;;; ----------------------------------------------------------------------

(defpage company-page actions/company/search ("actions/company/search" :request-type :get)
    ((search string))
  (with-db ()
    (let* ((filter (params->filter))
           (rows (rows (make-instance 'company-table :filter filter))))
      (if (single-item-list-p rows)
          (see-other (apply #'company/details
                            :company-id (key (first rows))
                            filter))
          (see-other (apply #'company filter))))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage company-page company ("company")
    ((company-id integer chk-company-id)
     (search     string)
     (subset     string)
     (start      integer))
  (with-view-page
    (let* ((filter (params->filter))
           (company-table (make-instance 'company-table
                                         :op :catalogue
                                         :selected-key (val company-id)
                                         :filter filter
                                         :start-index (val start))))
      ;; if company-id exists and is not found among records, ignore search term
      (when (and (val company-id)
                 (not (find (val company-id) (rows company-table) :key #'key)))
        (see-other (company :company-id (val company-id))))
      (with-document ()
        (:head
         (:title "Εταιρίες » Κατάλογος")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-top-actions :catalogue filter)
               (filters company-table)
               (:div :class "grid_12"
                     (:div :id "company-window" :class "window"
                           (:div :class "title"  "Κατάλογος")
                           (actions company-table)
                           (display company-table)))
               (footer)))))))

(defpage company-page company/details ("company/details")
    ((company-id integer chk-company-id                         t)
     (contact-id integer (chk-contact-id company-id contact-id))
     (search     string)
     (subset     string))
  (with-view-page
    (let* ((filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op :details
                                        :key (val company-id)
                                        :cancel-url (apply #'company
                                                           :company-id (val company-id)
                                                           filter)))
           (contact-table (make-instance 'contact-table
                                         :op :catalogue
                                         :selected-key (val contact-id)
                                         :company-id (val company-id))))
      (with-document ()
        (:head
         (:title "Εταιρία » Λεπτομέρειες")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-top-actions :details filter)
               (company-tabs (val company-id) filter 'data
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "company-window" :class "window"
                                           (:div :class "title" "Λεπτομέρειες")
                                           (actions company-form :filter filter)
                                           (display company-form)))
                               (:div :class "grid_6 omega"
                                     (:div :id "contact-window" :class "window"
                                           (:div :class "title" "Επαφές")
                                           (actions contact-table)
                                           (display contact-table)))))
               (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage company-page company/create ("company/create")
    ((search     string)
     (subset     string)
     (title      string  chk-company-title/create)
     (occupation string)
     (tof        string  chk-tof-title)
     (tin        string  chk-tin/create)
     (address    string)
     (city       string  chk-city-title)
     (pobox      integer chk-pobox)
     (zipcode    integer chk-zipcode)
     (notes      string))
  (with-view-page
    (let* ((filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op :create
                                        :cancel-url (apply #'company filter))))
      (with-document ()
        (:head
         (:title "Εταιρία » Δημιουργία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-top-actions :create filter)
               (company-tabs nil filter 'data
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "company-window" :class "window"
                                           (:div :class "title" "Νέα εταιρία")
                                           (actions company-form :filter filter)
                                           (notifications)
                                           (with-form (actions/company/create :search (val search))
                                             (display company-form
                                                      :payload (params->payload)
                                                      :styles (params->styles)))))))
               (footer)))))))

(defpage company-page actions/company/create ("actions/company/create"
                                              :request-type :post)
    ((search     string)
     (subset     string)
     (title      string  chk-company-title/create)
     (occupation string)
     (tof        string  chk-tof-title)
     (tin        string  chk-tin/create)
     (address    string)
     (city       string  chk-city-title)
     (pobox      integer chk-pobox)
     (zipcode    integer chk-zipcode)
     (notes      string))
  (with-controller-page (company/create)
    (let* ((tof-id (tof-id (val tof)))
           (city-id (city-id (val city)))
           (new-company (make-instance 'company
                                       :title (val title)
                                       :occupation (val occupation)
                                       :tof-id tof-id
                                       :tin (val tin)
                                       :address (val address)
                                       :city-id city-id
                                       :zipcode (val zipcode)
                                       :pobox (val pobox)
                                       :notes (val notes))))
      (insert-dao new-company)
      (see-other (apply #'company/details :company-id (company-id new-company)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage company-page company/update ("company/update")
    ((search     string)
     (subset     string)
     (company-id integer chk-company-id                              t)
     (contact-id integer (chk-contact-id company-id contact-id))
     (title      string  (chk-company-title/update title company-id))
     (occupation string)
     (tof        string  chk-tof-title)
     (tin        string  (chk-tin/update tin company-id))
     (address    string)
     (city       string  chk-city-title)
     (pobox      integer chk-pobox)
     (zipcode    integer chk-zipcode)
     (notes      string))
  (with-view-page
    (let* ((filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op :update
                                        :key (val company-id)
                                        :cancel-url (apply #'company/details
                                                           :company-id (val company-id) filter)))
           (contact-table (make-instance 'contact-table
                                         :op :catalogue
                                         :selected-key (val contact-id)
                                         :company-id (val company-id))))
      (with-document ()
        (:head
         (:title "Εταιρία » Επεξεργασία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-top-actions :update filter)
               (company-tabs (val company-id) filter 'data
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "company-window" :class "window"
                                           (:div :class "title" "Επεξεργασία")
                                           (actions company-form :filter filter)
                                           (notifications)
                                           (with-form (actions/company/update
                                                       :company-id (val company-id)
                                                       :search (val search))
                                             (display company-form :payload (params->payload)
                                                                   :styles (params->styles)))))
                               (:div :class "grid_6 omega"
                                     (:div :id "contact-window" :class "window"
                                           (:div :class "title" "Επαφές")
                                           (actions contact-table)
                                           (display contact-table)))))
               (footer)))))))

(defpage company-page actions/company/update ("actions/company/update"
                                              :request-type :post)
    ((search     string)
     (subset     string)
     (company-id integer chk-company-id)
     (title      string  (chk-company-title/update title company-id))
     (occupation string)
     (tof        string  chk-tof-title)
     (tin        string  (chk-tin/update tin company-id))
     (address    string)
     (city       string  chk-city-title)
     (pobox      integer chk-pobox)
     (zipcode    integer chk-zipcode)
     (notes      string))
  (with-controller-page (company/update)
    (let ((tof-id (tof-id (val tof)))
          (city-id (city-id (val city))))
      (execute (:update 'company :set
                        'title (val title)
                        'occupation (val occupation)
                        'tof-id tof-id
                        'tin (val tin)
                        'address (val address)
                        'city-id city-id
                        'pobox (val pobox)
                        'zipcode (val zipcode)
                        'notes (val notes)
                        :where (:= 'id (val company-id))))
      (see-other (apply #'company/details :company-id (val company-id)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage company-page company/delete ("company/delete")
    ((company-id integer chk-company-id/ref t)
     (search     string)
     (subset     string))
  (with-view-page
    (let* ((filter (params->filter))
           (company-table (make-instance 'company-table
                                         :op :delete
                                         :selected-key (val company-id)
                                         :filter filter)))
      (with-document ()
        (:head
         (:title "Εταιρία » Διαγραφή")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-top-actions :delete filter)
               (filters company-table)
               (:div :class "grid_12"
                     (:div :id "company-window" :class "window"
                           (:div :class "title" "Εταιρία » Διαγραφή")
                           (actions company-table)
                           (with-form (actions/company/delete :company-id (val company-id)
                                                              :search (val search))
                             (display company-table))))
               (footer)))))))

(defpage company-page actions/company/delete ("actions/company/delete"
                                              :request-type :post)
    ((company-id integer chk-company-id)
     (search     string)
     (subset     string))
  (with-controller-page (company/delete)
    (with-transaction ()
      (execute (:delete-from 'contact
                :where (:= 'company-id (val company-id))))
      (delete-dao (get-dao 'company (val company-id))))
    (see-other (apply #'company
                      (params->filter)))))
