(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass company-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id tx-id start))
   (payload-parameter-names
    :allocation :class
    :initform '(title occupation tof tin address city pobox zipcode notes))
   (filter-parameter-names
    :allocation :class
    :initform '(search subset))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))
   (messages
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



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(defun company-referenced-p (id)
  (with-db ()
    (or (query (:select 1 :from 'project
                :where (:= 'company-id id)))
        (query (:select 1 :from 'cheque
                :where (:= 'company-id id)))
        (query (:select 1 :from 'tx
                :where (:= 'company-id id))))))

(define-existence-predicate company-id-exists-p company id)
(define-existence-predicate contact-id-exists-p contact id)
(define-existence-predicate* company-title-exists-p company title id)
(define-existence-predicate* tin-exists-p company tin id)


(defun chk-company-id (id)
  (if (company-id-exists-p id)
      nil
      :company-id-unknown))

(defun chk-company-id/ref (id)
  (if (and (not (null id))
           (not (chk-company-id id))
           (not (company-referenced-p id)))
      nil
      :company-referenced))

(defun chk-company-title/create (title)
  (cond ((eql :null title) :company-title-null)
        ((company-title-exists-p title) :company-title-exists)
        (t nil)))

(defun chk-company-title/update (title id)
  (cond ((eql :null title) :company-title-null)
        ((company-title-exists-p title id) :company-title-exists)
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

(defun chk-tin/update (tin id)
  (cond ((eql :null tin) nil)
        ((tin-exists-p tin id)
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

(defun company-top-actions (op id filter &optional dates)
  (top-actions (make-instance 'menu
                              :spec `((catalogue
                                       ,(html ()
                                          (:a :href (apply #'company :id id filter)
                                              (:img :src "/scrooge/img/application_view_list.png")
                                              "Κατάλογος")))
                                      (create
                                       ,(html ()
                                          (:a :href (apply #'company/create filter)
                                              (:img :src "/scrooge/img/add.png")
                                              "Νέα Εταιρία")))
                                      (print
                                       ,(html ()
                                          (:a :href (apply #'company/details/transactions/print
                                                           :id id (append dates filter))
                                              (:img :src "/scrooge/img/printer.png")
                                              "Εκτύπωση"))))
                              :css-class "hmenu"
                              :disabled (ecase op
                                          ((:catalogue :delete) '(catalogue print))
                                          ((:create '(create print)))
                                          ((:update :details) '(print))
                                          ((:transactions) nil)))
               (searchbox #'actions/company/search
                          #'(lambda (&rest args)
                              (apply #'company :id id args))
                          filter
                          "ac-company")))

(defun company-actions (op id filter)
  (actions-menu (append (make-menu-spec
                         (action-anchors/crud+details (apply #'company/details :id id filter)
                                                      (apply #'company/update :id id filter)
                                                      (if (chk-company-id/ref id)
                                                          nil
                                                          (apply #'company/delete :id id filter))))
                        `((:create-project
                           ,(html ()
                              (:a :href (project/create :company (and id
                                                                      (title (get-dao 'company id))))
                                  :class "create"
                                  "Νέο Έργο")))))
                (enabled-actions/crud+details op id :create-project)))

(defun company-filters (filter)
  (let* ((filter* (remove-from-plist filter :subset))
         (filter-spec `(("nil"      ,(apply #'company filter*)
                                    "Όλες")
                        ("projects" ,(apply #'company :subset "projects" filter*)
                                    "Με ενεργά έργα")
                        ("debt"     ,(apply #'company :subset "debt" filter*)
                                    "Με χρεωστικό υπόλοιπο")
                        ("credit"   ,(apply #'company :subset "credit" filter*)
                                    "Με πιστωτικό υπόλοιπο"))))
    (filter-area (filter-navbar filter-spec
                                :active (getf filter :subset)))))

(defun company-tabs (id filter active content)
  (with-html
    (:div :class "grid_12"
          (:div :id "company-area"
                (when id
                  (htm (:div :id "company-title"
                             (:h3 :class "grid_12"
                                  (str (title (get-dao 'company id))))
                             (navbar
                              `((data ,(apply #'company/details :id id filter)
                                      "Στοιχεία")
                                (transactions ,(apply #'company/details/transactions :id id filter)
                                              "Συναλλαγές")
                                (cheques ,(apply #'company/details/cheques :id id filter)
                                         "Επιταγές"))
                              :css-class "hnavbar grid_5 prefix_7"
                              :active active
                              :id "company-tabs"
                              :css-class "hnavbar")
                             (clear))))
                (display content)
                (clear)))))

(defpage company-page actions/company/search ("actions/company/search" :request-type :get)
    ((search string))
  (with-db ()
    (let* ((filter (params->filter))
           (records (get-records (make-instance 'company-table
                                                :filter filter))))
      (if (or (not records)
              (and records (cdr records)))
          (see-other (apply #'company filter))
          (see-other (apply #'company/details :id (getf (first records) :id) filter))))))



;;; ------------------------------------------------------------
;;; Company form
;;; ------------------------------------------------------------

(defclass company-form (crud-form/plist)
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

(defmethod get-record ((type (eql 'company)) id)
  (declare (ignore type))
  (query (:select 'company.title 'occupation
                  'tin (:as 'tof.title 'tof)
                  'address (:as 'city.title 'city)
                  'zipcode 'pobox 'notes
                  :from 'company
                  :left-join 'city
                  :on (:= 'company.city-id 'city.id)
                  :left-join 'tof
                  :on (:=  'company.tof-id 'tof.id)
                  :where (:= 'company.id id))
         :plist))



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
         (push `(:= project.state "ongoing")
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
                                                      :on (:= cheque-event2.tx-id tx2.id)
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

;;; rows

(defclass company-row (scrooge-row/plist)
  ())

(defmethod selector ((row company-row) selected-p)
  (simple-selector row selected-p #'company))

(defmethod controls ((row company-row) controls-p)
  (simple-controls row controls-p #'company))

(defmethod payload ((row company-row) enabled-p)
  (let ((record (record row)))
    (list*
     (html ()
       (:a :href (apply #'company/details
                        :id (key row)
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



;;; ------------------------------------------------------------
;;; Company transactions table
;;; ------------------------------------------------------------

(defun company-debits (company-id &optional since until)
  (let ((base-query '(:select tx-date (:as tx.id tx-id) tx.description
                      (:as tx.amount debit-amount) cheque.due-date
                      :from tx
                      :left-join cheque-event
                      :on (:= cheque-event.tx-id tx.id)
                      :left-join cheque
                      :on (:= cheque.id cheque-event.cheque-id)))
        (where-base `(:= tx.company-id ,company-id))
        (where-tx `(:or (:= tx.credit-acc-id ,*cash-acc-id*)
                        (:in tx.credit-acc-id
                             (:set ,@*revenues-accounts*))
                        (:and (:= tx.credit-acc-id ,*cheque-payable-acc-id*)
                              (:not (:exists (:select 1
                                              :from (:as tx tx2)
                                              :inner-join (:as cheque-event cheque-event2)
                                              :on (:= cheque-event2.tx-id tx2.id)
                                              :where (:and
                                                      (:= cheque-event2.cheque-id
                                                          cheque-event.cheque-id)
                                                      (:= tx2.debit-acc-id
                                                          ,*cheque-payable-acc-id*))))))))
        (where-dates nil))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since tx-date) where-dates))
    (when (and until (not (eql until :null)))
      (push `(:<= tx-date ,until) where-dates))
    (let ((sql `(:order-by (,@base-query
                            :where (:and ,where-base
                                         ,where-tx
                                         ,@where-dates))
                           'tx.description)))
      (query (sql-compile sql)
             :plists))))

(defun company-credits (company-id &optional since until)
  (let ((base-query '(:select tx-date (:as tx.id tx-id) tx.description
                      (:as tx.amount credit-amount) cheque.due-date
                      :from tx
                      :left-join cheque-event
                      :on (:= cheque-event.tx-id tx.id)
                      :left-join cheque
                      :on (:= cheque.id cheque-event.cheque-id)))
        (where-base `(:= tx.company-id ,company-id))
        (where-tx `(:or (:= tx.debit-acc-id ,*cash-acc-id*)
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
                                                          ,*cheque-receivable-acc-id*))))))))
        (where-dates nil))
    (when (and since (not (eql since :null)))
      (push `(:<= ,since tx-date) where-dates))
    (when (and until (not (eql until :null)))
      (push `(:<= tx-date ,until) where-dates))
    (let ((sql `(:order-by (,@base-query
                            :where (:and ,where-base
                                         ,where-tx
                                         ,@where-dates))
                           'tx.description)))
      (query (sql-compile sql)
             :plists))))

(defun company-debits/credits (company-id since until)
  (flet ((get-date (row)
           (getf row :tx-date)))
    (let* ((sorted (stable-sort (nconc (company-debits company-id)
                                       (company-credits company-id))
                                #'local-time:timestamp<
                                :key #'get-date))
           (truncated (remove-if (lambda (row)
                                   (or (and since
                                            (not (eql since :null))
                                            (timestamp< (get-date row) since))
                                       (and until
                                            (not (eql until :null))
                                            (timestamp> (get-date row) until))))
                                 sorted)))

      (let ((total 0)
            (debit-sum 0)
            (credit-sum 0))
        (dolist (row sorted)
          (let* ((debit (getf row :debit-amount 0))
                 (credit (getf row :credit-amount 0))
                 (delta (- debit credit)))
            (setf total (+ total delta))
            (setf debit-sum (+ debit-sum debit))
            (setf credit-sum (+ credit-sum credit))
            (unless (eql (getf row :due-date) :null)
              (setf (getf row :description)
                    (concatenate 'string
                                 (getf row :description)
                                 " (Λήξη: "
                                 (lisp->html (getf row :due-date))
                                 ")")))
            (nconc row (list :total total))))
        (values (nreverse truncated) debit-sum credit-sum total)))))



;;; table

(defclass company-tx-table (scrooge-table)
  ((header-labels  :initform '("" "Ημερομηνία" "Περιγραφή" "Χρέωση" "Πίστωση" "Υπόλοιπο" ""))
   (paginator      :initform (make-instance 'company-tx-paginator
                                            :id "company-tx-paginator"
                                            :css-class "paginator")
                   :initarg :paginator)
   (company-id     :accessor company-id :initarg :company-id))
  (:default-initargs :item-class 'company-tx-row :id "company-tx-table"))


;;; rows

(defclass company-tx-row (scrooge-row/plist)
  ())

(defmethod key ((row company-tx-row))
  (getf (record row) :tx-id))

(defmethod selector ((row company-tx-row) selected-p)
  (let* ((table (collection row))
         (tx-id (key row))
         (company-id (company-id table))
         (filter (filter table))
         (start (page-start (paginator table) (index row) (start-index table))))
    (html ()
      (:a :href (if selected-p
                    (apply #'company/details/transactions :id company-id
                                                          :start start filter)
                    (apply #'company/details/transactions :id company-id
                                                          :tx-id tx-id filter))
          (selector-img selected-p)))))

(defmethod controls ((row company-tx-row) controls-p)
  (simple-controls row controls-p #'company/details/transactions))

(defmethod payload ((row company-tx-row) enabled-p)
  (let ((record (record row)))
    (mapcar (lambda (name)
              (make-instance 'textbox
                             :name name
                             :value (getf record (make-keyword name))
                             :disabled (not enabled-p)))
            '(tx-date description debit-amount credit-amount total))))


;;; paginator

(defclass company-tx-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg company-tx-paginator) start)
  (apply #'company/details/transactions :start start (filter (table pg))))



;;; ------------------------------------------------------------
;;; Company cheques table
;;; ------------------------------------------------------------

(defclass company-cheques-table (cheque-table)
  ((paginator :accessor paginator :initarg :paginator))
  (:default-initargs :item-class 'company-cheques-row
                     :paginator (make-instance 'company-cheques-paginator
                                               :id "cheque-paginator"
                                               :css-class "paginator")))

(defclass company-cheques-row (cheque-row)
  ())

(defclass company-cheques-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg company-cheques-paginator) start)
  (let ((table (table pg)))
    (apply #'company/details/cheques (kind table) :start start (filter table))))

(defmethod selector ((row company-cheques-row) selected-p)
  nil)



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage company-page company ("company")
    ((id     integer chk-company-id)
     (search string)
     (subset string)
     (start  integer))
  (with-view-page
    (let* ((op :catalogue)
           (filter (params->filter))
           (company-table (make-instance 'company-table
                                         :op op
                                         :filter filter
                                         :start-index (val start))))
      ;; if id exists and is not found among records, ignore search term
      (when (and (val id)
                 (not (find (val id) (rows company-table) :key #'key)))
        (see-other (company :id (val id))))
      (with-document ()
        (:head
         (:title "Εταιρίες » Κατάλογος")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-top-actions op (val id) filter)
               (company-filters filter)
               (:div :class "grid_12"
                     (:div :id "company-window" :class "window"
                           (:div :class "title"  "Κατάλογος")
                           (company-actions op (val id) filter)
                           (display company-table
                                    :key (val id))))
               (footer)))))))

(defpage company-page company/details ("company/details")
    ((search     string)
     (subset     string)
     (id         integer chk-company-id t)
     (contact-id integer (chk-contact-id id contact-id)))
  (with-view-page
    (let* ((filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op :details
                                        :record (get-record 'company (val id))
                                        :cancel-url (apply #'company :id (val id) filter)))
           (contact-table (make-instance 'contact-table
                                         :op :catalogue
                                         :company-id (val id))))
      (with-document ()
        (:head
         (:title "Εταιρία » Λεπτομέρειες")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-top-actions :details (val id) filter)
               (company-tabs (val id) filter 'data
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "company-window" :class "window"
                                           (:div :class "title" "Λεπτομέρειες")
                                           (company-actions :details (val id) filter)
                                           (display company-form)))
                               (:div :class "grid_6 omega"
                                     (:div :id "contact-window" :class "window"
                                           (:div :class "title" "Επαφές")
                                           (contact-actions :catalogue
                                                            (val id)
                                                            (val contact-id)
                                                            filter)
                                           (display contact-table
                                                    :key (val contact-id))))))
               (footer)))))))

(defpage company-page company/details/cheques ("company/details/cheques")
    ((search    string)
     (subset    string)
     (id        integer chk-company-id t)
     (cheque-id integer)
     (since     date)
     (until     date)
     (start     integer)
     (cstate    string))
  (with-view-page
    (let* ((filter (params->filter))
           (system (params->plist #'val-or-raw (list id cheque-id)))
           (dates  (params->plist #'val-or-raw (list since until)))
           (payable-table (make-instance 'company-cheques-table
                                         :op :details
                                         :filter (append system filter dates)
                                         :start-index (val start)
                                         :kind "payable"))
           (receivable-table (make-instance 'company-cheques-table
                                            :op :details
                                            :filter (append system filter dates)
                                            :start-index (val start)
                                            :kind "receivable")))
      (with-document ()
        (:head
         (:title "Εταιρία » Λεπτομέρειες » Επιταγές")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-top-actions :transactions (val id) filter dates)
               (company-tabs (val id) filter 'cheques
                             (html ()
                               (:div :class "secondary-filter-area"
                                     (company-cheques-filters (append system filter dates)))
                               (:div :id "company-tx-window"
                                     (when (records payable-table)
                                       (htm (:div :class "window"
                                                  (:div :class "title" "Προς είσπραξη")
                                                  (display payable-table
                                                           :key (val cheque-id)))))
                                     (when (records receivable-table)
                                       (htm (:div :class "window"
                                                  (:div :class "title" "Προς πληρωμή")
                                                  (display receivable-table
                                                           :key (val cheque-id))))))))
               (footer)))))))

(defun company-cheques-filters (filter)
  (let* ((url-fn #'company/details/cheques)
         (filter* (remove-from-plist filter :cstate))
         (filter-spec `((nil      ,(apply url-fn filter*)
                                  "Όλες")
                        (pending  ,(apply url-fn :cstate "pending" filter*)
                                  "Σε εκκρεμότητα")
                        (paid     ,(apply url-fn :cstate "paid" filter*)
                                  "Πληρωμένες")
                        (bounced  ,(apply url-fn :cstate "bounced" filter*)
                                  "Ακάλυπτες")
                        (returned ,(apply url-fn :cstate "returned" filter*)
                                  "Επιστραμμένες")
                        (stamped  ,(apply url-fn :cstate "stamped" filter*)
                                  "Σφραγισμένες"))))
    (display (filter-navbar filter-spec
                            :active (getf filter :cstate)))
    (display (datebox (lambda (&rest args)
                        (apply url-fn args))
                      filter))))

(defpage company-page company/details/transactions ("company/details/transactions")
    ((search string)
     (subset string)
     (id     integer chk-company-id t)
     (tx-id  integer)
     (since  date)
     (until  date)
     (start  integer))
  (with-view-page
    (let ((filter (params->filter))
          (system (params->plist #'val-or-raw (list id tx-id)))
          (dates  (params->plist #'val-or-raw (list since until))))
      (multiple-value-bind (records debit-sum credit-sum total)
          (company-debits/credits (val id) (val since) (val until))
        (with-document ()
          (:head
           (:title "Εταιρία » Λεπτομέρειες » Συναλλαγές")
           (main-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header)
                 (main-navbar 'company)
                 (company-top-actions :transactions (val id) filter dates)
                 (company-tabs (val id) filter 'transactions
                               (html ()
                                 (:div :class "secondary-filter-area"
                                       (display (datebox #'company/details/transactions
                                                         (append system filter dates))))
                                 (:div :id "company-tx-window" :class "window"
                                       (:div :class "title" "Συναλλαγές")
                                       (display (make-instance 'company-tx-table
                                                               :records records
                                                               :company-id (val id)
                                                               :op :details
                                                               :filter (append system filter dates)
                                                               :start-index (val start))
                                                :key (val tx-id))
                                       (:h4 "Σύνολο Χρεώσεων: " (fmt "~9,2F" debit-sum))
                                       (:h4 "Σύνολο Πιστώσεων: " (fmt "~9,2F" credit-sum))
                                       (:h4 "Γενικό Σύνολο: " (fmt "~9,2F" total)))))
                 (footer))))))))

(defpage company-page company/details/transactions/print ("company/details/transactions/print")
    ((search string)
     (subset string)
     (id     integer chk-company-id t)
     (tx-id  integer)
     (since  date)
     (until  date))
  (with-view-page
    (let ((filter (params->filter))
          (system (params->plist #'val-or-raw (list id tx-id)))
          (dates  (params->plist #'val-or-raw (list since until))))
      (multiple-value-bind (records debit-sum credit-sum)
          (company-debits/credits (val id) (val since) (val until))
        (with-document ()
          (:head
           (:title "Εταιρία » Λεπτομέρειες » Συναλλαγές » Εκτύπωση")
           (print-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (:div :class "grid_12"
                       (:a :id "back"
                           :href (apply #'company/details/transactions (append system filter dates))
                           "« Επιστροφή")
                       (:div :id "company-tx-window" :class "window"
                             (:div :class "title"
                                   (:h3 (str (title (get-dao 'company (val id)))))
                                   (display (datebox #'company/details/transactions/print
                                                     (append system filter dates))))
                             (display (make-instance 'company-tx-table
                                                     :records records
                                                     :company-id (val id)
                                                     :op :details
                                                     :filter (append system filter dates)
                                                     :paginator nil))
                             (:h4 "Σύνολο χρεώσεων: " (fmt "~9,2F" debit-sum))
                             (:h4 "Σύνολο πιστώσεων: " (fmt "~9,2F" credit-sum))
                             (:h4 "Γενικό Σύνολο: " (fmt "~9,2F" (- debit-sum
                                                                    credit-sum))))))))))))



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
    (let* ((op :create)
           (filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op op
                                        :record nil
                                        :cancel-url (apply #'company filter))))
      (with-document ()
        (:head
         (:title "Εταιρία » Δημιουργία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-top-actions op nil filter)
               (company-tabs nil filter 'data
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "company-window" :class "window"
                                           (:div :class "title" "Νέα εταιρία")
                                           (company-actions op nil filter)
                                           (notifications)
                                           (with-form (actions/company/create :search (val search))
                                             (display company-form :payload (params->payload)
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
      (see-other (apply #'company/details :id (company-id new-company)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage company-page company/update ("company/update")
    ((search     string)
     (subset     string)
     (id         integer chk-company-id t)
     (contact-id integer (chk-contact-id id contact-id))
     (title      string  (chk-company-title/update title id))
     (occupation string)
     (tof        string  chk-tof-title)
     (tin        string  (chk-tin/update tin id))
     (address    string)
     (city       string  chk-city-title)
     (pobox      integer chk-pobox)
     (zipcode    integer chk-zipcode)
     (notes      string))
  (with-view-page
    (let* ((op :update)
           (filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op op
                                        :record (get-record 'company (val id))
                                        :cancel-url (apply #'company/details :id (val id) filter)))
           (contact-table (make-instance 'contact-table
                                         :op :catalogue
                                         :company-id (val id))))
      (with-document ()
        (:head
         (:title "Εταιρία » Επεξεργασία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-top-actions op (val id) filter)
               (company-tabs (val id) filter 'data
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "company-window" :class "window"
                                           (:div :class "title" "Επεξεργασία")
                                           (company-actions :update (val id) filter)
                                           (notifications)
                                           (with-form (actions/company/update :id (val id)
                                                                              :search (val search))
                                             (display company-form :payload (params->payload)
                                                                   :styles (params->styles)))))
                               (:div :class "grid_6 omega"
                                     (:div :id "contact-window" :class "window"
                                           (:div :class "title" "Επαφές")
                                           (contact-actions :catalogue
                                                            (val id)
                                                            (val contact-id)
                                                            filter)
                                           (display contact-table
                                                    :key (val contact-id))))))
               (footer)))))))

(defpage company-page actions/company/update ("actions/company/update"
                                              :request-type :post)
    ((search     string)
     (subset     string)
     (id         integer chk-company-id)
     (title      string  (chk-company-title/update title id))
     (occupation string)
     (tof        string  chk-tof-title)
     (tin        string  (chk-tin/update tin id))
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
                        :where (:= 'id (val id))))
      (see-other (apply #'company/details :id (val id)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage company-page company/delete ("company/delete")
    ((id     integer chk-company-id/ref t)
     (search string)
     (subset string))
  (with-view-page
    (let* ((op :delete)
           (filter (params->filter))
           (company-table (make-instance 'company-table
                                         :op op
                                         :filter filter)))
      (with-document ()
        (:head
         (:title "Εταιρία » Διαγραφή")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-top-actions op (val id) filter)
               (company-filters filter)
               (:div :class "grid_12"
                     (:div :id "company-window" :class "window"
                           (:div :class "title" "Εταιρία » Διαγραφή")
                           (company-actions op nil filter)
                           (with-form (actions/company/delete :id (val id)
                                                              :search (val search))
                             (display company-table
                                      :key (val id)))))
               (footer)))))))

(defpage company-page actions/company/delete ("actions/company/delete"
                                              :request-type :post)
    ((id     integer chk-company-id)
     (search string)
     (subset string))
  (with-controller-page (company/delete)
    (with-transaction ()
      (execute (:delete-from 'contact
                :where (:= 'company-id (val id))))
      (delete-dao (get-dao 'company (val id))))
    (see-other (apply #'company
                      (params->filter)))))
