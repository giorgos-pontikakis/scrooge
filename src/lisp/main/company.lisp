(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass company-family (family-mixin)
  ()
  (:default-initargs
   :parameter-groups '(:system (company-id contact-id start
                                subset)  ;; checked filter parameter
                       :payload (title occupation tof tin address city pobox zipcode notes
                                 revenues-account expenses-account immediate-tx-only-p)
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
                  "Μη αποδεκτός ταχυδρομικός κωδικός."))
                (revenues-account
                 (:not-revenues-account
                  "Άγνωστος λογαριασμός εσόδων."
                  :account-title-null
                  "Το όνομα λογαριασμού είναι κενό."
                  :account-title-unknown
                  "Άγνωστος λογαριασμός."))
                (expenses-account
                 (:not-expenses-account
                  "Άγνωστος λογαριασμός εξόδων."
                  :account-title-null
                  "Το όνομα λογαριασμού είναι κενό."
                  :account-title-unknown
                  "Άγνωστος λογαριασμός."))))))



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(defun company-referenced-p (company-id)
  (or (referenced-by company-id 'project 'company-id)
      (referenced-by company-id 'cheque 'company-id)
      (referenced-by company-id 'tx 'company-id)))

(define-existence-predicate company-id-exists-p company id)
(define-existence-predicate contact-id-exists-p contact id)
(define-existence-predicate* company-title-exists-p company title id)
(define-existence-predicate* tin-exists-p company tin id)

(defun chk-company-id (company-id)
  (cond ((eql :null company-id)
         :company-id-null)
        ((not (company-id-exists-p company-id))
         :account-id-unknown)))

(defun chk-company-id/ref (company-id)
  (cond ((chk-company-id company-id))
        ((company-referenced-p company-id) :company-referenced)))

(defun chk-company-title/create (title)
  (cond ((eql :null title) :company-title-null)
        ((company-title-exists-p title) :company-title-exists)))

(defun chk-company-title/update (title company-id)
  (cond ((eql :null title) :company-title-null)
        ((company-title-exists-p title company-id) :company-title-exists)))

(defun chk-company-title (title)
  (cond ((eql :null title) :company-title-null)
        ((not (company-title-exists-p title)) :company-title-unknown)))

(defun chk-company-title/cash (title)
  (or (chk-company-title title)
      (if (with-db ()
            (query (:select 'immediate-tx-only-p :from 'company :where (:= 'title title))
                   :single!))
          :company-immediate-tx-only
          nil)))

(defun chk-tin/create (tin)
  (cond ((eql :null tin) nil)
        ((tin-exists-p tin)
         :tin-exists)
        ((not (valid-tin-p tin))
         :tin-invalid)))

(defun chk-tin/update (tin company-id)
  (cond ((eql :null tin) nil)
        ((tin-exists-p tin company-id)
         :tin-exists)
        ((not (valid-tin-p tin))
         :tin-invalid)))

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

(defun chk-subset (subset)
  (if (or (eql subset :null)
          (member subset '("projects" "debit" "credit") :test #'string-equal))
      nil
      :invalid-subset))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun company-top-actions (op)
  (let ((print-url (case op
                     (:details 'company/details/print)
                     (:catalogue 'company/print))))
    (top-actions-area
     (make-instance 'scrooge-menu
                    :spec (make-menu-spec
                           `(:catalogue ,(family-url 'company :system :filter)
                             :create (,(family-url 'company/create :filter) "Νέα Εταιρία")
                             :print ,(family-url print-url :system :filter)))
                    :css-class "hmenu"
                    :disabled (case op
                                (:catalogue '(:catalogue))
                                ((:create :update :delete) '(:create :print))))
     (searchbox (family-url-fn 'actions/company/search)
                (family-url-fn 'company :system)
                (family-params 'company :filter)
                "ac-company"))))

(defun company-tabs (company-id filter active content)
  (declare (ignore filter))
  (let ((spec `((data ,(family-url 'company/details :system :filter)
                      "Στοιχεία")
                (tx ,(family-url 'company/tx :system :filter)
                    "Συναλλαγές")
                (cheque ,(apply #'company/cheque "customer" :cstate "pending"
                                (family-params 'company/cheque :system))
                        "Επιταγές"))))
    (with-html
      (:div :class "grid_12"
            (:div :id "company-area"
                  (when company-id
                    (htm (:div :id "company-title"
                               (:h2 :class "grid_12 alpha"
                                    (str (title (get-dao 'company company-id))))
                               (display
                                (make-instance 'navbar :spec spec
                                                       :css-class "hnavbar grid_5 prefix_7"
                                                       :active active
                                                       :id "company-tabs"
                                                       :css-class "hnavbar"))
                               (clear))))
                  (display content)
                  (clear))))))



;;; ------------------------------------------------------------
;;; Company form
;;; ------------------------------------------------------------

(defclass company-form (scrooge-crud-form company-family)
  ()
  (:default-initargs :record-class 'cons))

(defmethod display ((form company-form) &key styles)
  (let* ((disabled (eql (op form) :read))
         (record (record form))
         (ft (factory #'make-instance 'form-textbox :disabled disabled :record record :styles styles)))
    (with-html
      (:div :class "data-form company-form"
            (:div :class "company-form-title"
                  (:label "Επωνυμία"
                          (call ft :name 'title)))
            (:div :class "form-group"
                  (:label "Επάγγελμα"
                          (call ft :name 'occupation
                                   :css-class-enabled "ac-occupation"))
                  (:div :id "tin"
                        (:label "Α.Φ.Μ."
                                (call ft :name 'tin)))
                  (:div :id "tof"
                        (:label "Δ.Ο.Υ."
                                (call ft :name 'tof
                                         :css-class-enabled "ac-tof")))
                  (clear))
            (:div :class "form-group"
                  (:label "Διεύθυνση"
                          (call ft :name 'address))
                  (:label "Πόλη"
                          (call ft :name 'city
                                   :css-class-enabled "ac-city"))
                  (:div :id "zipcode"
                        (:label "Ταχυδρομικός κωδικός"
                                (call ft :name 'zipcode)))
                  (:div :id "pobox"
                        (:label "Ταχυδρομική θυρίδα"
                                (call ft :name 'pobox)))
                  (clear))
            (:div :class "form-group advanced"
                  (:label "Λογαριασμός εσόδων"
                          (call ft :name 'revenues-account
                                   :value (title
                                           (get-dao 'account
                                                    (account-id 'revenues-root-account)))
                                   :css-class-enabled "ac-revenues"))
                  (:label "Λογαριασμός εξόδων"
                          (call ft :name 'expenses-account
                                   :value (title
                                           (get-dao 'account
                                                    (account-id 'expenses-root-account)))
                                   :css-class-enabled "ac-expenses"))
                  (:label "Τρόπος πληρωμής"
                          (obj 'dropdown :name 'immediate-tx-only-p
                                         :value-label-alist '((nil . "Μέσω ανοιχτού λογαριασμού")
                                                              (t . "Απ' ευθείας εξόφληση"))
                                         :disabled disabled
                                         :selected (getf record :immediate-tx-only-p nil))))
            (:div :class "form-group"
                  (:label "Σημειώσεις"
                          (:textarea :name 'notes
                                     :disabled disabled
                                     (str (lisp->html (or (getf record :notes) :null))))))
            (:div :class "data-form-buttons"
                  (unless disabled
                    (ok-button :body (if (eql (op form) :update) "Ανανέωση" "Δημιουργία"))
                    (cancel-button (cancel-url form) :body "Άκυρο")))))))

(defmethod get-record ((form company-form))
  (let ((company-id (key form)))
    (query (:select 'company.title 'occupation
                    'tin (:as 'tof.title 'tof)
                    'address (:as 'city.title 'city)
                    'zipcode 'pobox 'notes 'immediate-tx-only-p
                    (:as 'revenues-account.title 'revenues-account)
                    (:as 'expenses-account.title 'expenses-account)
                    :from 'company
                    :left-join 'city
                    :on (:= 'company.city-id 'city.id)
                    :left-join 'tof
                    :on (:= 'company.tof-id 'tof.id)
                    :left-join (:as 'account 'revenues-account)
                    :on (:= 'company.revenues-account-id 'revenues-account.id)
                    :left-join (:as 'account 'expenses-account)
                    :on (:= 'company.expenses-account-id 'expenses-account.id)
                    :where (:= 'company.id company-id))
           :plist)))


(defmethod actions ((form company-form) &key filter)
  (let* ((company-id (key form))
         (spec (if company-id
                   `(:update ,(apply #'company/update :company-id company-id filter)
                     :delete ,(if (company-referenced-p company-id)
                                  nil
                                  (apply #'company/delete :company-id company-id filter))
                     :create-project (,(project/create
                                        :company (title (get-dao 'company company-id)))
                                      "Νέο Έργο"
                                      "create"))
                   nil)))
    (actions-menu (make-menu-spec spec)
                  (disabled-actions form))))

(defmethod disabled-actions ((form company-form) &key)
  (ecase (op form)
    (:read '())
    ((:create :update :delete) '(:create-project :update :delete))))



;;; ------------------------------------------------------------
;;; Company table
;;; ------------------------------------------------------------

;;; table

(defclass company-table (scrooge-crud-table)
  ()
  (:default-initargs :record-class 'cons
                     :item-class 'company-row
                     :paginator (make-instance 'company-paginator)
                     :id "company-table"
                     :header-labels '("" "Επωνυμία" "Α.Φ.Μ." "Δ.Ο.Υ." "Ισοζύγιο" "" "")))

(defmethod get-records ((table company-table))
  (let* ((search (getf (filter table) :search))
         (subset (getf (filter table) :subset))
         (subset-dc-p (and subset
                           (member subset (list "credit" "debit") :test #'string=)))
         (ilike-term (ilike search))
         (base-query `(:select company.id company.title tin
                               (:as tof.title tof)
                               address occupation
                               (:as city.title city-name)
                               ,@(if subset-dc-p
                                     '((:as (:select (company-balance company.id)) balance))
                                     nil) ;; SQL function
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
         (where nil)
         (order '(company.title)))
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
        (subset-dc-p
         (if (string= subset "debit")
             (progn (push `(:< ,*company-tx-significant-amount* (company-balance company.id))
                          where)
                    (push '(:desc balance) order))
             (progn (push `(:< (company-balance company.id) ,(* -1 *company-tx-significant-amount*))
                          where)
                    (push 'balance order))))))
    (let ((sql `(:order-by (,@base-query :where (:and t ,@where))
                           ,@order)))
      (query (sql-compile sql)
             :plists))))

(defmethod extra-info ((table company-table))
  (when (records table)
    (with-html
      (:div :class "window-footer"
            (:h4 "Σύνολο: " (str (fmt-amount (reduce #'+
                                                     (mapcar (getfer :balance 0)
                                                             (records table))))))))))

(defmethod actions ((tbl company-table) &key)
  (let* ((company-id (selected-key tbl))
         (filter (filter tbl))
         (hrefs (if company-id
                    `(:details ,(apply #'company/details :company-id company-id filter)
                      :delete ,(if (company-referenced-p company-id)
                                   nil
                                   (apply #'company/delete :company-id company-id filter))
                      :create-project (,(project/create
                                         :company (title (get-dao 'company company-id)))
                                       "Νέο Έργο"
                                       "create"))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions tbl))))

(defmethod disabled-actions ((tbl company-table) &key)
  (ecase (op tbl)
    (:read '())
    (:delete '(:details :delete :create-project))))

(defmethod filters ((tbl company-table))
  (let* ((filter* (remove-from-plist (filter tbl) :subset))
         (filter-spec `(("nil"      ,(apply #'company filter*)
                                    "Όλες")
                        ("projects" ,(apply #'company :subset "projects" filter*)
                                    "Με ενεργά έργα")
                        ("debit"    ,(apply #'company :subset "debit" filter*)
                                    "Χρεωστικές")
                        ("credit"   ,(apply #'company :subset "credit" filter*)
                                    "Πιστωτικές"))))
    (filter-area (filter-navbar filter-spec
                                :active (getf (filter tbl) :subset)))))


;;; rows

(defclass company-row (scrooge-row)
  ())

(defmethod selector ((row company-row) selected-p)
  (simple-selector row selected-p #'company :company-id))

(defmethod controls ((row company-row) controls-p)
  (simple-controls row controls-p #'company :company-id))

(defmethod payload ((row company-row) enabled-p)
  (let ((factory (factory #'make-instance 'table-textbox :enabled enabled-p :record (record row))))
    (mapply factory
            `((:name title :href ,(apply #'company/details :company-id (key row)
                                         (filter (collection row))))
              (:name tin)
              (:name tof)
              (:name balance :format-fn ,(compose #'fmt-amount #'abs))))))

(defmethod display ((table company-table) &key payload)
  (unless (member (getf (filter table) :subset) (list "credit" "debit") :test #'string=)
    (let* ((keys (mapcar #'key (rows table)))
           (balances (query (:select 'id
                                     (:as (:select (:company-balance 'id)) 'balance)
                                     :from 'company
                                     :where (:in 'id (:set keys)))
                            :plists)))
      (mapc (lambda (row)
              (let ((balance (getf (find (getf (record row) :id) balances :key (getfer :id))
                                   :balance)))
                (setf (record row)
                      (nconc (list :balance balance) (record row)))))
            (rows table))))
  (call-next-method table :payload payload))


;;; paginator

(defclass company-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg company-paginator) start)
  (apply #'company :start start (filter (table pg))))



;;; ----------------------------------------------------------------------
;;; SEARCH & AJAX
;;; ----------------------------------------------------------------------

(defpage company-page actions/company/search ("actions/company/search" :request-type :get)
    ((search string)
     (subset string chk-subset))
  (with-db ()
    (let* ((filter (params->filter))
           (company-table (make-instance 'company-table :op :read
                                                        :filter filter))
           (records (records company-table)))
      (if (single-item-list-p records)
          (see-other (apply #'company/details
                            :company-id (get-key (first records))
                            filter))
          (see-other (apply #'company filter))))))

(defpage company-page company/accounts
    ("company/accounts" :content-type "text/plain"
                        :parameter-groups '(:system (title)))
    ((title string chk-company-title t))
  (with-xhr-page (autocomplete-xhr-auth-error)
    (with-output-to-string (*standard-output*)
      (let ((company (get-dao 'company (company-id (val title)))))
        (write-json (alist-hash-table
                     `(("immediateTxOnly" . ,(immediate-tx-only-p company))
                       ("accountIDs" . ,(vector (account-id 'revenues-root-account)
                                                (vector (revenues-account-id company)
                                                        (expenses-account-id company)))))))))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage company-page company ("company")
    ((company-id integer chk-company-id)
     (start      integer)
     (search     string)
     (subset     string  chk-subset))
  (with-view-page
    (let* ((filter (params->filter))
           (company-table (make-instance 'company-table
                                         :op :read
                                         :selected-key (val company-id)
                                         :filter filter
                                         :start-index (val start))))
      (check-id-inclusion company-table
                          (val company-id)
                          (apply #'company filter))
      (with-document ()
        (:head
         (:title "Εταιρίες » Κατάλογος")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'main)
               (navbar 'main 'company)
               (company-top-actions :catalogue)
               (filters company-table)
               (:div :class "grid_12"
                     (:div :id "company-window" :class "window"
                           (:div :class "title"  "Κατάλογος")
                           (actions company-table)
                           (display company-table)
                           (extra-info company-table)))
               (footer)))))))

(defpage company-page company/print ("company/print")
    ((company-id integer chk-company-id)
     (start      integer)
     (search     string)
     (subset     string  chk-subset))
  (with-view-page
    (let* ((filter (params->filter))
           (company-table (make-instance 'company-table
                                         :op :read
                                         :selected-key nil
                                         :filter filter
                                         :start-index (val start)
                                         :paginator nil)))
      (with-document ()
        (:head
         (:title "Εταιρίες » Κατάλογος » Εκτύπωση")
         (print-headers))
        (:body
         (:div :id "container" :class "container_12"
               (:div :class "grid_12"
                     (back (family-url 'company :system :filter))
                     (:div :id "company-window" :class "window"
                           (:div :class "title"
                                 (:h2 (str (conc (if (string= (val subset) "debit") "Χρεωστικές" "Πιστωτικές")
                                                 " Εταιρίες"))))
                           (display company-table)
                           (extra-info company-table))
                     (print-pages-footer))))))))

(defpage company-page company/details ("company/details")
    ((company-id integer chk-company-id                         t)
     (contact-id integer (chk-contact-id company-id contact-id))
     (search     string)
     (subset     string  chk-subset))
  (with-view-page
    (let* ((filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op :read
                                        :key (val company-id)
                                        :cancel-url (apply #'company
                                                           :company-id (val company-id)
                                                           filter)))
           (contact-table (make-instance 'contact-table
                                         :op :read
                                         :selected-key (val contact-id)
                                         :company-id (val company-id))))
      (with-document ()
        (:head
         (:title "Εταιρία » Λεπτομέρειες » Στοιχεία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'main)
               (navbar 'main 'company)
               (company-top-actions :details)
               (company-tabs (val company-id) filter 'data
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "company-window" :class "window"
                                           (:div :class "title" "Στοιχεία")
                                           (actions company-form :filter filter)
                                           (display company-form)))
                               (:div :class "grid_6 omega"
                                     (:div :id "contact-window" :class "window"
                                           (:div :class "title" "Επαφές")
                                           (actions contact-table)
                                           (display contact-table)))))
               (footer)))))))

(defpage company-page company/details/print ("company/details/print")
    ((company-id integer chk-company-id t)
     (search     string)
     (subset     string  chk-subset))
  (with-view-page
    (let* ((filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op :read
                                        :key (val company-id)
                                        :cancel-url (apply #'company
                                                           :company-id (val company-id)
                                                           filter)))
           (contact-table (make-instance 'contact-table
                                         :op :read
                                         :company-id (val company-id))))
      (with-document ()
        (:head
         (:title "Εταιρία » Λεπτομέρειες » Στοιχεία » Εκτύπωση")
         (print-headers))
        (:body
         (:div :id "container" :class "container_12"
               (:div :class "grid_12"
                     (back (family-url 'company/details :system :filter)))
               (company-tabs (val company-id) filter 'data
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "company-window" :class "window"
                                           (:div :class "title" "Στοιχεία")
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
    ((title               string  chk-company-title/create)
     (occupation          string)
     (tof                 string  chk-tof-title)
     (tin                 string  chk-tin/create)
     (address             string)
     (city                string  chk-city-title)
     (pobox               integer chk-pobox)
     (zipcode             integer chk-zipcode)
     (notes               string)
     (search              string)
     (subset              string  chk-subset)
     (revenues-account    string  chk-revenues-account-title)
     (expenses-account    string  chk-expenses-account-title)
     (immediate-tx-only-p boolean))
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
               (header 'main)
               (navbar 'main 'company)
               (company-top-actions :create)
               (company-tabs nil filter 'data
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "company-window" :class "window"
                                           (:div :class "title" "Νέα εταιρία")
                                           (actions company-form)
                                           (notifications)
                                           (with-form (actions/company/create :search (val search))
                                             (display company-form
                                                      :payload (params->payload)
                                                      :styles (params->styles)))))))
               (footer)))))))

(defpage company-page actions/company/create ("actions/company/create"
                                              :request-type :post)
    ((search              string)
     (subset              string  chk-subset)
     (title               string  chk-company-title/create t)
     (occupation          string)
     (tof                 string  chk-tof-title)
     (tin                 string  chk-tin/create)
     (address             string)
     (city                string  chk-city-title)
     (pobox               integer chk-pobox)
     (zipcode             integer chk-zipcode)
     (notes               string)
     (revenues-account    string  chk-revenues-account-title)
     (expenses-account    string  chk-expenses-account-title)
     (immediate-tx-only-p boolean))
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
                                       :notes (val notes)
                                       :revenues-account-id (account-id (val revenues-account))
                                       :expenses-account-id (account-id (val expenses-account))
                                       :immediate-tx-only-p (val immediate-tx-only-p))))
      (insert-dao new-company)
      (see-other (apply #'company/details :company-id (company-id new-company)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage company-page company/update ("company/update")
    ((company-id          integer chk-company-id                              t)
     (contact-id          integer (chk-contact-id company-id contact-id))
     (title               string  (chk-company-title/update title company-id))
     (occupation          string)
     (tof                 string  chk-tof-title)
     (tin                 string  (chk-tin/update tin company-id))
     (address             string)
     (city                string  chk-city-title)
     (pobox               integer chk-pobox)
     (zipcode             integer chk-zipcode)
     (notes               string)
     (search              string)
     (subset              string  chk-subset)
     (revenues-account    string  chk-revenues-account-title)
     (expenses-account    string  chk-expenses-account-title)
     (immediate-tx-only-p boolean))
  (with-view-page
    (let* ((filter (params->filter))
           (company-form (make-instance 'company-form
                                        :op :update
                                        :key (val company-id)
                                        :cancel-url (apply #'company/details
                                                           :company-id (val company-id) filter)))
           (contact-table (make-instance 'contact-table
                                         :op :read
                                         :selected-key (val contact-id)
                                         :company-id (val company-id))))
      (with-document ()
        (:head
         (:title "Εταιρία » Επεξεργασία")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'main)
               (navbar 'main 'company)
               (company-top-actions :update)
               (company-tabs (val company-id) filter 'data
                             (html ()
                               (:div :class "grid_6 alpha"
                                     (:div :id "company-window" :class "window"
                                           (:div :class "title" "Στοιχεία » Επεξεργασία")
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
    ((company-id          integer chk-company-id)
     (title               string  (chk-company-title/update title company-id))
     (occupation          string)
     (tof                 string  chk-tof-title)
     (tin                 string  (chk-tin/update tin company-id))
     (address             string)
     (city                string  chk-city-title)
     (pobox               integer chk-pobox)
     (zipcode             integer chk-zipcode)
     (notes               string)
     (search              string)
     (subset              string  chk-subset)
     (revenues-account    string  chk-revenues-account-title)
     (expenses-account    string  chk-expenses-account-title)
     (immediate-tx-only-p boolean))
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
                        'revenues-account-id (account-id (val revenues-account))
                        'expenses-account-id (account-id (val expenses-account))
                        'immediate-tx-only-p (val immediate-tx-only-p)
                        :where (:= 'id (val company-id))))
      (see-other (apply #'company/details :company-id (val company-id)
                        (params->filter))))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage company-page company/delete ("company/delete")
    ((company-id integer chk-company-id/ref t)
     (search     string)
     (subset     string  chk-subset))
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
               (header 'main)
               (navbar 'main 'company)
               (company-top-actions :delete)
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
    ((company-id integer chk-company-id/ref t)
     (search     string)
     (subset     string  chk-subset))
  (with-controller-page (company/delete)
    (with-transaction ()
      (execute (:delete-from 'contact
                :where (:= 'company-id (val company-id))))
      (delete-dao (get-dao 'company (val company-id))))
    (see-other (apply #'company
                      (params->filter)))))
