(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass company-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id start))
   (payload-parameter-names
    :allocation :class
    :initform '(title occupation tof tin address city pobox zipcode notes))
   (filter-parameter-names
    :allocation :class
    :initform '(search))
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

(defun company-top-actions (op id filter)
  (top-actions (html ()
                 (menu `((catalogue ,(html ()
                                       (:a :href (apply #'company :id id filter)
                                           (:img :src "/scrooge/img/application_view_list.png")
                                           "Κατάλογος")))
                         (create ,(html ()
                                    (:a :href (company/create)
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
                 (searchbox #'actions/company/search
                            #'(lambda (&rest args)
                                (apply #'company :id id args))
                            filter
                            "ac-company"))))

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
  (filter-area (filter-navbar `((nil ,(apply #'company filter) "Όλες")))))

(defun company-tabs (id filter active content)
  (with-html
    (:div :class "grid_12"
          (:div :class "company"
                (:div :class "company-titlebar"
                      (if id
                          (htm
                           (:h3 :class "grid_8 alpha"
                                (str (title (get-dao 'company id)))))
                          (htm
                           (:h3 :class "grid_8 alpha" "")))
                      (navbar
                       `((data ,(apply #'company/details :id id filter)
                               "Στοιχεία")
                         (transactions ,(apply #'company/details/transactions :id id filter)
                                       "Συναλλαγές"))
                       :css-class "hnavbar grid_3 omega"
                       :active active)
                      (clear))
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
                       :on (:= contact.company-id company.id)))
         (composite-query (if search
                              (append base-query
                                      `(:where (:or (:ilike company.title ,ilike-term)
                                                    (:ilike tin ,ilike-term)
                                                    (:ilike address ,ilike-term)
                                                    (:ilike city.title ,ilike-term)
                                                    (:ilike occupation ,ilike-term)
                                                    (:ilike company.notes ,ilike-term)
                                                    (:ilike contact.tag ,ilike-term)
                                                    (:ilike contact.phone ,ilike-term))))
                              base-query))
         (final-query `(:order-by ,composite-query company.title)))
    (query (sql-compile final-query)
           :plists)))

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
;;; VIEW
;;; ------------------------------------------------------------

(defpage company-page company ("company")
    ((id     integer chk-company-id)
     (search string)
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
                                         :company-id (val id)))
           (op :details))
      (with-document ()
        (:head
         (:title "Εταιρία » Λεπτομέρειες")
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

(defpage company-page company/details/transactions ("company/details/transactions")
    ((search     string)
     (id         integer chk-company-id t))
  (with-view-page
    (let* ((filter (params->filter)))
      (with-document ()
        (:head
         (:title "Εταιρία » Λεπτομέρειες » Συναλλαγές")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (filter-navbar  filter (val id))
               (:p "money")
               (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage company-page company/create ("company/create")
    ((search     string)
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
                               (:div :class "grid_6"
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
    (let* ((op :update) (filter (params->filter))
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
     (search string))
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
     (search string))
  (with-controller-page (company/delete)
    (with-transaction ()
      (execute (:delete-from 'contact
                :where (:= 'company-id (val id))))
      (delete-dao (get-dao 'company (val id))))
    (see-other (apply #'company
                      (params->filter)))))

;; (defun company-debits (company-id)
;;   (query (:select 'tx.id 'tx.description 'tx.amount
;;           :from 'tx
;;           :inner-join (:as 'account 'debit-account)
;;           :on (:= 'debit-account.id 'tx.debit-acc-id)
;;           :inner-join (:as 'account 'credit-account)
;;           :on (:= 'credit-account.id 'tx.credit-acc-id)
;;           :where (:and (:= 'tx.company-id company-id)

;;                        (:= 'credit-account.id *cash-acc-id*
;;                            (:= 'credit-account.id *cheque-payable-acc-id*))
;;                        (:in 'debit-account.id (:set (list *invoice-receivable-acc-id*)))))))

;; (defun company-credits (company-id)
;;   (query (:select 'tx.id 'tx.description 'tx.amount
;;           :from 'tx
;;           :inner-join (:as 'account 'debit-account)
;;           :on (:= 'debit-account.id 'tx.debit-acc-id)
;;           :inner-join (:as 'account 'credit-account)
;;           :on (:= 'credit-account.id 'tx.credit-acc-id)
;;           :where (:and (:= 'tx.company-id company-id)
;;                        (:= 'debit-account.id *cash-acc-id*)
;;                        (:= 'debit-account.id *cheque-receivable-acc-id*)
;;                        (:= 'credit-account.id *invoice-payable-acc-id*)))))

;; ;; (defun company-credits (company-id)
;; ;;   (sql-compile `(:select tx.id tx.description tx.amount
;; ;;                  :from tx
;; ;;                  :inner-join (:as account debit-account)
;; ;;                  :on (:= debit-account.id tx.debit-acc-id)
;; ;;                  :inner-join (:as account credit-account)
;; ;;                  :on (:= credit-account.id tx.credit-acc-id)
;; ;;                  :where (:and (:= tx.company-id ,company-id)
;; ;;                               (:in debit-account.id (:set (,*cash-acc-id* ,*cheque-receivable-acc-id*)))
;; ;;                               (:in credit-account.id (:set (list ,*invoice-payable-acc-id*)))))))

;; (defun dfs (expanded fringe)
;;   (let ((head (first fringe))
;;         (tail (rest fringe)))
;;     (if head
;;         (dfs (append expanded (list head))
;;              (append (children head) tail))
;;         expanded)))


;; (defun tree-search (tree)
;;   (if (children head)))

;; (with-db ()
;;   (let ((root (root (make-instance 'account-tree :root-key *expenses-root-acc-id* :filter '(:debit-p t)))))
;;     (mapcar (compose (lambda (rec) (getf rec :title)) #'record)
;;             (dfs '() (list root)))))


;; (defun descendant-p (root-key key)
;;   (let ((node (get-dao 'account key)))
;;     (cond ((equal (root-parent-key (tree node)) (parent-key node))
;;            nil)
;;           ((equal (parent-key node) (key root-node))
;;            t)
;;           (descendant-p root-node (get-dao )))))


;; (mapcar (lambda (node)
;;           (key node))
;;         (children))


;; (defun depth-first-flatten-tree (fringe)
;;   (let ((expanded (first fringe)))
;;     (if (null expanded)
;;         fringe
;;         (depth-first-flatten-tree (append (children expanded)
;;                                           fringe)))))
