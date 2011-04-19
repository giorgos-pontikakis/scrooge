(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(define-existence-predicate company-id-exists-p company id)
(define-existence-predicate company-title-exists-p company title)
(define-existence-predicate tin-exists-p company tin)
(define-existence-predicate contact-id-exists-p contact id)

(define-uniqueness-predicate company-title-unique-p company title id)
(define-uniqueness-predicate tin-unique-p company tin id)

(defun company-referenced-p (id)
  (with-db ()
    (or (query (:select 1 :from 'project
                        :where (:= 'company-id id)))
        (query (:select 1 :from 'cheque
                        :where (:= 'company-id id)))
        (query (:select 1 :from 'tx
                        :where (:= 'company-id id))))))

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

(defun chk-new-company-title (title &optional id)
  (cond ((eql :null title) :company-title-null)
        ((not (company-title-unique-p title id)) :company-title-exists)
        (t nil)))

(defun chk-company-title (title)
  (cond ((eql :null title) :company-title-null)
        ((not (company-title-exists-p title)) :company-title-unknown)
        (t nil)))

(defun chk-company-title* (title)
  "Like chk-company-title, but accepts null titles"
  (cond ((eql :null title) nil)
        ((not (company-title-exists-p title)) :company-title-unknown)
        (t nil)))

(defun chk-contact-id (company-id contact-id)
  (if (and (company-id-exists-p company-id)
           (contact-id-exists-p contact-id))
      nil
      :contact-id-invalid))

(defun chk-tin (tin &optional id)
  (cond ((eql :null tin) nil)
        ((not (tin-unique-p tin id))
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



;;; ------------------------------------------------------------
;;; Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/admin/company/create ("actions/admin/company/create"
                                                   :request-type :post)
    ((search     string)
     (title      string  chk-new-company-title)
     (occupation string)
     (tof        string  chk-tof-title)
     (tin        string  chk-tin)
     (address    string)
     (city       string  chk-city-title)
     (pobox      integer chk-pobox)
     (zipcode    integer chk-zipcode)
     (notes      string))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (let* ((tof-id (tof-id (val tof)))
                 (city-id (city-id (val city)))
                 (new-company (make-instance 'company
                                             :search (val search)
                                             :title (val title)
                                             :occupation (val occupation)
                                             :tof-id (or tof-id :null)
                                             :tin (val tin)
                                             :address (val address)
                                             :city-id (or city-id :null)
                                             :zipcode (val zipcode)
                                             :pobox (val pobox)
                                             :notes (val notes))))
            (insert-dao new-company)
            (see-other (company :id (id new-company)))))
        (see-other (company/create :search (raw search)
                                   :title (raw title)
                                   :occupation (raw occupation)
                                   :tof (raw tof)
                                   :tin (raw tin)
                                   :address (raw address)
                                   :city (raw city)
                                   :zipcode (raw zipcode)
                                   :pobox (raw pobox)
                                   :notes (raw notes))))))

(define-dynamic-page actions/admin/company/update ("actions/admin/company/update"
                                                   :request-type :post)
    ((search     string)
     (id         integer chk-company-id)
     (title      string  (chk-new-company-title title id))
     (occupation string)
     (tof        string  chk-tof-title)
     (tin        string  (chk-tin tin id))
     (address    string)
     (city       string  chk-city-title)
     (pobox      integer chk-pobox)
     (zipcode    integer chk-zipcode)
     (notes      string))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
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
            (see-other (company :id (val id)))))
        (see-other (company/update :search (raw search)
                                   :id (raw id)
                                   :title (raw title)
                                   :occupation (raw occupation)
                                   :tof (raw tof)
                                   :tin (raw tin)
                                   :address (raw address)
                                   :city (raw city)
                                   :pobox (raw pobox)
                                   :zipcode (raw zipcode)
                                   :notes (raw notes))))))

(define-dynamic-page actions/admin/company/delete ("actions/admin/company/delete"
                                                   :request-type :post)
    ((id     integer chk-company-id)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (with-transaction ()
            (execute (:delete-from 'contact
                                   :where (:= 'company-id (val id))))
            (delete-dao (get-dao 'company (val id))))
          (see-other (company :search (val search))))
        (see-other (notfound)))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun company-menu (id filter &optional disabled-items)
  (display
   (make-instance 'actions-menu
                  :id "company-actions"
                  :style "hnavbar actions"
                  :spec (crud+details-actions-spec (apply #'company :id id filter)
                                                   (apply #'company/create filter)
                                                   (apply #'company/details :id id filter)
                                                   (apply #'company/update :id id filter)
                                                   (if (chk-company-id/ref id)
                                                       nil
                                                       (apply #'company/delete :id id filter))))
   :disabled-items disabled-items))

(defun company-notifications ()
  (notifications '((title   (:company-title-null "Το όνομα της εταιρίας είναι κενό"
                             :company-title-exists "Υπάρχει ήδη εταιρία με αυτή την επωνυμία"))
                   (tof     (:tof-title-unknown "Η Δ.Ο.Υ. αυτή δεν έχει οριστεί."))
                   (city    (:city-title-unknown "Η πόλη αυτή δεν έχει οριστεί."))
                   (tin     (:tin-exists "Υπάρχει ήδη εταιρία με αυτόν τον Α.Φ.Μ."
                             :tin-invalid "Άκυρος Α.Φ.Μ."))
                   (pobox   (:parse-error "Άκυροι χαρακτήρες στο αριθμό ταχυδρομικής θυρίδας"
                             :pobox-invalid "Μη αποδεκτός αριθμός ταχυδρομικής θυρίδας."))
                   (zipcode (:parse-error "Άκυροι χαρακτήρες στον ταχυδρομικό κωδικό"
                             :zipcode-invalid "Μη αποδεκτός ταχυδρομικός κωδικός.")))))



;;; ------------------------------------------------------------
;;; Database interface
;;; ------------------------------------------------------------

(defun get-company-plist (id)
  (with-db ()
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
           :plist)))



;;; ------------------------------------------------------------
;;; Company table
;;; ------------------------------------------------------------

;;; table

(defclass company-table (crud-table)
  ((item-key-field :initform :id)
   (header-labels  :initform '("" "Επωνυμία" "Α.Φ.Μ." "Δ.Ο.Υ."))
   (paginator      :initform (make-instance 'default-paginator
                                           :id "company-paginator"
                                           :style "paginator"
                                           :urlfn #'company)))
  (:default-initargs :item-class 'company-row :id "company-table"))

(defmethod read-records ((table company-table))
  (let* ((search (getf (filter table) :search))
         (ilike-term (ilike search))
         (base-query `(:select company.id company.title tin
                               (:as tof.title tof)
                               address
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
                                                    (:ilike company.notes ,ilike-term)
                                                    (:ilike contact.tag ,ilike-term))))
                              base-query))
         (final-query `(:order-by ,composite-query company.title)))
    (with-db ()
      (query (sql-compile final-query)
             :plists))))

;;; rows

(defclass company-row (crud-row)
  ())

(defmethod cells ((row company-row) &key start)
  (let* ((id (key row))
         (record (record row))
         (pg (paginator (collection row)))
         (filter (filter (collection row))))
    (list :selector (make-instance 'selector-cell
                                   :states (list
                                            :on (apply #'company
                                                       :start (page-start pg (index row) start)
                                                       filter)
                                            :off (apply #'company :id id filter)))
          :payload (mapcar (lambda (name)
                             (make-instance 'textbox
                                            :name name
                                            :value (getf record (make-keyword name))))
                           '(title tin tof))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (apply #'company :id id filter))))))



;;; ------------------------------------------------------------
;;; Company - Pages
;;; ------------------------------------------------------------

(define-dynamic-page company ("admin/company")
    ((id integer chk-company-id)
     (search string)
     (start integer))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (parameters->plist search))
               (company-table (make-instance 'company-table
                                             :op :catalogue
                                             :filter filter)))
          (with-document ()
            (:head
             (:title "Εταιρίες » Κατάλογος")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-navbar 'company)
                   (:div :id "company-window" :class "window grid_10"
                         (:div :class "title" "Εταιρίες » Κατάλογος")
                         (company-menu (val id)
                                       filter
                                       (if (val id)
                                           '(:catalogue)
                                           '(:catalogue :details :update :delete)))
                         (display company-table
                                  :selected-id (val* id)
                                  :start (val* start)))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (company) (val search)))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page company/create ("admin/company/create")
    ((search     string)
     (title      string  chk-new-company-title)
     (occupation string)
     (tof        string  chk-tof-title)
     (tin        string  chk-tin)
     (address    string)
     (city       string  chk-city-title)
     (pobox      integer chk-pobox)
     (zipcode    integer chk-zipcode)
     (notes      string))
  (with-auth ("configuration")
    (no-cache)
    (let ((filter (parameters->plist search)))
      (with-document ()
        (:head
         (:title "Εταιρία » Δημιουργία")
         (admin-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'admin)
               (admin-navbar 'company)
               (:div :id "company-window" :class "window grid_6"
                     (:div :class "title" "Εταιρία » Δημιουργία")
                     (company-menu nil
                                   filter
                                   '(:details :create :update :delete))
                     (company-notifications)
                     (with-form (actions/admin/company/create :search (val search))
                       (company-data-form 'create
                                          :filter filter
                                          :data (parameters->plist title
                                                                   occupation
                                                                   tof
                                                                   tin
                                                                   address
                                                                   city
                                                                   pobox
                                                                   zipcode
                                                                   notes)
                                          :styles (parameters->styles title
                                                                      occupation
                                                                      tof
                                                                      tin
                                                                      address
                                                                      city
                                                                      pobox
                                                                      zipcode
                                                                      notes))))
               (:div :id "contact-window" :class "window grid_6"
                     (:div :class "title" "Επαφές")
                     (:div :class "hnavbar actions"
                           (:p "Προς το παρόν δεν μπορείτε να δημιουργήσετε επαφές.<br />Ολοκληρώστε πρώτα τη δημιουργία της εταιρίας.")))
               (footer)))))))

(define-dynamic-page company/update ("admin/company/update")
    ((search     string)
     (id         integer chk-company-id t)
     (contact-id integer (chk-contact-id id contact-id))
     (title      string  (chk-new-company-title title id))
     (occupation string)
     (tof        string  chk-tof-title)
     (tin        string  (chk-tin tin id))
     (address    string)
     (city       string  chk-city-title)
     (pobox      integer chk-pobox)
     (zipcode    integer chk-zipcode)
     (notes      string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (parameters->plist search))
               (contact-table (make-instance 'contact-table
                                             :op :details
                                             :company-id (val id))))
          (with-document ()
            (:head
             (:title "Εταιρία » Επεξεργασία")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-navbar 'company)
                   (:div :id "company-window" :class "window grid_6"
                         (:div :class "title" "Εταιρία » Επεξεργασία")
                         (company-menu (val id)
                                       filter
                                       '(:create :update))
                         (company-notifications)
                         (with-form (actions/admin/company/update :id (val id) :search (val search))
                           (company-data-form 'update
                                              :id (val id)
                                              :filter filter
                                              :data (plist-union (parameters->plist title
                                                                                    occupation
                                                                                    tof
                                                                                    tin
                                                                                    address
                                                                                    city
                                                                                    pobox
                                                                                    zipcode
                                                                                    notes)
                                                                 (get-company-plist (val id)))
                                              :styles (parameters->styles title
                                                                          occupation
                                                                          tof
                                                                          tin
                                                                          address
                                                                          city
                                                                          pobox
                                                                          zipcode
                                                                          notes))))
                   (:div :id "contact-window" :class "window grid_6"
                         (:div :class "title" "Επαφές")
                         (contact-menu (val id)
                                       (val contact-id)
                                       filter
                                       (if (val contact-id)
                                           '(:catalogue)
                                           '(:catalogue :update :delete)))
                         (display contact-table
                                  :selected-id (val contact-id)))
                   (footer)))))
        (see-other (error-page)))))

(define-dynamic-page company/details ("admin/company/details")
    ((search     string)
     (id         integer chk-company-id t)
     (contact-id integer (chk-contact-id id contact-id)))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((filter (parameters->plist search))
              (contact-table (make-instance 'contact-table
                                            :op :details
                                            :company-id (val id))))
          (with-document ()
            (:head
             (:title "Εταιρία » Λεπτομέρειες")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-navbar 'company)
                   (:div :id "company-window" :class "window grid_6"
                         (:div :class "title" "Εταιρία » Λεπτομέρειες")
                         (company-menu (val id)
                                       filter
                                       '(:details :create))
                         (company-data-form 'details
                                            :filter filter
                                            :id (val id)
                                            :data (get-company-plist (val id))))
                   (:div :id "contact-window" :class "window grid_6"
                         (:div :class "title" "Επαφές » Κατάλογος")
                         (contact-menu (val id)
                                       (val contact-id)
                                       filter
                                       (if (val contact-id)
                                           '(:catalogue)
                                           '(:catalogue :update :delete)))
                         (display contact-table
                                  :selected-id (val contact-id)))))))
        (see-other (error-page)))))

(define-dynamic-page company/delete ("admin/company/delete")
    ((id     integer chk-company-id/ref t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let* ((filter (parameters->plist search))
               (company-table (make-instance 'company-table
                                             :op :delete
                                             :filter filter)))
          (with-document ()
            (:head
             (:title "Διαγραφή εταιρίας")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-navbar 'company)
                   (:div :id "company-window" :class "window grid_10"
                         (:div :class "title" "Διαγραφή εταιρίας")
                         (company-menu (val id)
                                       filter
                                       '(:catalogue :delete))
                         (with-form (actions/admin/company/delete :id (val id)
                                                                  :search (val* search))
                           (display company-table
                                    :selected-id (val id))))
                   (:div :id "sidebar" :class "sidebar grid_2"
                         (searchbox (company) (val search)))
                   (footer)))))
        (see-other (error-page)))))

(defun company-data-form (op &key id data styles filter)
  (let ((disabled (eql op :details)))
    (flet ((label+textbox (name label &optional extra-styles)
             (with-html
               (label name label)
               (textbox name
                        :value (getf data (make-keyword name))
                        :disabled disabled
                        :style (conc (getf styles (make-keyword name))
                                     " " extra-styles)))))
      (with-html
        (:div :class "data-form company-data"
              (:div :class "company-form-no-fieldset"
                    (label+textbox 'title "Επωνυμία" "ac-company"))
              (:fieldset
               (:legend "Φορολογικά στοιχεία")
               (:div :class "company-data-full" (label+textbox 'occupation "Επάγγελμα"))
               (:div :id "tin" :class "company-data-half" (label+textbox 'tin "Α.Φ.Μ."))
               (:div :id "tof-div" :class "company-data-half" (label+textbox 'tof "Δ.Ο.Υ." "ac-tof")))
              (:fieldset
               (:legend "Διεύθυνση")
               (:div :id "address" (label+textbox 'address "Οδός"))
               (:div :id "city" (label+textbox 'city "Πόλη" "ac-city"))
               (:div :id "zipcode" (label+textbox 'zipcode "Ταχυδρομικός κωδικός"))
               (:div :id "pobox" (label+textbox 'pobox "Ταχυδρομική θυρίδα")))
              (:div :class "company-form-no-fieldset"
                    (label 'notes "Σημειώσεις")
                    (:textarea :name 'notes
                               :cols 56 :rows 10 :disabled disabled
                               (str (lisp->html (or (getf data :notes) :null))))))
        (:div :class "data-form-buttons"
              (if disabled
                  (cancel-button (apply #'company :id id filter)
                                 "Επιστροφή στον Κατάλογο Εταιριών")
                  (progn
                    (ok-button (if (eql op :update) "Ανανέωση" "Δημιουργία"))
                    (cancel-button (apply #'company :id id filter) "Άκυρο"))))))))
