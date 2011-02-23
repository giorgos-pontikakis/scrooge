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
  (declare (ignore id))
  nil)

(defun chk-company-id (id)
  (if (company-id-exists-p id)
      nil
      :company-id-unknown))

(defun chk-company-id/ref (id)
  (if (and (not (null id))
           (null (chk-company-id id))
           (null (company-referenced-p id)))
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
          (positive-int-p pobox))
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
     (zipcode    integer chk-zipcode))
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
                                             :pobox (val pobox))))
            (insert-dao new-company)
            (see-other (company :id (id new-company) :search (val search)))))
        (see-other (company/create :search (raw search)
                                   :title (raw title)
                                   :occupation (raw occupation)
                                   :tof (raw tof)
                                   :tin (raw tin)
                                   :address (raw address)
                                   :city (raw city)
                                   :zipcode (raw zipcode)
                                   :pobox (raw pobox))))))

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
     (zipcode    integer chk-zipcode))
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
                              :where (:= 'id (val id))))
            (see-other (company :id (val id) :search (val search)))))
        (see-other (company/update :search (raw search)
                                   :id (raw id)
                                   :title (raw title)
                                   :occupation (raw occupation)
                                   :tof (raw tof)
                                   :tin (raw tin)
                                   :address (raw address)
                                   :city (raw city)
                                   :zipcode (raw zipcode)
                                   :pobox (raw pobox))))))

(define-dynamic-page actions/admin/company/delete ("actions/admin/company/delete"
                                                   :request-type :post)
    ((id     integer chk-company-id)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'company (val id)))
          (see-other (company :search (val search))))
        (see-other (notfound)))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun company-menu (id search &optional disabled-items)
  (display
   (make-instance 'actions-menu
                  :id "company-actions"
                  :style "hnavbar actions grid_8 alpha"
                  :spec (crud+details+archive-actions-spec (company :id id
                                                                    :search search)
                                                           (company/create :search search)
                                                           (company/details :id id
                                                                            :search search)
                                                           (company/update :id id
                                                                           :search search)
                                                           (company/details :id id
                                                                            :search search)
                                                           (if (chk-company-id/ref id)
                                                               nil
                                                               (company/delete :id id
                                                                               :search search))))
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
                    'zipcode 'pobox
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
  ((header-labels :initform '("" "Επωνυμία" "Α.Φ.Μ." "Δ.Ο.Υ." "Διεύθυνση"))
   (paginator     :initform (make-instance 'paginator
                                           :id "company-paginator"
                                           :style "paginator grid_8 alpha"
                                           :delta 10
                                           :urlfn (lambda (search start)
                                                    (company :search search
                                                             :start start)))))
  (:default-initargs :item-class 'company-row))


(defmethod read-records ((table company-table))
  (let* ((search (filter table))
         (base-query `(:select company.id company.title tin
                               (:as tof.title tof)
                               address
                               (:as city.title city-name)
                               :from company
                               :left-join city
                               :on (:= city.id company.city-id)
                               :left-join tof
                               :on (:= tof.id company.tof-id)))
         (composite-query (if search
                              (append base-query
                                      `(:where (:or (:ilike company.title ,(ilike search))
                                                    (:ilike tin ,(ilike search))
                                                    (:ilike address ,(ilike search))
                                                    (:ilike city.title ,(ilike search)))))
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
         (search (filter (collection row))))
    (list :selector (make-instance 'selector-cell
                                   :states (list
                                            :on (company :search search
                                                         :start (page-start pg (index row) start))
                                            :off (company :search search
                                                          :id id)))
          :payload (mapcar (lambda (name)
                             (make-instance 'textbox-cell
                                            :name name
                                            :value (getf record (make-keyword name))))
                           '(title tin tof address))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (company :id id :search search))))))



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
        (let ((company-table (make-instance 'company-table
                                            :op 'catalogue
                                            :filter (val* search))))
          (with-document ()
            (:head
             (:title "Εταιρίες")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-navbar 'company)
                   (:div :id "company-window" :class "window grid_8"
                         (:div :class "title" "Κατάλογος εταιριών")
                         (company-menu (val id)
                                       (val search)
                                       (if (val id)
                                           '(catalogue)
                                           '(catalogue details archive update delete)))
                         (display company-table
                                  :selected-id (val* id)
                                  :start (val* start)))
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
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
     (zipcode    integer chk-zipcode))
  (with-auth ("configuration")
    (no-cache)
    (with-document ()
      (:head
       (:title "Δημιουργία εταιρίας")
       (admin-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'admin)
             (admin-navbar 'company)
             (:div :id "company-window" :class "window grid_8"
                   (:div :class "title" "Δημιουργία εταιρίας")
                   (company-menu nil
                                 (val search)
                                 '(details create update archive delete))
                   (:div :id "notifications grid_12"
                         (company-notifications))
                   (with-form (actions/admin/company/create)
                     (company-data-form 'create
                                        :search (val search)
                                        :data (parameters->plist title
                                                                 occupation
                                                                 tof
                                                                 tin
                                                                 address
                                                                 city
                                                                 pobox
                                                                 zipcode)
                                        :styles (parameters->styles title
                                                                    occupation
                                                                    tof
                                                                    tin
                                                                    address
                                                                    city
                                                                    pobox
                                                                    zipcode))))
             (footer))))))

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
     (zipcode    integer chk-zipcode))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-document ()
          (:head
           (:title "Επεξεργασία εταιρίας")
           (admin-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'admin)
                 (admin-navbar 'company)
                 (:div :id "company-window" :class "window grid_8"
                       (:div :class "title" "Επεξεργασία εταιρίας")
                       (company-menu (val id)
                                     (val search)
                                     '(create update))
                       (:div :id "notifications grid_12"
                             (company-notifications))
                       (with-form (actions/admin/company/update :id (val id))
                         (company-data-form 'update
                                            :id (val id)
                                            :search (val search)
                                            :data (plist-union (parameters->plist title
                                                                                  occupation
                                                                                  tof
                                                                                  tin
                                                                                  address
                                                                                  city
                                                                                  pobox
                                                                                  zipcode)
                                                               (get-company-plist (val id)))
                                            :styles (parameters->styles title
                                                                        occupation
                                                                        tof
                                                                        tin
                                                                        address
                                                                        city
                                                                        pobox
                                                                        zipcode))))
                 (:div :id "contact-window" :class "window grid_6"
                       (:div :class "title" "Επαφές")
                       (contact-menu (val id)
                                     (val contact-id)
                                     (val search)
                                     (if (val contact-id)
                                         '(details)
                                         '(details update delete))))
                 (footer))))
        (see-other (error-page)))))

(define-dynamic-page company/details ("admin/company/details")
    ((search     string)
     (id         integer chk-company-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((contact-table (make-instance 'contact-table
                                            :op 'update
                                            :company-id (val id))))
          (with-document ()
            (:head
             (:title "Λεπτομέρειες εταιρίας")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-navbar 'company)
                   (:div :id "company-window" :class "window grid_6"
                         (:div :class "title" "Λεπτομέρειες εταιρίας")
                         (company-menu (val id)
                                       (val search)
                                       '(details create))
                         (company-data-form 'details
                                            :search (val search)
                                            :id (val id)
                                            :data (get-company-plist (val id))))
                   (:div :id "contact-window" :class "window grid_6"
                         (:div :class "title" "Επαφές > Δημιουργία")
                         (contact-menu (val id)
                                       nil
                                       (val search)
                                       '(details create update delete))
                         (:div :class "grid_6 alpha company-data-form-contact-data"
                               (display contact-table)))
                   (error-page)))))
        (see-other (notfound)))))

(define-dynamic-page company/delete ("admin/company/delete")
    ((id     integer chk-company-id t)
     (search string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((company-table (make-instance 'company-table
                                            :op 'delete
                                            :filter (val* search))))
          (with-document ()
            (:head
             (:title "Διαγραφή εταιρίας")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-navbar 'company)
                   (:div :id "company-window" :class "window grid_8"
                         (:div :class "title" "Διαγραφή εταιρίας")
                         (company-menu (val id)
                                       (val search)
                                       '(catalogue delete))
                         (with-form (actions/admin/company/delete :id (val id)
                                                                  :search (val* search))
                           (display company-table
                                    :selected-id (val id))))
                   (:div :id "sidebar" :class "sidebar grid_3"
                         (:p :class "title" "Φίλτρα")
                         (searchbox (company) (val search)))
                   (footer)))))
        (see-other (error-page)))))

(defun company-data-form (op &key id data styles search)
  (let ((disabledp (eql op 'details)))
    (with-html
      (:div :id "company-data-form" :class "data-form"
            (:div :class "grid_8 alpha company-data-form-title"
                  (label 'title "Επωνυμία")
                  (textbox 'title
                           :value (getf data :title)
                           :disabledp disabledp
                           :style (getf styles :title)))
            (:div :class "grid_4 alpha company-data-form-core-data"
                  (:fieldset
                   (:legend "Φορολογικά στοιχεία")
                   (:ul (:li (label 'occupation "Επάγγελμα")
                             (textbox 'occupation
                                      :id "occupation"
                                      :value (getf data :occupation)
                                      :disabledp disabledp
                                      :style (getf styles :occupation)))
                        (:li (label 'tin "Α.Φ.Μ.")
                             (textbox 'tin
                                      :id "tin"
                                      :value (getf data :tin)
                                      :disabledp disabledp
                                      :style (getf styles :tin)))
                        (:li (label 'tof "Δ.Ο.Υ.")
                             (textbox 'tof
                                      :id "tof"
                                      :value (getf data :tof)
                                      :disabledp disabledp
                                      :style (getf styles :tof)))))
                  (:fieldset
                   (:legend "Διεύθυνση")
                   (:ul (:li (label 'address "Οδός")
                             (textbox 'address
                                      :value (getf data :address)
                                      :disabledp disabledp
                                      :style (getf styles :address)))
                        (:li (label 'city "Πόλη")
                             (textbox 'city
                                      :id "city"
                                      :value (getf data :city)
                                      :disabledp disabledp
                                      :style (getf styles :city)))
                        (:li (label 'zipcode "Ταχυδρομικός κώδικας")
                             (textbox 'zipcode
                                      :value (getf data :zipcode)
                                      :disabledp disabledp
                                      :style (getf styles :zipcode))
                             (label 'pobox "Ταχυδρομική θυρίδα")
                             (textbox 'pobox
                                      :value (getf data :pobox)
                                      :disabledp disabledp
                                      :style (getf styles :pobox))))))
            (:div :class "data-form-buttons grid_8"
                  (if disabledp
                      (cancel-button (company :id id :search search)
                                     "Επιστροφή στον Κατάλογο Εταιριών")
                      (progn
                        (ok-button (if (eql op 'update) "Ανανέωση" "Δημιουργία"))
                        (cancel-button (company :id id :search search) "Άκυρο"))))))))
