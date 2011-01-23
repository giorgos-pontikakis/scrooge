(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(define-existence-predicate company-id-exists-p company id)
(define-existence-predicate company-title-exists-p company title)
(define-existence-predicate tin-exists-p company tin)
(define-uniqueness-predicate company-title-unique-p company title id)
(define-uniqueness-predicate tin-unique-p company tin id)

(defun int-5digits-p (num)
  (and (integerp num)
       (> num 9999)
       (<= num 99999)))

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


(defun chk-tin (tin &optional id)
  (cond ((eql :null tin) nil)
        ((not (tin-unique-p tin id))
         :tin-exists)
        ((not (valid-tin-p tin))
         :tin-invalid)
        (t nil)))

(defun chk-pobox (pobox)
  (if (or (eql :null pobox)
          (int-5digits-p pobox))
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

(define-dynamic-page actions/company/create ("actions/company/create" :request-type :post)
    ((filter     string)
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
                                             :filter (val filter)
                                             :title (val title)
                                             :occupation (val occupation)
                                             :tof-id (or tof-id :null)
                                             :tin (val tin)
                                             :address (val address)
                                             :city-id (or city-id :null)
                                             :zipcode (val zipcode)
                                             :pobox (val pobox))))
            (insert-dao new-company)
            (see-other (company :id (id new-company) :filter (val filter)))))
        (see-other (company/create :filter (raw filter)
                                   :title (raw title)
                                   :occupation (raw occupation)
                                   :tof (raw tof)
                                   :tin (raw tin)
                                   :address (raw address)
                                   :city (raw city)
                                   :zipcode (raw zipcode)
                                   :pobox (raw pobox))))))

(define-dynamic-page actions/company/update ("actions/company/update" :request-type :post)
    ((filter     string)
     (id         integer)
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
            (see-other (company :id (val id) :filter (val filter)))))
        (see-other (company/update :filter (raw filter)
                                   :id (raw id)
                                   :title (raw title)
                                   :occupation (raw occupation)
                                   :tof (raw tof)
                                   :tin (raw tin)
                                   :address (raw address)
                                   :city (raw city)
                                   :zipcode (raw zipcode)
                                   :pobox (raw pobox))))))

(define-dynamic-page actions/company/delete ("actions/company/delete" :request-type :post)
    ((id     integer chk-company-id)
     (filter string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-db ()
          (delete-dao (get-dao 'company (val id)))
          (see-other (company :filter (val filter))))
        (see-other (notfound)))))



;;; ------------------------------------------------------------
;;; Company menu
;;; ------------------------------------------------------------

(defun company-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "company-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (company-actions-spec (company :id id
                                                               :filter filter)
                                                      (company/details :id id
                                                                       :filter filter)
                                                      (company/create :filter filter)
                                                      (company/update :id id
                                                                      :filter filter)
                                                      (company/details :id id
                                                                       :filter filter)
                                                      (if (chk-company-id/ref id)
                                                          nil
                                                          (company/delete :id id
                                                                          :filter filter))))
           :disabled-items disabled-items))



;;; ------------------------------------------------------------
;;; Database interface
;;; ------------------------------------------------------------

(defun get-company-plists (filter)
  (let* ((base-query `(:select company.id company.title tin
                               (:as tof.title tof)
                               address
                               (:as city.title city-name)
                               :from company
                               :left-join city
                               :on (:= city.id company.city-id)
                               :left-join tof
                               :on (:= tof.id company.tof-id)))
         (composite-query (if filter
                              (append base-query
                                      `(:where (:or (:ilike company.title ,(ilike filter))
                                                    (:ilike tin ,(ilike filter))
                                                    (:ilike address ,(ilike filter))
                                                    (:ilike city.title ,(ilike filter)))))
                              base-query))
         (final-query `(:order-by ,composite-query company.title)))
    (with-db ()
      (query (sql-compile final-query)
             :plists))))

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
  ((header-labels :initform '("" "Επωνυμία" "Α.Φ.Μ." "Δ.Ο.Υ." "Διεύθυνση" "Πόλη"))
   (paginator     :initform (make-instance 'paginator
                                           :id "company-paginator"
                                           :style "paginator grid_9 alpha"
                                           :delta 10
                                           :urlfn (lambda (filter start)
                                                    (company :filter filter
                                                             :start start))))))


(defmethod read-items ((table company-table))
  (iter (for rec in (get-company-plists (filter table)))
        (for i from 0)
        (collect (make-instance 'company-row
                                :key (getf rec :id)
                                :record rec
                                :collection table
                                :index i))))


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
                                            :on (company :filter filter
                                                         :start (page-start pg (index row) start))
                                            :off (company :filter filter
                                                          :id id)))
          :payload (mapcar (lambda (name)
                             (make-instance 'textbox-cell
                                            :name name
                                            :value (getf record (make-keyword name))))
                           '(title tin tof address city-name))
          :controls (list
                     (make-instance 'ok-cell)
                     (make-instance 'cancel-cell
                                    :href (company :id id :filter filter))))))



;;; ------------------------------------------------------------
;;; Notifications
;;; ------------------------------------------------------------

(defun company-notifications (&rest params)
  (notifications '((title   (:company-title-null "Το όνομα της εταιρίας είναι κενό"
                             :company-title-exists "Υπάρχει ήδη εταιρία με αυτή την επωνυμία"))
                   (tof     (:tof-title-unknown "Η Δ.Ο.Υ. αυτή δεν έχει οριστεί."))
                   (city    (:city-title-unknown "Η πόλη αυτή δεν έχει οριστεί."))
                   (tin     (:tin-exists "Υπάρχει ήδη εταιρία με αυτόν τον Α.Φ.Μ."
                             :tin-invalid "Άκυρος Α.Φ.Μ."))
                   (pobox   (:parse-error "Άκυροι χαρακτήρες στο αριθμό ταχυδρομικής θυρίδας"
                             :pobox-invalid "Μη αποδεκτός αριθμός ταχυδρομικής θυρίδας."))
                   (zipcode (:parse-error "Άκυροι χαρακτήρες στον ταχυδρομικό κωδικό"
                             :zipcode-invalid "Μη αποδεκτός ταχυδρομικός κωδικός.")))
                 params))



;;; ------------------------------------------------------------
;;; Company - Pages
;;; ------------------------------------------------------------

(define-dynamic-page company ("admin/company")
    ((id integer chk-company-id)
     (filter string)
     (start integer))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((company-table (make-instance 'company-table
                                            :op 'catalogue
                                            :filter (val* filter))))
          (with-document ()
            (:head
             (:title "Εταιρίες")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-menu 'company)
                   (:div :id "company-window" :class "window grid_9"
                         (:div :class "title" "Κατάλογος εταιριών")
                         (company-menu (val id)
                                       (val filter)
                                       (if (val id)
                                           '(catalogue create)
                                           '(catalogue details archive update delete)))
                         (display company-table
                                  :selected-id (val* id)
                                  :start (val* start)))
                   (:div :id "controls" :class "controls grid_3"
                         (filters 'company (val filter)))
                   (footer)))))
        (see-other (notfound)))))

(define-dynamic-page company/create ("admin/company/create")
    ((filter     string)
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
             (admin-menu 'company)
             (:div :id "company-window" :class "window grid_9"
                   (:div :class "title" "Δημιουργία εταιρίας")
                   (company-menu nil
                                 (val filter)
                                 '(details create update archive delete))
                   (with-form (actions/company/create)
                     (company-data-form 'create
                                        :filter (val filter)
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
             (:div :id "controls" :class "controls grid_3"
                   (company-notifications title tof tin city pobox zipcode))
             (footer))))))

(define-dynamic-page company/update ("admin/company/update")
    ((filter     string)
     (id         integer chk-company-id t)
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
                 (admin-menu 'company)
                 (:div :id "company-window" :class "window grid_9"
                       (:div :class "title" "Επεξεργασία εταιρίας")
                       (company-menu (val id)
                                     (val filter)
                                     '(create update))
                       (with-form (actions/company/update :id (val id))
                         (company-data-form 'update
                                            :id (val id)
                                            :filter (val filter)
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
                 (:div :id "controls" :class "controls grid_3"
                       (company-notifications title tof tin city pobox zipcode))
                 (footer))))
        (see-other (error-page)))))

(define-dynamic-page company/details ("admin/company/details")
    ((filter     string)
     (id         integer chk-company-id t))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (with-document ()
          (:head
           (:title "Λεπτομέρειες εταιρίας")
           (admin-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'admin)
                 (admin-menu 'company)
                 (:div :id "company-window" :class "window grid_9"
                       (:div :class "title" "Λεπτομέρειες εταιρίας")
                       (company-menu (val id)
                                     (val filter)
                                     '(details create))
                       (with-form (actions/company/update :id (val id))
                         (company-data-form 'details
                                            :filter (val filter)
                                            :id (val id)
                                            :data (get-company-plist (val id)))))
                 (:div :id "controls" :class "controls grid_3"
                       "")
                 (error-page))))
        (see-other (notfound)))))

(define-dynamic-page company/delete ("admin/company/delete")
    ((id     integer chk-company-id t)
     (filter string))
  (with-auth ("configuration")
    (no-cache)
    (if (validp id)
        (let ((company-table (make-instance 'company-table
                                            :op 'delete
                                            :filter (val* filter))))
          (with-document ()
            (:head
             (:title "Διαγραφή εταιρίας")
             (admin-headers))
            (:body
             (:div :id "container" :class "container_12"
                   (header 'admin)
                   (admin-menu 'company)
                   (:div :id "company-window" :class "window grid_9"
                         (:div :class "title" "Διαγραφή εταιρίας")
                         (company-menu (val id)
                                       (val filter)
                                       '(catalogue create delete))
                         (with-form (actions/company/delete :id (val id)
                                                            :filter (val* filter))
                           (display company-table
                                    :selected-id (val id))))
                   (:div :id "controls" :class "controls grid_3"
                         (filters 'company (val filter)))
                   (footer)))))
        (see-other (error-page)))))

(defun company-data-form (op &key filter id data styles)
  (let ((disabledp (eql op 'details)))
    (with-html
      (:div :id "company-data-form" :class "data-form"
            (:div :id "company-title" :class "grid_9 alpha"
                  (label 'title "Επωνυμία")
                  (textbox 'title
                           :value (getf data :title)
                           :disabledp disabledp
                           :style (getf styles :title)))
            (:div :id "company-tax-data" :class "grid_4 alpha"
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
                                      :style (getf styles :tof))))))
            (:div :id "company-address-data" :class "grid_5 omega"
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
            (unless disabledp
              (htm (:div :id "company-data-form-buttons" :class "grid_9"
                         (ok-button (if (eql op 'update) "Ανανέωση" "Δημιουργία"))
                         (cancel-button (company :id id :filter filter) "Άκυρο"))))))))
