(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

(define-existence-predicate company-id-exists-p company id)
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
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (let ((tof-id (tof-id (val tof)))
              (city-id (city-id (val city))))
          (let ((new-company (make-instance 'company
                                            :title (val title)
                                            :occupation (val occupation)
                                            :tof-id (or tof-id :null)
                                            :tin (val tin)
                                            :address (val address)
                                            :city-id (or city-id :null)
                                            :zipcode (val zipcode)
                                            :pobox (val pobox))))
            (insert-dao new-company)
            (see-other (company :id (id new-company))))))
      (see-other (company/create :filter (raw filter)
                                 :title (raw title)
                                 :occupation (raw occupation)
                                 :tof (raw tof)
                                 :tin (raw tin)
                                 :address (raw address)
                                 :city (raw city)
                                 :zipcode (raw zipcode)
                                 :pobox (raw pobox)))))

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
      (see-other (company/update :id (raw id)
                                 :title (raw title)
                                 :occupation (raw occupation)
                                 :tof (raw tof)
                                 :tin (raw tin)
                                 :address (raw address)
                                 :city (raw city)
                                 :zipcode (raw zipcode)
                                 :pobox (raw pobox)))))

(define-dynamic-page actions/company/delete ("actions/company/delete" :request-type :post)
    ((id integer chk-company-id))
  (no-cache)
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'company (val id)))
        (see-other (company)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; Company menu
;;; ------------------------------------------------------------

(defun company-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "company-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (standard-actions-spec (company :id id
                                                                :filter filter)
                                                       (company/create :filter filter)
                                                       (company/update :id id
                                                                       :filter filter)
                                                       (if (chk-company-id/ref id)
                                                           nil
                                                           (company/delete :id id
                                                                           :filter filter))))
           :disabled-items disabled-items))



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
  (flet ()
    (iter (for rec in (company-data (filter table)))
          (for i from 0)
          (collect (make-instance 'company-row
                                  :key (getf rec :id)
                                  :record rec
                                  :collection table
                                  :index i)))))

(defun company-data (filter)
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
;;; Other areas
;;; ------------------------------------------------------------

(defun company-filters (filter)
  (with-html
    (:div :id "filters"
          (:p :class "title" "Φίλτρα")
          (with-form (company)
            (htm
             (:p :class "search"
                 (textbox 'filter :value filter)
                 (submit (html ()
                           (img "magnifier.png")))))))))

(defun company-notifications (&rest params)
  (notifications '(title   ((:company-title-null "Το όνομα της εταιρίας είναι κενό")
                            (:company-title-exists "Υπάρχει ήδη εταιρία με αυτή την επωνυμία"))
                   tof     ((:tof-title-unknown "Η Δ.Ο.Υ. αυτή δεν έχει οριστεί."))
                   city    ((:city-title-unknown "Η πόλη αυτή δεν έχει οριστεί."))
                   tin     ((:tin-exists "Υπάρχει ήδη εταιρία με αυτόν τον Α.Φ.Μ.")
                            (:tin-invalid "Άκυρος Α.Φ.Μ."))
                   pobox   ((:parse-error "Άκυροι χαρακτήρες στο αριθμό ταχυδρομικής θυρίδας")
                            (:pobox-invalid "Μη αποδεκτός αριθμός ταχυδρομικής θυρίδας."))
                   zipcode ((:parse-error "Άκυροι χαρακτήρες στον ταχυδρομικό κωδικό")
                            (:zipcode-invalid "Μη αποδεκτός ταχυδρομικός κωδικός.")))
                 params))



;;; ------------------------------------------------------------
;;; Company - Pages
;;; ------------------------------------------------------------

(define-dynamic-page company ("main/company")
    ((id integer chk-company-id)
     (filter string)
     (start integer))
  (no-cache)
  (if (validp id)
      (let ((company-table (make-instance 'company-table
                                          :op 'view
                                          :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Εταιρίες")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'main)
                 (main-menu 'company)
                 (:div :id "company-window" :class "window grid_9"
                       (:div :class "title" "Κατάλογος εταιριών")
                       (company-menu (val id)
                                     (val filter)
                                     (if (val id)
                                         '(view)
                                         '(view update delete)))
                       (display company-table
                                :selected-id (val* id)
                                :start (val* start)))
                 (:div :id "controls" :class "controls grid_3"
                       (company-filters (val filter)))
                 (footer)))))
      (see-other (notfound))))

(define-dynamic-page company/create ("company/create")
    ((filter     string)
     (title      string  chk-new-company-title)
     (occupation string)
     (tof        string  chk-tof-title)
     (tin        string  chk-tin)
     (address    string)
     (city       string  chk-city-title)
     (pobox      integer chk-pobox)
     (zipcode    integer chk-zipcode))
  (no-cache)
  (with-document ()
    (:head
     (:title "Δημιουργία εταιρίας")
     (config-headers))
    (:body
     (:div :id "container" :class "container_12"
           (header 'config)
           (main-menu 'company)
           (:div :id "company-window" :class "window grid_9"
                 (:div :class "title" "Δημιουργία εταιρίας")
                 (company-menu nil
                               (val filter)
                               '(create update delete))
                 (with-form (actions/company/create)
                   (company-data-form 'create
                                      :filter filter
                                      :title title
                                      :occupation occupation
                                      :tof tof
                                      :tin tin
                                      :address address
                                      :city city
                                      :pobox pobox
                                      :zipcode zipcode)))
           (:div :id "controls" :class "controls grid_3"
                 (company-notifications title tof tin city pobox zipcode))
           (footer)))))

(define-dynamic-page company/update ("company/")
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
  (no-cache)
  (with-document ()
    (:head
     (:title "Επεξεργασία εταιρίας")
     (config-headers))
    (:body
     (:div :id "container" :class "container_12"
           (header 'config)
           (main-menu 'company)
           (:div :id "company-window" :class "window grid_9"
                 (:div :class "title" "Επεξεργασία εταιρίας")
                 (company-menu nil
                               (val filter)
                               '(create update delete))
                 (with-form (actions/company/update :id (val id))
                   (company-data-form 'update
                                      :filter filter
                                      :id id
                                      :title title
                                      :occupation occupation
                                      :tof tof
                                      :tin tin
                                      :address address
                                      :city city
                                      :pobox pobox
                                      :zipcode zipcode)))
           (:div :id "controls" :class "controls grid_3"
                 (company-notifications title tof tin city pobox zipcode))
           (footer)))))

(define-dynamic-page company/delete ("company/delete")
    ((id integer chk-company-id)
     (filter string))
  (no-cache)
  (if (validp id)
      (let ((company-table (make-instance 'company-table
                                          :op 'delete
                                          :filter (val* filter))))
        (with-document ()
          (:head
           (:title "Διαγραφή τράπεζας")
           (config-headers))
          (:body
           (:div :id "container" :class "container_12"
                 (header 'config)
                 (main-menu 'company)
                 (:div :id "company-window" :class "window grid_9"
                       (:div :class "title" "Διαγραφή εταιρίας")
                       (company-menu (val id)
                                     (val filter)
                                     '(create delete))
                       (with-form (actions/company/delete :id (val id)
                                                          :filter (val* filter))
                         (display company-table
                                  :selected-id (val id))))
                 (:div :id "controls" :class "controls grid_3"
                       (company-filters (val filter)))
                 (footer)))))
      (see-other (notfound))))

(defun company-data-form (op &key filter id title occupation tof tin address city pobox zipcode)
  (let ((default (if (eql op 'update)
                     (with-db ()
                       (query (:select 'company.title 'occupation
                                       'tin (:as 'tof.title 'tof)
                                       'address (:as 'city.title 'city)
                                       'zipcode 'pobox
                                       :from 'company
                                       :inner-join 'city
                                       :on (:= 'company.city-id 'city.id)
                                       :inner-join 'tof
                                       :on (:=  'company.tof-id 'tof.id)
                                       :where (:= 'company.id (val id)))
                              :plist))
                     nil)))
    (flet ((vod (param) ;; vod = value or default
             (or (val* param)
                 (getf default (key param))))
           (sty (param)
             (if (validp param) "" "attention")))
      (with-html
        (:div :id "company-data-form"
              (:div :id "company-title" :class "grid_9 alpha"
                    (label 'title "Επωνυμία" :style "strong")
                    (textbox 'title
                             :value (vod title)
                             :style (sty title)))
              (:div :id "company-tax-data" :class "grid_4 alpha"
                    (:fieldset
                     (:legend "Φορολογικά στοιχεία")
                     (:ul (:li (label 'occupation "Επάγγελμα")
                               (textbox 'occupation
                                        :value (vod occupation)
                                        :style (sty occupation)))
                          (:li (label 'tin "Α.Φ.Μ.")
                               (textbox 'tin
                                        :value (vod tin)
                                        :style (sty tin)))
                          (:li (label 'tof "Δ.Ο.Υ.")
                               (textbox 'tof
                                        :value (vod tof)
                                        :style (sty tof))))))
              (:div :id "company-address-data" :class "grid_5 omega"
                    (:fieldset
                     (:legend "Διεύθυνση")
                     (:ul (:li (label 'address "Οδός")
                               (textbox 'address
                                        :value (vod address)
                                        :style (sty address)))
                          (:li (label 'city "Πόλη")
                               (textbox 'city
                                        :value (vod city)
                                        :style (sty city)))
                          (:li (label 'zipcode "Ταχυδρομικός κώδικας")
                               (textbox 'zipcode
                                        :value (vod zipcode)
                                        :style (sty zipcode))
                               (label 'pobox "Ταχυδρομική θυρίδα")
                               (textbox 'pobox
                                        :value (vod pobox)
                                        :style (sty pobox))))))
              (:div :id "company-data-form-buttons" :class "grid_9"
                    (ok-button (if (eql op 'update) "Ανανέωση" "Δημιουργία"))
                    (cancel-button (company :filter (vod filter)) "Άκυρο")))))))



;; (defun company-data-form (&key params defaults readonlyp)
;;   (bind (((title occupation tof tin address city zipcode pobox)
;;           (cond ((null params) defaults)
;;                 ((null defaults) (mapcar #'val* params))
;;                 (t (mapcar (lambda (param default)
;;                              (cond ((not (suppliedp param)) default)
;;                                    ((validp param) (val param))
;;                                    (t (raw param))))
;;                            params defaults))))
;;          ((title% occupation% tin% tof% address% city% zipcode% pobox%)
;;           (if params
;;               (mapcar #'(lambda (p) (if (validp p) nil "attention")) params)
;;               (make-list (length defaults)))))
;;     (with-html
;;       (with-table2 (:style "formtable compname") ("compname-legend-col" "compname-field-col")
;;         )
;;       )))





;; ;;; -------------------- Companies --------------------


;; ;;; Database utilities

;; (defun company-defaults (id)
;;   (with-db ()
;;     (query (:select 'company.title 'occupation 'tof.title 'tin
;;                     'address 'city 'zipcode 'pobox
;;                     :from 'company
;;                     :inner-join 'tof
;;                     :on (:= 'tof.id 'company.tof-id)
;;                     :where (:= 'company.id id))
;;            :row)))


;; ;;; Navigation bars

;; (define-navbar companies-navbar () (:id "subnavbar" :ul-style "hmenu")
;;   (all    (companies) (:img :src (url "img/table.png")) "Όλες")
;;   (active (companies) "Με ενεργά έργα")
;;   (debit  (companies) "Χρεώστριες")
;;   (credit (companies) "Πιστώτριες"))

;; (define-navbar company-navbar (company-id) (:id "subnavbar" :ul-style "hmenu")
;;   (overview     (company/view :id company-id)
;;                 (:img :src (url "img/table.png")) "Επισκόπηση")
;;   (cheques      (company/cheques :company-id company-id)
;;                 (:img :src (url "img/table.png")) "Επιταγές")
;;   (transactions (company/transactions :company-id company-id)
;;                 (:img :src (url "img/table.png")) "Συναλλαγές"))


;; ;;; Actions

;; (define-dynamic-page actions/company/create ((title      string  #'not-db-null-p)
;;                                              (occupation string)
;;                                              (tof        string  #'valid-tof-p)
;;                                              (tin        string  #'valid-tin-p)
;;                                              (address    string)
;;                                              (city       string)
;;                                              (pobox      integer #'positive-p)
;;                                              (zipcode    integer #'positive-p))
;;     ("actions/company/create" :request-type :post)
;;   (no-cache)
;;   (with-parameter-list params
;;     (if (every #'validp params)
;;         (with-parameter-rebinding #'val
;;           (with-db ()
;;             (let ((tof-id (tof-id tof)))
;;               (insert-dao
;;                (make-instance 'company
;;                               :title title
;;                               :occupation occupation
;;                               :tof-id tof-id
;;                               :tin tin
;;                               :address address
;;                               :city city
;;                               :zipcode zipcode
;;                               :pobox pobox))
;;               (see-other (companies)))))
;;         (with-parameter-rebinding #'raw
;;           (see-other (company/create :title title
;;                                     :occupation occupation
;;                                     :tof tof
;;                                     :tin tin
;;                                     :address address
;;                                     :city city
;;                                     :zipcode zipcode
;;                                     :pobox pobox))))))

;; (define-dynamic-page actions/company/delete ((id integer #'valid-company-id-p))
;;     ("actions/company/delete" :request-type :post)
;;   (no-cache)
;;   (if (validp id)
;;       (with-db ()
;;         (delete-dao (get-dao 'company (val id)))
;;         (see-other (companies)))
;;       (see-other (company/notfound))))

;; (define-dynamic-page actions/company/update ((id         integer #'valid-company-id-p)
;;                                              (title      string)
;;                                              (occupation string)
;;                                              (tof        string  #'valid-tof-p)
;;                                              (tin        string  #'valid-tin-p)
;;                                              (address    string)
;;                                              (city       string)
;;                                              (pobox      integer #'positive-p)
;;                                              (zipcode    integer #'positive-p))
;;     ("actions/company/update" :request-type :post)
;;   (no-cache)
;;   (with-parameter-list params
;;     (if (every #'validp params)
;;         (with-parameter-rebinding #'val
;;           (with-db ()
;;             (let ((tof-id (tof-id tof)))
;;               (execute (:update 'company :set
;;                                 'title title
;;                                 'occupation occupation
;;                                 'tof-id tof-id
;;                                 'tin tin
;;                                 'address address
;;                                 'city city
;;                                 'pobox pobox
;;                                 'zipcode zipcode
;;                                 :where (:= 'id id)))
;;               (see-other (companies :id id)))))
;;         (with-parameter-rebinding #'raw
;;           (see-other (company/update :id id
;;                                     :title title
;;                                     :occupation occupation
;;                                     :tof tof
;;                                     :tin tin
;;                                     :address address
;;                                     :city city
;;                                     :zipcode zipcode
;;                                     :pobox pobox))))))


;; ;;; Snippets

;; (defun companies-table (active-id)
;;   (with-db ()
;;     (let ((companies (query (:select 'company.id 'company.title 'company.tin 'tof.title
;;                                      :from 'company
;;                                      :inner-join 'tof
;;                                      :on (:= 'company.tof-id 'tof.id))))
;;           (header '("" "Επωνυμία" "Α.Φ.Μ" "Δ.Ο.Υ.")))
;;       (with-html
;;         (:table :id "companies-table" :class "forms-in-row"
;;                 (:thead
;;                  (:tr (iter (for label in header)
;;                             (htm (:th (str label))))))
;;                 (:tbody
;;                  (iter (for (id title tin doy) in companies)
;;                        (let ((activep (and active-id (= active-id id))))
;;                          (htm
;;                           (:tr :class (if activep "active" nil)
;;                                (:td :class "select"
;;                                     (:a :href (companies :id id)
;;                                         (:img :src (url (if activep
;;                                                             "img/bullet_red.png"
;;                                                             "img/bullet_blue.png")))))
;;                                (:td (:a :href (company/view :id id)
;;                                         (str (lisp-to-html title))))
;;                                (:td (:p (str (lisp-to-html tin))))
;;                                (:td (:p (str (lisp-to-html doy))))))))))))))

;; (define-menu company-menu (id) (:div-style "actions" :ul-style "hmenu")
;;   (:create (with-html
;;              (:li (:a :href (company/create)
;;                       (:img :src (url "img/add.png")) "Δημιουργία"))))
;;   (:view (if id
;;              (with-html
;;                (:li (:a :href (company/view :id id)
;;                         (:img :src (url "img/magnifier.png")) "Προβολή")))
;;              nil))
;;   (:edit (if id
;;              (with-html
;;                (:li (:a :href (company/update :id id)
;;                         (:img :src (url "img/pencil.png")) "Επεξεργασία")))
;;              nil))
;;   (:delete (with-db ()
;;              (let ((tx-exist-p (and id
;;                                     (query (:select 'id
;;                                                     :from 'tx
;;                                                     :where (:= 'company-id id))))))
;;                (if (or (null id) tx-exist-p)
;;                    nil
;;                    (with-html
;;                      (:li (:a :href (company/delete :id id)
;;                               (:img :src (url "img/delete.png")) "Διαγραφή"))))))))

;; (defun company-data-form (&key params defaults readonlyp)
;;   (bind (((title occupation tof tin address city zipcode pobox)
;;           (cond ((null params) defaults)
;;                 ((null defaults) (mapcar #'val* params))
;;                 (t (mapcar (lambda (param default)
;;                              (cond ((not (suppliedp param)) default)
;;                                    ((validp param) (val param))
;;                                    (t (raw param))))
;;                            params defaults))))
;;          ((title% occupation% tin% tof% address% city% zipcode% pobox%)
;;           (if params
;;               (mapcar #'(lambda (p) (if (validp p) nil "attention")) params)
;;               (make-list (length defaults)))))
;;     (with-html
;;       (with-table2 (:style "formtable compname") ("compname-legend-col" "compname-field-col")
;;         ((label 'title "Επωνυμία:" :style "strong")
;;          (textbox 'title
;;                   :value title
;;                   :readonlyp readonlyp
;;                   :style title%)))
;;       (:fieldset
;;        (:legend "Φορολογικά στοιχεία")
;;        (with-table2 (:style "formtable tax") ("tax-legend-col" "tax-field-col")
;;          ((label 'occupation "Επάγγελμα:")
;;           (textbox 'occupation
;;                    :value occupation
;;                    :readonlyp readonlyp
;;                    :style occupation%))
;;          ((label 'tin "Α.Φ.Μ.:")
;;           (textbox 'tin
;;                    :value tin
;;                    :readonlyp readonlyp
;;                    :style tin%))
;;          ((label 'tof "Δ.Ο.Υ.:")
;;           (textbox 'tof
;;                    :value tof
;;                    :readonlyp readonlyp
;;                    :style tof%))))
;;       (:fieldset
;;        (:legend "Διεύθυνση")
;;        (with-table2 (:style "formtable address") ("addr-legend-col" "addr-field-col")
;;          ((label 'address "Οδός:")
;;           (textbox 'address
;;                    :value address
;;                    :readonlyp readonlyp
;;                    :style address%))
;;          ((label 'city "Πόλη:")
;;           (textbox 'city
;;                    :value city
;;                    :readonlyp readonlyp
;;                    :style city%))
;;          ((label 'zipcode "Ταχυδρομικός κώδικας:")
;;           (textbox 'zipcode
;;                    :value zipcode
;;                    :readonlyp readonlyp
;;                    :style zipcode%))
;;          ((label 'pobox "Ταχυδρομική θυρίδα:")
;;           (textbox 'pobox
;;                    :value pobox
;;                    :readonlyp readonlyp
;;                    :style pobox%)))))))


;; (defun company-data-view (id defaults)
;;   (with-html
;;     (:div :id "company-data" :class "window"
;;           (company-menu id :edit :delete)
;;           (:h2 "Στοιχεία εταιρίας")
;;           (company-data-form :defaults defaults :readonlyp t))))

;; (defun company-data-update (id params defaults)
;;   (with-html
;;     (:div :id "company-data" :class "window"
;;           (company-menu (val id) :view :delete)
;;           (:h2 "Επεξεργασία εταιρίας")
;;           (with-form (actions/company/update :id (val id))
;;             (company-data-form :params params :defaults defaults)
;;             (:ul :class "prompt hmenu"
;;                  (:li (submit "Ενημέρωση"))
;;                  (:li (:a :href (company/view :id (val id)) "Ακύρωση")))))))

;; (defun company-data-delete (id defaults)
;;   (with-html
;;     (:div :id "company-data" :class "window"
;;           (company-menu id  :view :edit)
;;           (:h2 "Διαγραφή εταιρίας")
;;           (with-form (actions/company/delete :id id)
;;             (company-data-form :defaults defaults :readonlyp t)
;;             (:ul :class "prompt hmenu"
;;                  (:li (submit "Διαγραφή"))
;;                  (:li (:a :href (company/view :id id) "Ακύρωση")))))))


;; ;;; Pages

;; (define-dynamic-page companies ((id integer #'valid-company-id-p)) ("companies")
;;   (no-cache)
;;   (if (validp id)
;;       (with-parameter-rebinding #'val
;;         (with-page ()
;;           (:head
;;            (:title "Εταιρίες")
;;            (head-css-std))
;;           (:body
;;            (:div :id "header"
;;                  (logo)
;;                  (primary-navbar 'companies)
;;                  (companies-navbar 'all))
;;            (:div :id "body"
;;                  (:div :class "message"
;;                        (:h2 :class "info" "Κατάλογος Εταιριών"))
;;                  (:div :id "companies" :class "window"
;;                        (company-menu id :create :view :edit :delete)
;;                        (companies-table id))
;;                  (footer)))))
;;       (see-other (company/notfound))))

;; (define-dynamic-page company/create ((title      string  #'not-db-null-p)
;;                                      (occupation string)
;;                                      (tof        string  #'valid-tof-p)
;;                                      (tin        string  #'valid-tin-p)
;;                                      (address    string)
;;                                      (city       string)
;;                                      (zipcode    integer #'positive-p)
;;                                      (pobox      integer #'positive-p))
;;     ("company/create")
;;   (no-cache)
;;   (with-parameter-list params
;;     (with-page ()
;;       (:head
;;        (:title "Εισαγωγή εταιρίας")
;;        (head-css-std)
;;        (head-js-std))
;;       (:body
;;        (:div :id "header"
;;              (logo)
;;              (primary-navbar 'companies))
;;        (:div :id "body"
;;              (:div :class "message"
;;                    (:h2 :class "info" "Εισαγωγή εταιρίας")
;;                    (company-errorbar title tof tin city pobox zipcode))
;;              (:div :id "content" :class "window"
;;                    (with-form (actions/company/create)
;;                      (company-data-form :params params)
;;                      (:ul :class "prompt hmenu"
;;                           (:li (submit "Δημιουργία"))
;;                           (:li (:a :href (companies) "Ακύρωση")))))
;;              (footer))))))

;; (define-dynamic-page company/view ((id integer #'valid-company-id-p)
;;                                    (contact-id integer #'valid-contact-id-p))
;;     ("company/view")
;;   (no-cache)
;;   (if (and (validp id) (validp contact-id))
;;       (let ((defaults (company-defaults (val id))))
;;         (with-page ()
;;           (:head
;;            (:title "Εταιρία: " (str (getf defaults 'title)))
;;            (head-css-std))
;;           (:body
;;            (:div :id "header"
;;                  (logo)
;;                  (primary-navbar 'companies)
;;                  (company-navbar 'overview (val id)))
;;            (:div :id "body"
;;                  (company-data-view (val id) defaults)
;;                  (contact-data-form (val id) :view (val contact-id))
;;                  (footer)))))
;;       (see-other (notfound))))

;; (define-dynamic-page company/update ((id         integer #'valid-company-id-p)
;;                                      (title      string  #'not-db-null-p)
;;                                      (occupation string)
;;                                      (tof        string  #'valid-tof-p)
;;                                      (tin        string  #'valid-tin-p)
;;                                      (address    string)
;;                                      (city       string)
;;                                      (zipcode    integer #'positive-p)
;;                                      (pobox      integer #'positive-p))
;;     ("company/update")
;;   (no-cache)
;;   (if (validp id)
;;       (with-parameter-list params
;;         (let ((defaults (company-defaults (val id))))
;;           (with-page ()
;;             (:head
;;              (:title "Επεξεργασία εταιρίας: " (str (getf defaults 'title)))
;;              (head-css-std)
;;              (head-js-std))
;;             (:body
;;              (:div :id "header"
;;                    (logo)
;;                    (primary-navbar 'companies)
;;                    (company-navbar 'overview (val id)))
;;              (:div :id "body"
;;                    (:div :id "message"
;;                          (:h2 :class "info" "Επεξεργασία εταιρίας")
;;                          (company-errorbar title tof tin city pobox zipcode))
;;                    (company-data-update id (rest params) defaults)
;;                    (contact-data-form (val id) :view)
;;                    (footer))))))
;;       (see-other (company/notfound))))

;; (define-dynamic-page company/delete ((id integer #'valid-company-id-p))
;;     ("company/delete")
;;   (no-cache)
;;   (if (validp id)
;;       (with-parameter-rebinding #'val
;;         (let ((defaults (company-defaults id)))
;;           (with-page ()
;;             (:head
;;              (:title "Διαγραφή εταιρίας:" (str (getf defaults 'title)))
;;              (head-css-std))
;;             (:body
;;              (:div :id "header"
;;                    (logo)
;;                    (primary-navbar 'companies)
;;                    (company-navbar 'overview id))
;;              (:div :id "body"
;;                    (company-data-delete id defaults)
;;                    (contact-data-form id :view)
;;                    (footer))))))
;;       (see-other (company/notfound))))

;; (define-dynamic-page company/cheques ((company-id integer #'valid-company-id-p))
;;     ("company/cheques")
;;   (if (validp company-id)
;;       (with-parameter-rebinding #'val
;;         (let ((defaults (company-defaults company-id)))
;;           (with-page ()
;;             (:head
;;              (:title "Εταιρία: " (str (getf defaults 'title)))
;;              (head-css-std))
;;             (:body
;;              (:div :id "header"
;;                    (logo)
;;                    (primary-navbar 'companies)
;;                    (company-navbar 'cheques company-id))
;;              (:div :id "body"
;;                    (:div :id "companies" :class "window"
;;                          (company-menu company-id :create :view :edit :delete)
;;                          (:h2 "Πίνακας Επιταγών"))
;;                    (footer))))))
;;       (see-other (company/notfound))))

;; (define-dynamic-page company/transactions ((company-id integer #'valid-company-id-p))
;;     ("company/transactions")
;;   (if (validp company-id)
;;       (with-parameter-rebinding #'val
;;         (with-html
;;           (let ((defaults (company-defaults company-id)))
;;             (with-page ()
;;               (:head
;;                (:title "Εταιρία: " (str (getf defaults 'title)))
;;                (head-css-std))
;;               (:body
;;                (:div :id "header"
;;                      (logo)
;;                      (primary-navbar 'companies)
;;                      (company-navbar 'transactions company-id))
;;                (:div :id "body"
;;                      (:div :id "companies" :class "window"
;;                            (company-menu company-id :create :view :edit :delete)
;;                            (:h2 "Πίνακας Συναλλαγών"))
;;                      (footer)))))))
;;       (see-other (company/notfound))))

;; (define-dynamic-page company/notfound () ("company/notfound")
;;   (no-cache)
;;   (with-page ()
;;     (:head
;;      (:title "Άγνωστη εταιρία")
;;      (head-css-std))
;;     (:body
;;      (:div :id "header"
;;            (logo)
;;            (primary-navbar 'companies))
;;      (:div :id "body"
;;            (:div :id "content" :class "window"
;;                  (:p "Η εταιρία που προσπαθείτε να προσπελάσετε δεν υπάρχει.")
;;                  (:p "Επιστρέψτε στο μενού των εταιριών και προσπαθήστε ξανά."))))))



;; ;;; -------------------- Contacts --------------------


;; ;;; Actions

;; (define-dynamic-page actions/contact/create ((company-id integer #'valid-company-id-p)
;;                                              (tag string)
;;                                              (phone string))
;;     ("actions/contact/create" :request-type :post)
;;   (with-parameter-list params
;;     (if (every #'validp params)
;;         (with-parameter-rebinding #'val
;;           (with-db ()
;;             (insert-dao (make-instance 'contact
;;                                        :company-id company-id
;;                                        :tag tag
;;                                        :phone phone))
;;             (redirect (company/view :id company-id))))
;;         (with-parameter-rebinding #'raw
;;          (see-other (company/create-contact :company-id company-id))))))

;; (define-dynamic-page actions/contact/delete ((contact-id integer #'valid-contact-id-p))
;;     ("actions/contact/delete" :request-type :post)
;;   (if (validp contact-id)
;;       (with-parameter-rebinding #'val
;;         (with-db ()
;;           (let ((dao (get-dao 'contact contact-id)))
;;             (delete-dao dao)
;;             (redirect (company/view :id (company-id dao))))))
;;       (see-other (notfound))))

;; (define-dynamic-page actions/contact/update ((contact-id integer #'valid-contact-id-p)
;;                                              (tag string)
;;                                              (phone string))
;;     ("actions/contact/update" :request-type :post)
;;   (with-parameter-list params
;;     (if (every #'validp params)
;;         (with-parameter-rebinding #'val
;;           (with-db ()
;;             (let ((dao (get-dao 'contact contact-id)))
;;               (setf (tag dao) tag)
;;               (setf (phone dao) phone)
;;               (update-dao dao)
;;               (redirect (company/view :id (company-id dao) :contact-id contact-id)))))
;;         (see-other (notfound)))))

;; ;;; Snippets

;; (define-menu contact-menu (company-id contact-id) ()
;;   (:create (lambda ()
;;              (with-html
;;                (:li (:a :href (company/create-contact :company-id company-id)
;;                         (:img :src (url "img/add.png")) "Δημιουργία")))))
;;   (:edit (lambda ()
;;            (unless (null contact-id)
;;              (with-html
;;                (:li (:a :href (company/update-contact :contact-id contact-id)
;;                         (:img :src (url "img/pencil.png")) "Επεξεργασία"))))))
;;   (:delete (lambda ()
;;              (unless (null contact-id)
;;                (with-html
;;                  (:li (:a :href (company/delete-contact :contact-id contact-id)
;;                           (:img :src (url "img/delete.png")) "Διαγραφή")))))))

;; (defun contact-data-form-prologue (company-id contact-id intent)
;;   (case intent
;;     (:view (contact-menu company-id contact-id :create :edit :delete)
;;            (with-html
;;              (:h2 "Επαφές")))
;;     (:create (contact-menu company-id nil)
;;              (with-html
;;                (:h2 "Δημιουργία επαφής")))
;;     (:delete (contact-menu company-id contact-id :create :edit)
;;              (with-html
;;                (:h2 "Διαγραφή επαφής")))
;;     (:update (contact-menu company-id contact-id :create :delete)
;;              (with-html
;;                (:h2 "Επεξεργασία επαφής")))))

;; (defun contact-data-form (company-id intent &optional contact-id)
;;   (flet ((normal-row (cid tag phone activep)
;;            (with-html
;;              (:tr :class (if activep "active" nil)
;;                   (:td :class "select"
;;                        (:a :href (company/view :id company-id :contact-id cid)
;;                            (:img :src (str (if activep
;;                                                (url "img/bullet_red.png")
;;                                                (url "img/bullet_blue.png"))))))
;;                   (:td :class "data"
;;                        (str (lisp-to-html tag)))
;;                   (:td :class "data"
;;                        (str (lisp-to-html phone)))
;;                   (:td :class "button" "")
;;                   (:td :class "button" ""))))
;;          (form-row-create ()
;;            (with-form (actions/contact/create :company-id company-id)
;;              (:tr :class "active"
;;                   (:td :class "select"
;;                        (:a :href (company/view :id company-id)
;;                            (:img :src (url "img/bullet_red.png"))))
;;                   (:td :class "data" (textbox 'tag))
;;                   (:td :class "data" (textbox 'phone))
;;                   (:td :class "button"
;;                        (ok-button))
;;                   (:td :class "button"
;;                        (cancel-button (company/view :id company-id))))))
;;          (form-row-update (cid tag phone)
;;            (with-form (actions/contact/update :contact-id contact-id)
;;              (with-html
;;                (:tr :class "active"
;;                     (:td :class "select"
;;                          (:a :href (company/view :id company-id :contact-id cid)
;;                              (:img :src (url "img/bullet_red.png"))))
;;                     (:td :class "data"
;;                          (htm (textbox 'tag :value tag)))
;;                     (:td :class "data"
;;                          (htm (textbox 'phone :value phone)))
;;                     (:td :class "button"
;;                          (ok-button))
;;                     (:td :class "button"
;;                          (cancel-button (company/view :id company-id
;;                                                       :contact-id contact-id)))))))
;;          (form-row-delete (cid tag phone)
;;            (with-form (actions/contact/delete :contact-id contact-id)
;;              (:tr :class "attention"
;;                   (:td :class "select"
;;                        (:a :href (company/view :id company-id :contact-id cid)
;;                            (:img :src (url "img/bullet_red.png"))))
;;                   (:td :class "data"
;;                        (str (lisp-to-html tag)))
;;                   (:td :class "data"
;;                        (str (lisp-to-html phone)))
;;                   (:td :class "button"
;;                        (ok-button))
;;                   (:td :class "button"
;;                        (cancel-button (company/view :id company-id
;;                                                     :contact-id contact-id)))))))
;;     (let ((header '("" "Περιγραφή" "Αριθμός" "" ""))
;;           (contacts (with-db ()
;;                       (query (:select 'id 'tag 'phone
;;                                       :from 'contact
;;                                       :where (:= 'company-id company-id))))))
;;       (if (or contacts (eql intent :create))
;;           (with-html
;;             (:div :id "contact-data" :class "window"
;;                   (contact-data-form-prologue company-id contact-id intent)
;;                   (:table :id "contacts-table" :class "forms-in-row"
;;                           (:thead
;;                            (:tr (iter (for label in header)
;;                                       (htm (:th (str label))))))
;;                           (:tbody
;;                            (when (eql intent :create)
;;                              (form-row-create))
;;                            (iter (for (cid tag phone) in contacts)
;;                                  (for activep = (eql contact-id cid))
;;                                  (if activep
;;                                      (case intent
;;                                        (:view (normal-row cid tag phone activep))
;;                                        (:update (form-row-update cid tag phone))
;;                                        (:delete (form-row-delete cid tag phone)))
;;                                      (normal-row cid tag phone activep)))))))
;;           (with-html
;;             (:div :id "contact-data" :class "window"
;;                   (contact-menu company-id nil :create)
;;                   (:h2 "Δεν υπάρχουν επαφές")))))))


;; ;;; Pages

;; (define-dynamic-page company/create-contact ((company-id integer #'valid-company-id-p))
;;     ("company/create-contact")
;;   (no-cache)
;;   (if (validp company-id)
;;       (with-parameter-rebinding #'val
;;         (let ((defaults (company-defaults company-id)))
;;           (with-page ()
;;             (:head
;;              (:title "Εταιρία: Δημιουργία επαφής")
;;              (head-css-std))
;;             (:body
;;              (:div :id "header"
;;                    (logo)
;;                    (primary-navbar 'companies)
;;                    (company-navbar 'overview company-id))
;;              (:div :id "body"
;;                    (company-data-view company-id defaults)
;;                    (contact-data-form company-id :create))))))
;;       (see-other (company/notfound))))

;; (define-dynamic-page company/update-contact ((contact-id integer #'valid-contact-id-p))
;;     ("company/update-contact")
;;   (no-cache)
;;   (if (validp contact-id)
;;       (with-parameter-rebinding #'val
;;         (with-db ()
;;           (let* ((company-id (query (:select 'company-id
;;                                              :from 'contact
;;                                              :where (:= 'id contact-id))
;;                                     :single))
;;                  (defaults (company-defaults company-id)))
;;             (with-page ()
;;               (:head
;;                (:title "Εταιρία: Επεξεργασία επαφής")
;;                (head-css-std))
;;               (:body
;;                (:div :id "header"
;;                      (logo)
;;                      (primary-navbar 'companies)
;;                      (company-navbar 'overview company-id))
;;                (:div :id "body"
;;                      (company-data-view company-id defaults)
;;                      (contact-data-form company-id :update contact-id)))))))
;;       (see-other (notfound))))

;; (define-dynamic-page company/delete-contact ((contact-id integer #'valid-contact-id-p))
;;     ("company/delete-contact")
;;   (no-cache)
;;   (if (validp contact-id)
;;       (with-parameter-rebinding #'val
;;         (with-db ()
;;          (let* ((company-id (query (:select 'company-id
;;                                             :from 'contact
;;                                             :where (:= 'id contact-id))
;;                                    :single))
;;                 (defaults (company-defaults company-id)))
;;            (with-page ()
;;              (:head
;;               (:title "Εταιρία: Διαγραφή επαφής")
;;               (head-css-std))
;;              (:body
;;               (:div :id "header"
;;                     (logo)
;;                     (primary-navbar 'companies)
;;                     (company-navbar 'overview company-id))
;;               (:div :id "body"
;;                     (company-data-view company-id defaults)
;;                     (contact-data-form company-id :delete contact-id)))))))
;;       (see-other (notfound))))
