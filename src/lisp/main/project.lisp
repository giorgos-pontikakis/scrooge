(in-package :scrooge)

;;; Actions

(define-dynamic-page actions/project/create ((company     string  #'valid-company-p)
                                             (description string)
                                             (location    string)
                                             (price       integer #'positive-p)
                                             (start-date  date)
                                             (end-date    date)
                                             (status      string)
                                             (vat         integer #'positive-p))
    ("actions/project/create" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
        (with-parameter-rebinding #'val
          (with-db ()
            (let ((company-id (company-id company)))
              (insert-dao
               (make-instance 'project
                              :company-id company-id
                              :description description
                              :location location
                              :price price
                              :start-date start-date
                              :end-date end-date
                              :status status
                              :vat vat))
              (see-other (projects)))))
        (with-parameter-rebinding #'raw
          (see-other (project/create :company company
                                    :description description
                                    :location location
                                    :price price
                                    :start-date start-date
                                    :end-date end-date
                                    :status status
                                    :vat vat))))))

(define-dynamic-page actions/project/delete ((id integer #'valid-project-id-p))
    ("actions/project/delete" :request-type :post)
  (no-cache)
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'project (val id)))
        (see-other (projects)))
      (see-other (project/notfound))))

(define-dynamic-page actions/project/update ((id          integer #'valid-project-id-p)
                                             (company     string  #'valid-company-p)
                                             (description string)
                                             (location    string)
                                             (price       integer #'positive-p)
                                             (start-date  date)
                                             (end-date    date)
                                             (status      string)
                                             (vat         integer #'positive-p))
    ("actions/project/update" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
        (with-parameter-rebinding #'val
          (with-db ()
            (let ((company-id (company-id company)))
              (execute (:update 'project :set
                                'company-id company-id
                                'description description
                                'location location
                                'price price
                                'start-date start-date
                                'end-date end-date
                                'status status
                                'vat vat
                                :where (:= 'id id)))
              (see-other (projects :id id)))))
        (with-parameter-rebinding #'raw
          (see-other (project/update :company company
                                    :description description
                                    :location location
                                    :price price
                                    :start-date start-date
                                    :end-date end-date
                                    :status status
                                    :vat vat))))))


;;; Snippets

(define-navbar projects-navbar () (:id "subnavbar" :ul-style "hmenu")
  (all      (projects) (:img :src (url "img/table.png")))
  (quoted   (projects) "Δόθηκε προσφορά")
  (ongoing  (projects) "Σε εξέλιξη")
  (finished (projects) "Ολοκληρώθηκε"))

(defun project-defaults (id)
  (with-db ()
    (query (:select 'description 'location 'price 'start-date 'end-date 'status 'vat
                    :from 'project
                    :inner-join 'company
                    :on (:= 'project.company-.id 'company.id)
                    :where (:= 'project.id id))
           :row)))

(define-errorbar project-errorbar ()
  (company    "Άκυρο όνομα εταιρίας")
  (price      "Η τιμή πρέπει να είναι μεγαλύτερη από το μηδέν")
  (start-date "Μη αποδεκτή ημερομηνία")
  (end-date   "Μη αποδεκτή ημερομηνία")
  (vat        "Ο Φ.Π.Α. πρέπει να είναι θετικός αριθμός ή μηδέν"))

(defun projects-table (active-id)
  (with-db ()
    (let ((projects (query (:select 'id 'title 'location 'price 'status
                                    :from 'project)))
          (header '("" "Περιγραφή" "Τοποθεσία" "Τιμή" "Κατάσταση")))
      (with-html
        (:table :id "projects-table" :class "forms-in-row"
                (:thead
                 (:tr (iter (for label in header)
                            (htm (:th (str label))))))
                (:tbody
                 (iter (for (id title location price status) in projects)
                       (let ((activep (and active-id (= active-id id))))
                         (htm
                          (:tr :class (if activep "active" nil)
                               (:td :class "select"
                                    (:a :href (projects :id id)
                                        (:img :src (url (if activep
                                                            "img/bullet_red.png"
                                                            "img/bullet_blue.png")))))
                               (:td (:a :href (project/view :id id)
                                        (str (lisp-to-html title))))
                               (:td (:p (str (lisp-to-html location))))
                               (:td (:p (str (lisp-to-html price))))
                               (:td (:p (str (lisp-to-html status))))))))))))))

(define-menu project-menu (id) ()
  (:create (lambda ()
             (with-html
               (:li (:a :href (project/create)
                        (:img :src (url "img/add.png")) "Δημιουργία")))))
  (:view (lambda ()
           (if id
               (with-html
                 (:li (:a :href (project/view :id id)
                          (:img :src (url "img/magnifier.png")) "Προβολή")))
               nil)))
  (:edit (lambda ()
           (if id
               (with-html
                 (:li (:a :href (project/update :id id)
                          (:img :src (url "img/pencil.png")) "Επεξεργασία")))
               nil)))
  (:delete (lambda ()
             (if id
                 (with-html
                   (:li (:a :href (project/delete :id id)
                            (:img :src (url "img/delete.png")) "Διαγραφή")))
                 nil))))

(defun project-data-form (&key params defaults readonlyp)
  (bind (((description location company status start-date end-date)
          (cond ((null params) defaults)
                ((null defaults) (mapcar #'val* params))
                (t (mapcar (lambda (param default)
                             (cond ((not (suppliedp param)) default)
                                   ((validp param) (val param))
                                   (t (raw param))))
                           params defaults))))
         ((description% location% company% status% start-date% end-date%)
          (if params
              (mapcar #'(lambda (p) (if (validp p) nil "attention")) params)
              (make-list (length defaults)))))
    (with-html
      (:fieldset
       (:legend "")
       (with-table2 (:style "formtable tax") ("tax-legend-col" "tax-field-col")
         ((label 'description "Περιγραφή:")
          (textbox 'description
                   :value description
                   :readonlyp readonlyp
                   :style description%))
         ((label 'location "Τοποθεσία:")
          (textbox 'location
                   :value location
                   :readonlyp readonlyp
                   :style location%))
         ((label 'company "")
          (textbox 'company
                   :value company
                   :readonlyp readonlyp
                   :style company%))))
      (:fieldset
       (:legend "")
       (with-table2 (:style "formtable status") ("addr-legend-col" "addr-field-col")
         ((label 'status "Κατάσταση:")
          (textbox 'status
                   :value status
                   :readonlyp readonlyp
                   :style status%))
         ((label 'start-date "Ημερομηνία έναρξης:")
          (textbox 'start-date
                   :value start-date
                   :readonlyp readonlyp
                   :style start-date%))
         ((label 'end-date "Ημερομηνία ολοκλήρωσης:")
          (textbox 'end-date
                   :value end-date
                   :readonlyp readonlyp
                   :style end-date%)))))))

(defun project-data-view (id defaults)
  (with-html
    (:div :id "project-data" :class "window"
          (project-menu id :edit :delete)
          (:h2 "Στοιχεία έργου")
          (project-data-form :defaults defaults :readonlyp t))))

(defun project-data-update (id params defaults)
  (with-html
    (:div :id "project-data" :class "window"
          (project-menu (val id) :view :delete)
          (:h2 "Επεξεργασία έργου")
          (with-form (actions/project/update :id (val id))
            (project-data-form :params params :defaults defaults)
            (:ul :class "prompt hmenu"
                 (:li (submit "Ενημέρωση"))
                 (:li (:a :href (project/view :id (val id)) "Ακύρωση")))))))

(defun project-data-delete (id defaults)
  (with-html
    (:div :id "project-data" :class "window"
          (project-menu id  :view :edit)
          (:h2 "Διαγραφή έργου")
          (with-form (actions/project/delete :id id)
            (project-data-form :defaults defaults :readonlyp t)
            (:ul :class "prompt hmenu"
                 (:li (submit "Διαγραφή"))
                 (:li (:a :href (project/view :id id) "Ακύρωση")))))))

;;; Pages

(define-dynamic-page projects ((id integer #'valid-project-id-p)) ("projects")
  (no-cache)
  (if (validp id)
      (with-parameter-rebinding #'val
        (with-page ()
          (:head
           (:title "Έργα")
           (head-css-std))
          (:body
           (:div :id "header"
                 (logo)
                 (primary-navbar 'projects)
                 (projects-navbar 'all))
           (:div :id "body"
                 (:div :id "msg" :class "message"
                       (:h2 "Κατάλογος Έργων"))
                 (:div :id "companies" :class "window"
                       (project-menu id :create :view :edit :delete)
                       (projects-table id))
                 (footer)))))
      (see-other (project/notfound))))

(define-dynamic-page project/create ((company     string  #'valid-company-p)
                                     (description string)
                                     (location    string)
                                     (price       integer #'positive-p)
                                     (start-date  date)
                                     (end-date    date)
                                     (status      string)
                                     (vat         integer #'positive-p))
    ("project/create")
  (no-cache)
  (with-parameter-list params
    (with-page ()
      (:head
       (:title "Εισαγωγή έργου")
       (head-css-std)
       (head-js-std))
      (:body
       (:div :id "header"
             (logo)
             (primary-navbar 'projects))
       (:div :id "body"
             (:div :id "msg"
                   (:h2 "Εισαγωγή έργου"))
             (project-errorbar company price start-date end-date vat)
             (:div :id "content" :class "window"
                   (with-form (actions/project/create)
                     (project-data-form :params params)
                     (:ul :class "prompt hmenu"
                          (:li (submit "Δημιουργία"))
                          (:li (:a :href (projects) "Ακύρωση")))))
             (footer))))))

(define-dynamic-page project/view ((id integer #'valid-project-id-p))
    ("project/view")
  (no-cache)
  (if (validp id)
      (let ((defaults (project-defaults (val id))))
        (with-page ()
          (:head
           (:title "Έργο: " (str (getf defaults 'title)))
           (head-css-std))
          (:body
           (:div :id "header"
                 (logo)
                 (primary-navbar 'projects)
                 #|(project-navbar 'overview (val id))|#)
           (:div :id "body"
                 (project-data-view (val id) defaults)
                 (footer)))))
      (see-other (project/notfound))))

(define-dynamic-page project/update ((id          integer #'valid-project-id-p)
                                     (company     string  #'valid-company-p)
                                     (description string)
                                     (location    string)
                                     (price       integer #'positive-p)
                                     (start-date  date)
                                     (end-date    date)
                                     (status      string)
                                     (vat         integer #'positive-p))
    ("project/update")
  (no-cache)
  (if (validp id)
      (with-parameter-list params
        (let ((defaults (project-defaults (val id))))
          (with-page ()
            (:head
             (:title "Επεξεργασία έργρου: " (str (getf defaults 'project)))
             (head-css-std)
             (head-js-std))
            (:body
             (:div :id "header"
                   (logo)
                   (primary-navbar 'projects)
                   #|(project-navbar 'overview (val id))|#)
             (:div :id "body"
                   (project-errorbar company price start-date end-date vat)
                   (project-data-update id (rest params) defaults)
                   (footer))))))
      (see-other (project/notfound))))

(define-dynamic-page project/delete ((id integer #'valid-project-id-p))
    ("project/delete")
  (no-cache)
  (if (validp id)
      (with-parameter-rebinding #'val
        (let ((defaults (project-defaults id)))
          (with-page ()
            (:head
             (:title "Διαγραφή έργου:" (str (getf defaults 'title)))
             (head-css-std))
            (:body
             (:div :id "header"
                   (logo)
                   (primary-navbar 'projects)
                   #|(project-navbar 'overview id)|#)
             (:div :id "body"
                   (project-data-delete id defaults)
                   (contact-data-form id :view)
                   (footer))))))
      (see-other (project/notfound))))

(define-dynamic-page project/notfound () ("project/notfound")
  (no-cache)
  (with-page ()
    (:head
     (:title "Άγνωστο έργο")
     (head-css-std))
    (:body
     (:div :id "header"
           (logo)
           (primary-navbar 'projects))
     (:div :id "body"
           (:div :id "content" :class "window"
                 (:p "Το έργο που προσπαθείτε να προσπελάσετε δεν υπάρχει.")
                 (:p "Επιστρέψτε στο μενού των έργων και προσπαθήστε ξανά."))))))
