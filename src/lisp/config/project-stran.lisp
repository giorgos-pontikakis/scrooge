(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass project-stran-page (auth-dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(project-stran-id))
   (payload-parameter-names
    :allocation :class
    :initform '(title from-state to-state temtx))
   (filter-parameter-names
    :allocation :class
    :initform '())
   (messages
    :allocation :class
    :reader messages
    :initform
    '((title
       (:project-stran-title-null
        "Η περιγραφή της Μετάπτωσης είναι κενή."
        :project-stran-title-exists
        "Υπάρχει ήδη Μετάπτωση με αυτή την περιγραφή."))
      (from-state-id
       (:project-stran-from/to-exists
        "Έχει ήδη οριστεί συμβάν για αυτή την αρχική και τελική κατάσταση"
        :project-stran-from-to-equal
        "Η τελική κατάσταση δεν μπορεί να είναι ίδια με την αρχική κατάσταση."))
      (temtx
       (:temtx-title-null
        "Η περιγραφή της Πρότυπης Συναλλαγής είναι κενή."
        :temtx-title-unknown
        "Δεν έχει οριστεί Πρότυπη Συναλλαγή με αυτή την περιγραφή."))))))



;;; ----------------------------------------------------------------------
;;; Checks
;;; ----------------------------------------------------------------------

(define-existence-predicate project-stran-exists-p project-stran project-stran-id)
(define-existence-predicate* project-stran-title-exists-p project-stran title project-stran-id)

(defun chk-project-stran-id (project-stran-id)
  (if (project-stran-exists-p project-stran-id)
      nil
      :project-stran-id-unknown))

(defun chk-project-stran-title (title)
  (if (eql title :null)
      :project-stran-title-null
      nil))

(defun chk-project-stran-title/create (title)
  (cond ((eql :null title) :project-stran-title-null)
        ((project-stran-title-exists-p title) :project-stran-title-exists)))

(defun chk-project-stran-title/update (title project-stran-id)
  (cond ((eql :null title) :project-stran-title-null)
        ((project-stran-title-exists-p title project-stran-id) :project-stran-title-exists)))

(defun chk-project-state-id (state-id)
  (if (project-state-id-exists-p state-id)
      nil
      :project-state-id-invalid))

(defun chk-project-state-id* (state-id)
  "Same with chk-project-state but accepts null values"
  (if (or (eql state-id :null)
          (project-state-id-exists-p state-id))
      nil
      :project-state-id-invalid))


;;; post checks

(defun project-stran-from/to-exists-p (from-state-id to-state-id)
  (if (or (null from-state-id) (null to-state-id))
      nil
      (with-db ()
        (query (:select 1 :from 'project-stran
                :where (:and (:= 'from-state-id from-state-id)
                             (:= 'to-state-id to-state-id)))
               :plists))))

(defun check-project-stran-parameters (from-state-id to-state-id)
  (validate-parameters (lambda (from-state-id to-state-id)
                         (let ((from (val from-state-id))
                               (to (val to-state-id)))
                           (cond ((project-stran-from/to-exists-p from to)
                                  :project-stran-from/to-exists)
                                 ((string= from to)
                                  :project-stran-from-to-equal))))
                       from-state-id to-state-id)
  nil)



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun project-stran-actions (op project-stran-id)
  (actions-menu (make-menu-spec
                 (action-anchors/crud (config/project-stran/create)
                                      (config/project-stran/update :project-stran-id project-stran-id)
                                      (config/project-stran/delete :project-stran-id project-stran-id)))
                (enabled-actions/crud op project-stran-id)))



;;; ------------------------------------------------------------
;;; Project Stran Table
;;; ------------------------------------------------------------

;;; table

(defclass project-stran-table (scrooge-crud-table)
  ((header-labels  :initform '("" "<br />Περιγραφή"
                               "Αρχική<br />Κατάσταση" "Τελική<br />Κατάσταση"
                               " Πρότυπη<br />Συναλλαγή"))
   (paginator      :initform nil))
  (:default-initargs :item-class 'project-stran-row))

(defmethod get-records ((table project-stran-table))
  (query (:order-by (:select 'project-stran.project-stran-id 'project-stran.title
                             'from-state-id 'to-state-id
                             (:as 'temtx.title 'temtx)
                     :from 'project-stran
                     :left-join 'temtx
                     :on (:= 'temtx-id 'temtx.temtx-id))
                    'project-stran.title)
         :plists))


;;; rows

(defclass project-stran-row (scrooge-row)
  ())

(defmethod selector ((row project-stran-row) enabled-p)
  (let* ((project-stran-id (key row)))
    (html ()
      (:a :href (if enabled-p
                    (config/project-stran)
                    (config/project-stran :project-stran-id project-stran-id))
        (selector-img enabled-p)))))

(defmethod payload ((row project-stran-row) enabled-p)
  (let ((record (record row))
        (disabled (not enabled-p)))
    (list (make-instance 'textbox
                         :name 'title
                         :value (getf record :title)
                         :disabled disabled)
          (make-instance 'dropdown
                         :name 'from-state-id
                         :value-label-alist *project-states*
                         :selected (getf record :from-state-id)
                         :disabled disabled)
          (make-instance 'dropdown
                         :name 'to-state-id
                         :value-label-alist *project-states*
                         :selected (getf record :to-state-id)
                         :disabled disabled)
          (make-instance 'textbox
                         :name 'temtx
                         :value (getf record :temtx)
                         :css-class "ac-temtx"
                         :disabled disabled))))

(defmethod controls ((row project-stran-row) enabled-p)
  (let ((project-stran-id (key row)))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (config/project-stran :project-stran-id project-stran-id)))
        (list nil nil))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage project-stran-page config/project-stran ("config/project-stran/")
    ((start integer)
     (project-stran-id    integer chk-project-stran-id))
  (with-view-page
    (let ((project-stran-table (make-instance 'project-stran-table
                                              :op :catalogue
                                              :id "project-stran-table")))
      (with-document ()
        (:head
          (:title "Μεταπτώσεις Έργων » Κατάλογος")
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (config-navbar 'project-stran)
            (:div :class "grid_12"
              (:div :id "project-stran-window" :class "window"
                (:div :class "title" "Κατάλογος")
                (project-stran-actions :catalogue (val project-stran-id))
                (display project-stran-table
                         :key (val project-stran-id))))
            (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage project-stran-page config/project-stran/create
    ("config/project-stran/create")
    ((title         string chk-project-stran-title/create)
     (from-state-id string chk-project-state-id)
     (to-state-id   string chk-project-state-id)
     (temtx         string chk-temtx-title))
  (check-project-stran-parameters from-state-id to-state-id)
  (with-view-page
    (let* ((op :create)
           (project-stran-table (make-instance 'project-stran-table :op op)))
      (with-document ()
        (:head
          (:title "Μεταπτώσεις Έργων » Δημιουργία")
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (config-navbar 'project-stran)
            (:div :class "grid_12"
              (:div :class "window"
                (:div :class "title" "Δημιουργία")
                (project-stran-actions op nil)
                (notifications)
                (with-form (actions/config/project-stran/create)
                  (display project-stran-table :payload (params->payload)))))))))))

(defpage project-stran-page actions/config/project-stran/create
    ("actions/config/project-stran/create" :request-type :post)
    ((title         string chk-project-stran-title/create)
     (from-state-id string chk-project-state-id)
     (to-state-id   string chk-project-state-id)
     (temtx         string chk-temtx-title))
  (check-project-stran-parameters from-state-id to-state-id)
  (with-controller-page (config/project-stran/create)
    (let* ((temtx-id (temtx-id (val temtx)))
           (new-project-stran (make-instance 'project-stran
                                             :title (val title)
                                             :from-state-id (val from-state-id)
                                             :to-state-id (val to-state-id)
                                             :temtx-id temtx-id)))
      (insert-dao new-project-stran)
      (see-other (config/project-stran :project-stran-id (project-stran-id new-project-stran))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage project-stran-page config/project-stran/update
    ("config/project-stran/update")
    ((project-stran-id            integer chk-project-stran-id                      t)
     (title         string  (chk-project-stran-title/update title project-stran-id))
     (from-state-id string  chk-project-state-id)
     (to-state-id   string  chk-project-state-id)
     (temtx         string  chk-temtx-title))
  (check-project-stran-parameters from-state-id to-state-id)
  (with-view-page
    (let* ((op :update)
           (project-stran-table (make-instance 'project-stran-table :op op)))
      (with-document ()
        (:head
          (:title "Μεταπτώσεις Έργων » Επεξεργασία")
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (config-navbar 'project-stran)
            (:div :class "grid_12"
              (:div :id "project-stran-window" :class "window"
                (:div :class "title" "Επεξεργασία")
                (project-stran-actions op (val project-stran-id))
                (notifications)
                (with-form (actions/config/project-stran/update :project-stran-id (val project-stran-id))
                  (display project-stran-table :key (val project-stran-id)
                                               :payload (params->payload)))))
            (footer)))))))

(defpage project-stran-page actions/config/project-stran/update
    ("actions/config/project-stran/update" :request-type :post)
    ((project-stran-id            integer chk-project-stran-id                      t)
     (title         string  (chk-project-stran-title/update title project-stran-id))
     (from-state-id string  chk-project-state-id)
     (to-state-id   string  chk-project-state-id)
     (temtx         string  chk-temtx-title))
  (check-project-stran-parameters from-state-id to-state-id)
  (with-controller-page (config/project-stran/update)
    (let ((temtx-id (temtx-id (val temtx))))
      (execute (:update 'project-stran :set
                        'title (val title)
                        'from-state-id (val from-state-id)
                        'to-state-id (val to-state-id)
                        'temtx-id temtx-id
                        :where (:= 'project-stran-id (val project-stran-id)))))
    (see-other (config/project-stran :project-stran-id (val project-stran-id)))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage project-stran-page config/project-stran/delete
    ("config/project-stran/delete")
    ((project-stran-id integer chk-project-stran-id t))
  (with-view-page
    (let* ((op :delete)
           (project-stran-table (make-instance 'project-stran-table :op op)))
      (with-document ()
        (:head
          (:title "Μεταπτώσεις Έργων » Διαγραφή")
          (config-headers))
        (:body
          (:div :id "container" :class "container_12"
            (header 'config)
            (config-navbar 'project-stran)
            (:div :class "grid_12"
              (:div :id "project-stran-window" :class "window"
                (:div :class "title" "Μεταπτώσεις Έργων » Διαγραφή")
                (project-stran-actions op (val project-stran-id))
                (with-form (actions/config/project-stran/delete :project-stran-id (val project-stran-id))
                  (display project-stran-table
                           :key (val project-stran-id)))))
            (footer)))))))

(defpage project-stran-page actions/config/project-stran/delete
    ("actions/config/project-stran/delete" :request-type :post)
    ((project-stran-id integer chk-project-stran-id t))
  (with-controller-page (config/project-stran/delete)
    (delete-dao (get-dao 'project-stran (val project-stran-id)))
    (see-other (config/project-stran))))
