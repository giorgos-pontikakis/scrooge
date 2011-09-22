(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass project-stran-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id))
   (payload-parameter-names
    :allocation :class
    :initform '(title from-state to-state temtx))
   (filter-parameter-names
    :allocation :class
    :initform '())
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))
   (messages
    :allocation :class
    :reader messages
    :initform
    '((title
       (:project-stran-title-null
        "Η περιγραφή του συμβάντος είναι κενή."
        :project-stran-title-exists
        "Αυτή η περιγραφή συμβάντος έχει ήδη οριστεί."))
      (from-state
       (:project-stran-from/to-exists
        "Έχει ήδη οριστεί συμβάν για αυτή την αρχική και τελική κατάσταση"
        :project-stran-from-to-equal
        "Η τελική κατάσταση δεν μπορεί να είναι ίδια με την αρχική κατάσταση."))
      (tem-tx
       (:temtx-title-null
        "Η περιγραφή της Πρότυπης Συναλλαγής είναι κενή."
        :temtx-title-unknown
        "Δεν έχει οριστεί Πρότυπη Συναλλαγή με αυτή την περιγραφή."))))))



;;; ----------------------------------------------------------------------
;;; Checks
;;; ----------------------------------------------------------------------

(define-existence-predicate project-state-exists-p project-state id)

(define-existence-predicate project-stran-exists-p project-stran id)
(define-existence-predicate* project-stran-title-exists-p project-stran title id)

(defun chk-project-stran-id (id)
  (if (project-stran-exists-p id)
      nil
      :project-stran-id-unknown))

(defun chk-project-stran-title (title)
  (if (eql title :null)
      :project-stran-title-null
      nil))

(defun chk-project-stran-title/create (title)
  (cond ((eql :null title) :project-stran-title-null)
        ((project-stran-title-exists-p title) :project-stran-title-exists)
        (t nil)))

(defun chk-project-stran-title/update (title id)
  (cond ((eql :null title) :project-stran-title-null)
        ((project-stran-title-exists-p title id) :project-stran-title-exists)
        (t nil)))

(defun chk-project-state (state)
  (if (project-state-exists-p state)
      nil
      :project-state-invalid))


;;; post checks

(defun project-stran-from/to-exists-p (from-state to-state)
  (if (or (null from-state) (null to-state))
      nil
      (with-db ()
        (query (:select 1 :from 'project-stran
                :where (:and (:= 'from-state from-state)
                             (:= 'to-state to-state)))
               :plists))))

(defun chk-project-stran-from/to (from-state to-state)
  (if (project-stran-from/to-exists-p from-state to-state)
      :project-stran-from/to-exists
      nil))

(defun check-project-stran-parameters (from-state to-state)
  (validate-parameters (lambda (from to)
                         (chk-project-stran-from/to from to))
                       from-state to-state)
  (validate-parameters (lambda (from to)
                         (if (string= from to)
                             :project-stran-from-to-equal
                             nil))
                       from-state to-state)
  nil)



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun project-stran-menu (id &optional disabled)
  (anchor-menu (crud-actions-spec (config/project-stran :id id)
                                  (config/project-stran/create)
                                  (config/project-stran/update :id id)
                                  (config/project-stran/delete :id id))
               :id "project-stran-actions"
               :css-class "hmenu actions"
               :disabled disabled))



;;; ------------------------------------------------------------
;;; Project Stran Table
;;; ------------------------------------------------------------

;;; table

(defclass project-stran-table (scrooge-table)
  ((header-labels  :initform '("" "<br />Περιγραφή"
                               "Αρχική<br />Κατάσταση" "Τελική<br />Κατάσταση"
                               "Λογαριασμός<br />Χρέωσης" "Λογαριασμός<br />Πίστωσης"))
   (paginator      :initform nil))
  (:default-initargs :item-class 'project-stran-row))

(defmethod get-records ((table project-stran-table))
  (with-db ()
    (query (:order-by (:select 'project-stran.id 'project-stran.title
                               'from-state 'to-state
                               (:as 'temtx.title 'temtx)
                       :from 'project-stran
                       :inner-join 'temtx
                       :on (:= 'temtx-id 'temtx.id))
                      'project-stran.title)
           :plists)))


;;; rows

(defclass project-stran-row (scrooge-row/plist)
  ())

(defmethod selector ((row project-stran-row) enabled-p)
  (let* ((id (key row)))
    (html ()
      (:a :href (if enabled-p
                    (config/project-stran)
                    (config/project-stran :id id))
          (selector-img enabled-p)))))

(defmethod payload ((row project-stran-row) enabled-p)
  (let ((record (record row))
        (disabled (not enabled-p)))
    (list (make-instance 'textbox
                         :name 'title
                         :value (getf record :title)
                         :disabled disabled)
          (make-instance 'dropdown
                         :name 'from-state
                         :value-label-alist *project-states*
                         :selected (getf record :from-state)
                         :disabled disabled)
          (make-instance 'dropdown
                         :name 'to-state
                         :value-label-alist *project-states*
                         :selected (getf record :to-state)
                         :disabled disabled)
          (make-instance 'textbox
                         :name 'temtx
                         :value (getf record :temtx)
                         :css-class "ac-temtx"
                         :disabled disabled))))

(defmethod controls ((row project-stran-row) enabled-p)
  (let ((id (key row)))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (config/project-stran :id id)))
        (list nil nil))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage project-stran-page config/project-stran ("config/project-stran/")
    ((start integer)
     (id    integer chk-project-stran-id))
  (with-view-page
    (let ((title "Μεταπτώσεις Έργων » Κατάλογος")
          (project-stran-table (make-instance 'project-stran-table
                                              :op :read
                                              :id "project-stran-table")))
      (with-document ()
        (:head
         (:title (str title))
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'project-stran)
               (:div :id "project-stran-window" :class "window grid_12"
                     (:div :class "title" (str title))
                     (project-stran-menu (val id)
                                         (if (val id)
                                             '(:read)
                                             '(:read :update :delete)))
                     (display project-stran-table
                              :key (val id)))
               (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage project-stran-page config/project-stran/create
    ("config/project-stran/create")
    ((title      string chk-project-stran-title/create)
     (from-state string chk-project-state)
     (to-state   string chk-project-state)
     (temtx      string chk-temtx-title))
  (with-view-page
    (check-project-stran-parameters from-state to-state)
    (let ((project-stran-table (make-instance 'project-stran-table :op :create)))
      (with-document ()
        (:head
         (:title "Μεταπτώσεις Έργων » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'project)
               (:div :class "window grid_12"
                     (:div :class "title" "Μεταπτώσεις Έργων » Δημιουργία")
                     (project-stran-menu nil
                                         '(:create :update :delete))
                     (notifications)
                     (with-form (actions/config/project-stran/create)
                       (display project-stran-table :payload (params->payload))))))))))

(defpage project-stran-page actions/config/project-stran/create
    ("actions/config/project-stran/create" :request-type :post)
    ((title          string  chk-project-stran-title/create)
     (from-state     string  chk-project-state)
     (to-state       string  chk-project-state)
     (temtx          string  chk-temtx-title))
  (with-controller-page (config/project-stran/create)
    (check-project-stran-parameters from-state to-state)
    (let* ((temtx-id (temtx-id (val temtx)))
           (new-project-stran (make-instance 'project-stran
                                             :title (val title)
                                             :from-state (val from-state)
                                             :to-state (val to-state)
                                             :temtx-id temtx-id)))
      (insert-dao new-project-stran)
      (see-other (config/project-stran :id (project-stran-id new-project-stran))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage project-stran-page config/project-stran/update
    ("config/project-stran/update")
    ((id             integer chk-project-stran-id                      t)
     (title          string  (chk-project-stran-title/update title id))
     (from-state     string  chk-project-state)
     (to-state       string  chk-project-state)
     (temtx          string  chk-temtx-title))
  (with-view-page
    (check-project-stran-parameters from-state to-state)
    (let ((title "Μεταπτώσεις Έργων » Επεξεργασία")
          (project-stran-table (make-instance 'project-stran-table :op :update)))
      (with-document ()
        (:head
         (:title (str title))
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'project-stran)
               (:div :id "project-stran-window" :class "window grid_12"
                     (:div :class "title" (str title))
                     (project-stran-menu (val id)
                                         '(:create :update))
                     (notifications)
                     (with-form (actions/config/project-stran/update :id (val id))
                       (display project-stran-table :key (val id)
                                                    :payload (params->payload))))
               (footer)))))))

(defpage project-stran-page actions/config/project-stran/update
    ("actions/config/project-stran/update" :request-type :post)
    ((id             integer chk-project-stran-id                      t)
     (title          string  (chk-project-stran-title/update title id))
     (from-state     string  chk-project-state)
     (to-state       string  chk-project-state)
     (temtx          string  chk-temtx-title))
  (with-controller-page (config/project-stran/update)
    (check-project-stran-parameters from-state to-state)
    (let ((temtx-id (temtx-id (val temtx))))
      (execute (:update 'project-stran :set
                        'title (val title)
                        'from-state (val from-state)
                        'to-state (val to-state)
                        'temtx-id temtx-id
                        :where (:= 'id (val id)))))
    (see-other (config/project-stran :id (val id)))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage project-stran-page config/project-stran/delete
    ("config/project-stran/delete")
    ((id integer chk-project-stran-id t))
  (with-view-page
    (let ((project-stran-table (make-instance 'project-stran-table :op :delete)))
      (with-document ()
        (:head
         (:title "Μεταπτώσεις Έργων » Διαγραφή")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'project-stran)
               (:div :id "project-stran-window" :class "window grid_10"
                     (:div :class "title" "Μεταπτώσεις Έργων » Διαγραφή")
                     (project-stran-menu (val id)
                                         '(:create :delete))
                     (with-form (actions/config/project-stran/delete :id (val id))
                       (display project-stran-table
                                :key (val id))))
               (footer)))))))

(defpage project-stran-page actions/config/project-stran/delete
    ("actions/config/project-stran/delete" :request-type :post)
    ((id integer chk-project-stran-id t))
  (with-controller-page (config/project-stran/delete)
    (delete-dao (get-dao 'project-stran (val id)))
    (see-other (config/project-stran))))
