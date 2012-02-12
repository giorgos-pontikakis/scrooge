(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass cheque-stran-page (regex-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id))
   (payload-parameter-names
    :allocation :class
    :initform '(title from-state-id to-state-id temtx))
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
       (:cheque-stran-title-null
        "Η περιγραφή του συμβάντος είναι κενή."
        :cheque-stran-title-exists
        "Αυτή η περιγραφή συμβάντος έχει ήδη οριστεί."))
      (from-state-id
       (:cheque-stran-from/to/payable-exists
        "Έχει ήδη οριστεί συμβάν για αυτή την αρχική και τελική κατάσταση"
        :cheque-stran-from-to-equal
        "Η τελική κατάσταση δεν μπορεί να είναι ίδια με την αρχική κατάσταση."))
      (temtx
       (:temtx-title-null
        "Η περιγραφή της Πρότυπης Συναλλαγής είναι κενή."
        :temtx-title-unknown
        "Δεν έχει οριστεί Πρότυπη Συναλλαγή με αυτή την περιγραφή."))))))



;;; ----------------------------------------------------------------------
;;; Checks
;;; ----------------------------------------------------------------------

(define-existence-predicate cheque-state-id-exists-p cheque-state id)

(define-existence-predicate cheque-stran-exists-p cheque-stran id)
(define-existence-predicate* cheque-stran-title-exists-p cheque-stran title id)

(defun chk-cheque-stran-id (id)
  (if (cheque-stran-exists-p id)
      nil
      :cheque-stran-id-unknown))

(defun chk-cheque-stran-title (title)
  (if (eql title :null)
      :cheque-stran-title-null
      nil))

(defun chk-cheque-stran-title/create (title)
  (cond ((eql :null title) :cheque-stran-title-null)
        ((cheque-stran-title-exists-p title) :cheque-stran-title-exists)
        (t nil)))

(defun chk-cheque-stran-title/update (title id)
  (cond ((eql :null title) :cheque-stran-title-null)
        ((cheque-stran-title-exists-p title id) :cheque-stran-title-exists)
        (t nil)))

(defun chk-cheque-state-id (state-id)
  (if (cheque-state-id-exists-p state-id)
      nil
      :cheque-state-id-invalid))

(defun chk-cheque-state-id* (state-id)
  "Same with chk-cheque-state but accepts null values"
  (if (or (eql state-id :null)
          (cheque-state-id-exists-p state-id))
      nil
      :cheque-state-id-invalid))


;;; post checks

(defun cheque-stran-from/to/payable-exists-p/create (from-state-id to-state-id kind)
  (if (or (null from-state-id) (null to-state-id))
      nil
      (with-db ()
        (query (:select 1 :from 'cheque-stran
                :where (:and (:= 'from-state-id from-state-id)
                             (:= 'to-state-id to-state-id)
                             (:= 'payable-p (string= kind "payable"))))
               :plists))))

(defun cheque-stran-from/to/payable-exists-p/update (from-state-id to-state-id kind id)
  (if (or (null from-state-id) (null to-state-id))
      nil
      (with-db ()
        (query (:select 1 :from 'cheque-stran
                :where (:and (:= 'from-state-id from-state-id)
                             (:= 'to-state-id to-state-id)
                             (:= 'payable-p (string= kind "payable"))
                             (:not (:= 'id id))))
               :plists))))

(defun chk-cheque-stran-from/to/payable-exists/create (from-state-id to-state-id kind)
  (if (cheque-stran-from/to/payable-exists-p/create from-state-id to-state-id kind)
      :cheque-stran-from/to/payable-exists
      nil))

(defun chk-cheque-stran-from/to/payable-exists/update (from-state-id to-state-id kind id)
  (if (cheque-stran-from/to/payable-exists-p/update from-state-id to-state-id kind id)
      :cheque-stran-from/to/payable-exists
      nil))

(defun check-cheque-stran-parameters (from-state-id to-state-id kind &optional id)
  (if id
      (validate-parameters (lambda (from to)
                             (chk-cheque-stran-from/to/payable-exists/update from to kind (val id)))
                           from-state-id to-state-id)
      (validate-parameters (lambda (from to)
                             (chk-cheque-stran-from/to/payable-exists/create from to kind))
                           from-state-id to-state-id))
  (validate-parameters (lambda (from to)
                         (if (string= from to)
                             :cheque-stran-from-to-equal
                             nil))
                       from-state-id to-state-id)
  nil)



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun cheque-stran-top-actions (kind)
  (top-actions (make-instance 'menu
                              :spec `((create
                                       ,(html ()
                                          (:a :href (config/cheque-stran/create kind)
                                              (:img :src "/scrooge/img/add.png")
                                              "Νέα μετάπτωση"))))
                              :css-class "hmenu")
               nil))

(defun cheque-stran-actions (op kind id)
  (actions-menu (make-menu-spec (action-anchors/crud (config/cheque-stran/update kind :id id)
                                                     (config/cheque-stran/delete kind :id id)))
                (enabled-actions/crud op id)))

(defun cheque-stran-filters (kind)
  (filter-area (filter-navbar `((receivable ,(config/cheque-stran "receivable") "Προς είσπραξη")
                                (payable    ,(config/cheque-stran "payable")    "Προς πληρωμή"))
                              :active (intern (string-upcase kind)))))



;;; ------------------------------------------------------------
;;; Cheque Stran Table
;;; ------------------------------------------------------------

;;; table

(defclass cheque-stran-table (scrooge-table)
  ((kind :accessor kind :initarg :kind)
   (header-labels :initform '("" "<br />Περιγραφή"
                              "Αρχική<br />Κατάσταση" "Τελική<br />Κατάσταση"
                               "Πρότυπη<br />Συναλλαγή" "" ""))
   (paginator     :initform nil))
  (:default-initargs :item-class 'cheque-stran-row))

(defmethod get-records ((table cheque-stran-table))
  (query (:order-by (:select 'cheque-stran.id 'cheque-stran.title
                             'from-state-id 'to-state-id
                             (:as 'temtx.title 'temtx)
                             :from 'cheque-stran
                             :inner-join 'temtx
                             :on (:= 'temtx-id 'temtx.id)
                             :where (:= 'payable-p (string= (kind table) "payable")))
                    'cheque-stran.title)
         :plists))


;;; rows

(defclass cheque-stran-row (scrooge-row/plist)
  ())

(defmethod selector ((row cheque-stran-row) enabled-p)
  (let* ((id (key row))
         (table (collection row))
         (kind (kind table)))
    (html ()
      (:a :href (if enabled-p
                    (config/cheque-stran kind)
                    (config/cheque-stran kind :id id))
          (selector-img enabled-p)))))

(defmethod payload ((row cheque-stran-row) enabled-p)
  (let ((record (record row))
        (disabled (not enabled-p)))
    (list (make-instance 'textbox
                         :name 'title
                         :value (getf record :title)
                         :disabled disabled)
          (make-instance 'dropdown
                         :name 'from-state-id
                         :value-label-alist *cheque-states*
                         :selected (getf record :from-state-id)
                         :disabled disabled)
          (make-instance 'dropdown
                         :name 'to-state-id
                         :value-label-alist *cheque-states*
                         :selected (getf record :to-state-id)
                         :disabled disabled)
          (make-instance 'textbox
                         :name 'temtx
                         :value (getf record :temtx)
                         :css-class "ac-temtx"
                         :disabled disabled))))

(defmethod controls ((row cheque-stran-row) enabled-p)
  (let ((id (key row))
        (kind (kind (collection row))))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (config/cheque-stran kind :id id)))
        (list nil nil))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage cheque-stran-page config/cheque-stran
    (("config/cheque-stran/" (kind "(receivable|payable)")))
    ((start integer)
     (id    integer chk-cheque-stran-id))
  (with-view-page
    (let ((op :catalogue)
          (cheque-stran-table (make-instance 'cheque-stran-table
                                             :op :catalogue
                                             :id "cheque-stran-table"
                                             :kind kind)))
      (with-document ()
        (:head
         (:title "Μεταπτώσεις Επιταγών » Κατάλογος")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque-stran)
               (cheque-stran-top-actions kind)
               (cheque-stran-filters kind)
               (:div :class "grid_12"
                     (:div :id "cheque-stran-window" :class "window"
                           (:div :class "title" "Κατάλογος")
                           (cheque-stran-actions op kind (val id))
                           (display cheque-stran-table
                                    :key (val id))))
               (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage cheque-stran-page config/cheque-stran/create
    (("config/cheque-stran/" (kind "(receivable|payable)") "/create"))
    ((title         string chk-cheque-stran-title/create)
     (from-state-id string chk-cheque-state-id*)
     (to-state-id   string chk-cheque-state-id)
     (temtx         string chk-temtx-title))
  (check-cheque-stran-parameters from-state-id to-state-id kind)
  (with-view-page
    (let* ((op :create)
           (cheque-stran-table (make-instance 'cheque-stran-table
                                              :kind kind
                                              :op op)))
      (with-document ()
        (:head
         (:title "Μεταπτώσεις Επιταγών » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque-stran)
               (cheque-stran-top-actions kind)
               (cheque-stran-filters kind)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" "Δημιουργία")
                           (cheque-stran-actions op kind nil)
                           (notifications)
                           (with-form (actions/config/cheque-stran/create kind)
                             (display cheque-stran-table :payload (params->payload)))))))))))

(defpage cheque-stran-page actions/config/cheque-stran/create
    (("actions/config/cheque-stran/" (kind "(receivable|payable)") "/create") :request-type :post)
    ((title         string chk-cheque-stran-title/create)
     (from-state-id string chk-cheque-state-id*)
     (to-state-id   string chk-cheque-state-id)
     (temtx         string chk-temtx-title))
  (check-cheque-stran-parameters from-state-id to-state-id kind)
  (with-controller-page (config/cheque-stran/create kind)
    (let* ((temtx-id (temtx-id (val temtx)))
           (new-cheque-stran (make-instance 'cheque-stran
                                            :title (val title)
                                            :payable-p (string= kind "payable")
                                            :from-state-id (val from-state-id)
                                            :to-state-id (val to-state-id)
                                            :temtx-id temtx-id)))
      (insert-dao new-cheque-stran)
      (see-other (config/cheque-stran kind :id (cheque-stran-id new-cheque-stran))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage cheque-stran-page config/cheque-stran/update
    (("config/cheque-stran/" (kind "(receivable|payable)") "/update"))
    ((id            integer chk-cheque-stran-id                      t)
     (title         string  (chk-cheque-stran-title/update title id))
     (from-state-id string  chk-cheque-state-id*)
     (to-state-id   string  chk-cheque-state-id)
     (temtx         string  chk-temtx-title))
  (check-cheque-stran-parameters from-state-id to-state-id kind id)
  (with-view-page
    (let* ((op :update)
           (cheque-stran-table (make-instance 'cheque-stran-table
                                              :kind kind
                                              :op op)))
      (with-document ()
        (:head
         (:title "Μεταπτώσεις Επιταγών » Επεξεργασία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque-stran)
               (cheque-stran-top-actions kind)
               (cheque-stran-filters kind)
               (:div :class "grid_12"
                     (:div :id "cheque-stran-window" :class "window"
                           (:div :class "title" "Επεξεργασία")
                           (cheque-stran-actions op kind (val id))
                           (notifications)
                           (with-form (actions/config/cheque-stran/update kind :id (val id))
                             (display cheque-stran-table :key (val id)
                                                         :payload (params->payload)))))
               (footer)))))))

(defpage cheque-stran-page actions/config/cheque-stran/update
    (("actions/config/cheque-stran/" (kind "(receivable|payable)") "/update")
     :request-type :post)
    ((id            integer chk-cheque-stran-id                      t)
     (title         string  (chk-cheque-stran-title/update title id))
     (from-state-id string  chk-cheque-state-id*)
     (to-state-id   string  chk-cheque-state-id)
     (temtx         string  chk-temtx-title))
  (check-cheque-stran-parameters from-state-id to-state-id kind id)
  (with-controller-page (config/cheque-stran/update kind)
    (let ((temtx-id (temtx-id (val temtx))))
      (execute (:update 'cheque-stran :set
                        'title (val title)
                        'from-state-id (val from-state-id)
                        'to-state-id (val to-state-id)
                        'temtx-id temtx-id
                        :where (:= 'id (val id)))))
    (see-other (config/cheque-stran kind :id (val id)))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage cheque-stran-page config/cheque-stran/delete
    (("config/cheque-stran/" (kind "(receivable|payable)") "/delete"))
    ((id integer chk-cheque-stran-id t))
  (with-view-page
    (let* ((op :delete)
           (cheque-stran-table (make-instance 'cheque-stran-table
                                              :kind kind
                                              :op op)))
      (with-document ()
        (:head
         (:title "Μεταπτώσεις Επιταγών » Διαγραφή")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque-stran)
               (cheque-stran-top-actions kind)
               (cheque-stran-filters kind)
               (:div :class "grid_12"
                     (:div :id "cheque-stran-window" :class "window"
                           (:div :class "title" "Διαγραφή")
                           (cheque-stran-actions op kind (val id))
                           (with-form (actions/config/cheque-stran/delete kind
                                                                          :id (val id))
                             (display cheque-stran-table
                                      :key (val id)))))
               (footer)))))))

(defpage cheque-stran-page actions/config/cheque-stran/delete
    (("actions/config/cheque-stran/" (kind "(receivable|payable)") "/delete")
     :request-type :post)
    ((id integer chk-cheque-stran-id t))
  (with-controller-page (config/cheque-stran/delete kind)
    (delete-dao (get-dao 'cheque-stran (val id)))
    (see-other (config/cheque-stran kind))))
