(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass cheque-stran-family (family-mixin)
  ()
  (:default-initargs :parameter-groups '(:system (cheque-stran-id)
                                         :payload (title from-state-id to-state-id temtx)
                                         :filter ())))

(defclass cheque-stran-page (auth-regex-page cheque-stran-family)
  ((messages
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

(defun cheque-stran-top-actions (&optional op)
  (top-actions
   (make-instance 'menu
                  :spec (make-menu-spec
                         `(:create ("Νέα μετάπτωση" . ,(gurl 'config/cheque-stran/create))))
                  :css-class "hmenu"
                  :disabled (list op))
   nil))


;;; ----------------------------------------------------------------------
;;; Checks
;;; ----------------------------------------------------------------------

(define-existence-predicate cheque-state-id-exists-p cheque-state id)

(define-existence-predicate cheque-stran-exists-p cheque-stran id)
(define-existence-predicate* cheque-stran-title-exists-p cheque-stran title id)

(defun chk-cheque-stran-id (cheque-stran-id)
  (if (cheque-stran-exists-p cheque-stran-id)
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

(defun chk-cheque-stran-title/update (title cheque-stran-id)
  (cond ((eql :null title) :cheque-stran-title-null)
        ((cheque-stran-title-exists-p title cheque-stran-id) :cheque-stran-title-exists)
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

(defun cheque-stran-from/to/payable-exists-p/update (from-state-id to-state-id kind cheque-stran-id)
  (if (or (null from-state-id) (null to-state-id))
      nil
      (with-db ()
        (query (:select 1 :from 'cheque-stran
                :where (:and (:= 'from-state-id from-state-id)
                             (:= 'to-state-id to-state-id)
                             (:= 'payable-p (string= kind "payable"))
                             (:not (:= 'id cheque-stran-id))))
               :plists))))

(defun chk-cheque-stran-from/to/payable-exists/create (from-state-id to-state-id kind)
  (if (cheque-stran-from/to/payable-exists-p/create from-state-id to-state-id kind)
      :cheque-stran-from/to/payable-exists
      nil))

(defun chk-cheque-stran-from/to/payable-exists/update (from-state-id to-state-id kind cheque-stran-id)
  (if (cheque-stran-from/to/payable-exists-p/update from-state-id to-state-id kind cheque-stran-id)
      :cheque-stran-from/to/payable-exists
      nil))

(defun check-cheque-stran-parameters (from-state-id to-state-id kind &optional cheque-stran-id)
  (if cheque-stran-id
      (validate-parameters (lambda (from to)
                             (chk-cheque-stran-from/to/payable-exists/update from
                                                                             to
                                                                             kind
                                                                             (val cheque-stran-id)))
                           from-state-id to-state-id)
      (validate-parameters (lambda (from to)
                             (chk-cheque-stran-from/to/payable-exists/create from
                                                                             to
                                                                             kind))
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

(defmethod actions ((table cheque-stran-table) &key)
  (let* ((cheque-stran-id (selected-key table))
         (hrefs (if cheque-stran-id
                    (list :update (config/cheque-stran/update (kind table)
                                                              :cheque-stran-id cheque-stran-id)
                          :delete (config/cheque-stran/delete (kind table)
                                                              :cheque-stran-id cheque-stran-id))
                    nil)))
    (actions-menu (make-menu-spec hrefs)
                  (disabled-actions table))))

(defmethod filters ((tbl cheque-stran-table))
  (filter-area (filter-navbar `((receivable ,(config/cheque-stran "receivable") "Προς είσπραξη")
                                (payable    ,(config/cheque-stran "payable")    "Προς πληρωμή"))
                              :active (intern (string-upcase (kind tbl))))))


;;; rows

(defclass cheque-stran-row (scrooge-row/plist)
  ())

(defmethod selector ((row cheque-stran-row) enabled-p)
  (let* ((cheque-stran-id (key row))
         (table (collection row))
         (kind (kind table)))
    (html ()
      (:a :href (if enabled-p
                    (config/cheque-stran kind)
                    (config/cheque-stran kind :cheque-stran-id cheque-stran-id))
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
  (let ((cheque-stran-id (key row))
        (kind (kind (collection row))))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (config/cheque-stran kind :cheque-stran-id cheque-stran-id)))
        (list nil nil))))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage cheque-stran-page config/cheque-stran
    (("config/cheque-stran/" (kind "(receivable|payable)")))
    ((start           integer)
     (cheque-stran-id integer chk-cheque-stran-id))
  (with-view-page
    (let ((cheque-stran-table (make-instance 'cheque-stran-table
                                             :op :catalogue
                                             :selected-key (val cheque-stran-id)
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
               (cheque-stran-top-actions)
               (filters cheque-stran-table)
               (:div :class "grid_12"
                     (:div :id "cheque-stran-window" :class "window"
                           (:div :class "title" "Κατάλογος")
                           (actions cheque-stran-table)
                           (display cheque-stran-table)))
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
    (let ((cheque-stran-table (make-instance 'cheque-stran-table
                                             :kind kind
                                             :op :create)))
      (with-document ()
        (:head
         (:title "Μεταπτώσεις Επιταγών » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque-stran)
               (cheque-stran-top-actions :create)
               (filters cheque-stran-table)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" "Δημιουργία")
                           (actions cheque-stran-table)
                           (notifications)
                           (with-form (actions/config/cheque-stran/create kind)
                             (display cheque-stran-table :payload (params->values :payload)))))))))))

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
      (see-other (config/cheque-stran kind :cheque-stran-id (cheque-stran-id new-cheque-stran))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage cheque-stran-page config/cheque-stran/update
    (("config/cheque-stran/" (kind "(receivable|payable)") "/update"))
    ((cheque-stran-id integer chk-cheque-stran-id                                   t)
     (title           string  (chk-cheque-stran-title/update title cheque-stran-id))
     (from-state-id   string  chk-cheque-state-id*)
     (to-state-id     string  chk-cheque-state-id)
     (temtx           string  chk-temtx-title))
  (check-cheque-stran-parameters from-state-id to-state-id kind cheque-stran-id)
  (with-view-page
    (let ((cheque-stran-table (make-instance 'cheque-stran-table
                                             :kind kind
                                             :selected-key (val cheque-stran-id)
                                             :op :update)))
      (with-document ()
        (:head
         (:title "Μεταπτώσεις Επιταγών » Επεξεργασία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque-stran)
               (cheque-stran-top-actions)
               (filters cheque-stran-table)
               (:div :class "grid_12"
                     (:div :id "cheque-stran-window" :class "window"
                           (:div :class "title" "Επεξεργασία")
                           (actions cheque-stran-table)
                           (notifications)
                           (with-form
                               (actions/config/cheque-stran/update kind
                                                                   :cheque-stran-id (val cheque-stran-id))
                             (display cheque-stran-table :payload (params->values :payload)))))
               (footer)))))))

(defpage cheque-stran-page actions/config/cheque-stran/update
    (("actions/config/cheque-stran/" (kind "(receivable|payable)") "/update")
     :request-type :post)
    ((cheque-stran-id            integer chk-cheque-stran-id                      t)
     (title         string  (chk-cheque-stran-title/update title cheque-stran-id))
     (from-state-id string  chk-cheque-state-id*)
     (to-state-id   string  chk-cheque-state-id)
     (temtx         string  chk-temtx-title))
  (check-cheque-stran-parameters from-state-id to-state-id kind cheque-stran-id)
  (with-controller-page (config/cheque-stran/update kind)
    (let ((temtx-id (temtx-id (val temtx))))
      (execute (:update 'cheque-stran :set
                        'title (val title)
                        'from-state-id (val from-state-id)
                        'to-state-id (val to-state-id)
                        'temtx-id temtx-id
                        :where (:= 'id (val cheque-stran-id)))))
    (see-other (config/cheque-stran kind :cheque-stran-id (val cheque-stran-id)))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage cheque-stran-page config/cheque-stran/delete
    (("config/cheque-stran/" (kind "(receivable|payable)") "/delete"))
    ((cheque-stran-id integer chk-cheque-stran-id t))
  (with-view-page
    (let ((cheque-stran-table (make-instance 'cheque-stran-table
                                             :kind kind
                                             :selected-key (val cheque-stran-id)
                                             :op :delete)))
      (with-document ()
        (:head
         (:title "Μεταπτώσεις Επιταγών » Διαγραφή")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'cheque-stran)
               (cheque-stran-top-actions)
               (filters cheque-stran-table)
               (:div :class "grid_12"
                     (:div :id "cheque-stran-window" :class "window"
                           (:div :class "title" "Διαγραφή")
                           (actions cheque-stran-table)
                           (with-form
                               (actions/config/cheque-stran/delete kind
                                                                   :cheque-stran-id (val cheque-stran-id))
                             (display cheque-stran-table))))
               (footer)))))))

(defpage cheque-stran-page actions/config/cheque-stran/delete
    (("actions/config/cheque-stran/" (kind "(receivable|payable)") "/delete")
     :request-type :post)
    ((cheque-stran-id integer chk-cheque-stran-id t))
  (with-controller-page (config/cheque-stran/delete kind)
    (delete-dao (get-dao 'cheque-stran (val cheque-stran-id)))
    (see-other (config/cheque-stran kind))))
