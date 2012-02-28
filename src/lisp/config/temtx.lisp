(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Page family
;;; ----------------------------------------------------------------------

(defclass temtx-page (dynamic-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id))
   (payload-parameter-names
    :allocation :class
    :initform '(title debit-account credit-account))
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
       (:temtx-title-null
        "Η περιγραφή της Πρότυπης Συναλλαγής είναι κενή."
        :temtx-title-exists
        "Έχει ήδη οριστεί Πρότυπη Συναλλαγή με αυτή την περιγραφή."))
      (debit-account
       (:account-title-null
        "Άκυρος λογαριασμός χρέωσης: Το όνομα είναι κενό."
        :account-title-unknown
        "Άκυρος λογαριασμός χρέωσης: Δεν υπάρχει λογαριασμός με αυτό το όνομα."))
      (credit-account
       (:account-title-null
        "Άκυρος λογαριασμός πίστωσης: Το όνομα είναι κενό."
        :account-title-unknown
        "Άκυρος λογαριασμός πίστωσης: Δεν υπάρχει λογαριασμός με αυτό το όνομα."))))))



;;; ----------------------------------------------------------------------
;;; Validation
;;; ----------------------------------------------------------------------

(define-existence-predicate temtx-exists-p temtx id)
(define-existence-predicate* temtx-title-exists-p temtx title id)

(defun chk-temtx-id (id)
  (if (temtx-exists-p id)
      nil
      :temtx-id-unknown))

(defun chk-temtx-title/create (title)
  (cond ((eql :null title) :temtx-title-null)
        ((temtx-title-exists-p title) :temtx-title-exists)
        (t nil)))

(defun chk-temtx-title/update (title id)
  (cond ((eql :null title) :temtx-title-null)
        ((temtx-title-exists-p title id) :temtx-title-exists)
        (t nil)))

(defun chk-temtx-title (title)
  (cond ((eql title :null)
         :temtx-title-null)
        ((temtx-title-exists-p title)
         nil)
        (t
         :temtx-title-unknown)))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun temtx-top-actions ()
  (top-actions
   (make-instance 'menu
                  :spec `((create ,(html ()
                                     (:a :href (config/temtx/create)
                                         (:img :src "/scrooge/img/add.png")
                                         (str "Νέο Πρότυπο Συναλλαγής")))))
                  :css-class "hmenu")
   nil))

(defun temtx-actions (op id)
  (actions-menu (make-menu-spec
                 (action-anchors/crud (config/temtx/update :id id)
                                      (config/temtx/delete :id id)))
                (enabled-actions/crud op id)))



;;; ------------------------------------------------------------
;;; Template TX Table
;;; ------------------------------------------------------------

;;; table

(defclass temtx-table (scrooge-table)
  ((header-labels :initform '("" "<br />Περιγραφή"
                              "Λογαριασμός<br />Χρέωσης" "Λογαριασμός<br />Πίστωσης"))
   (paginator     :initform (make-instance 'temtx-paginator)))
  (:default-initargs :item-class 'temtx-row))

(defmethod get-records ((table temtx-table))
  (query (:order-by (:select 'temtx.id 'temtx.title
                             (:as 'debit-account.title 'debit-account)
                             (:as 'credit-account.title 'credit-account)
                             :from 'temtx
                             :inner-join (:as 'account 'debit-account)
                             :on (:= 'debit-acc-id 'debit-account.id)
                             :inner-join (:as 'account 'credit-account)
                             :on (:= 'credit-acc-id 'credit-account.id))
                    'temtx.title)
         :plists))


;;; rows

(defclass temtx-row (scrooge-row/plist)
  ())

(defmethod selector ((row temtx-row) selected-p)
  (simple-selector row selected-p #'config/temtx))

(defmethod payload ((row temtx-row) enabled-p)
  (let ((record (record row))
        (disabled (not enabled-p)))
    (list (make-instance 'textbox
                         :name 'title
                         :value (getf record :title)
                         :disabled disabled)
          (make-instance 'textbox
                         :name 'debit-account
                         :value (getf record :debit-account)
                         :css-class "ac-account"
                         :disabled disabled)
          (make-instance 'textbox
                         :name 'credit-account
                         :value (getf record :credit-account)
                         :css-class "ac-account"
                         :disabled disabled))))

(defmethod controls ((row temtx-row) controls-p)
  (simple-controls row controls-p #'config/temtx))


;;; paginator

(defclass temtx-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg temtx-paginator) start)
  (apply #'config/temtx :start start))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage temtx-page config/temtx ("config/temtx")
    ((start integer)
     (id    integer chk-temtx-id))
  (with-view-page
    (let* ((op :catalogue)
           (temtx-table (make-instance 'temtx-table
                                       :op op
                                       :id "temtx-table")))
      (with-document ()
        (:head
         (:title "Πρότυπα Συναλλαγών » Κατάλογος")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'temtx)
               (temtx-top-actions)
               (:div :class "grid_12"
                     (:div :id "temtx-window" :class "window"
                           (:div :class "title" "Κατάλογος")
                           (temtx-actions op (val id))
                           (display temtx-table
                                    :key (val id))))
               (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage temtx-page config/temtx/create ("config/temtx/create")
    ((title          string chk-temtx-title/create)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title))
  (with-view-page
    (let ((op :create)
          (temtx-table (make-instance 'temtx-table
                                      :op :create)))
      (with-document ()
        (:head
         (:title "Πρότυπα Συναλλαγών » Δημιουργία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'temtx)
               (temtx-top-actions)
               (:div :class "grid_12"
                     (:div :class "window"
                           (:div :class "title" "Δημιουργία")
                           (temtx-actions op nil)
                           (notifications)
                           (with-form (actions/config/temtx/create)
                             (display temtx-table :payload (params->payload)))))))))))

(defpage temtx-page actions/config/temtx/create ("actions/config/temtx/create" :request-type :post)
    ((title          string chk-temtx-title/create)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title))
  (with-controller-page (config/temtx/create)
    (let* ((debit-acc-id (account-id (val debit-account)))
           (credit-acc-id (account-id (val credit-account)))
           (new-temtx (make-instance 'temtx
                                     :title (val title)
                                     :debit-acc-id debit-acc-id
                                     :credit-acc-id credit-acc-id)))
      (insert-dao new-temtx)
      (see-other (config/temtx :id (temtx-id new-temtx))))))



;;; ----------------------------------------------------------------------
;;; UPDATE
;;; ----------------------------------------------------------------------

(defpage temtx-page config/temtx/update ("config/temtx/update")
    ((id             integer chk-temtx-id t)
     (title          string (chk-temtx-title/update title id))
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title))
  (with-view-page
    (let* ((op :update)
           (temtx-table (make-instance 'temtx-table
                                       :op op)))
      (with-document ()
        (:head
         (:title "Πρότυπα Συναλλαγών » Επεξεργασία")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'temtx)
               (temtx-top-actions)
               (:div :class "grid_12"
                     (:div :id "temtx-window" :class "window"
                           (:div :class "title" "Επεξεργασία")
                           (temtx-actions op (val id))
                           (notifications)
                           (with-form (actions/config/temtx/update :id (val id))
                             (display temtx-table :key (val id)
                                                  :payload (params->payload)))))
               (footer)))))))

(defpage temtx-page actions/config/temtx/update ("actions/config/temtx/update" :request-type :post)
    ((id             integer chk-temtx-id t)
     (title          string  (chk-temtx-title/update title id))
     (debit-account  string  chk-acc-title)
     (credit-account string  chk-acc-title))
  (with-controller-page (config/temtx/update)
    (let ((debit-acc-id (account-id (val debit-account)))
          (credit-acc-id (account-id (val credit-account))))
      (execute (:update 'temtx :set
                        'title (val title)
                        'debit-acc-id debit-acc-id
                        'credit-acc-id credit-acc-id
                        :where (:= 'id (val id)))))
    (see-other (config/temtx :id (val id)))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage temtx-page config/temtx/delete ("config/temtx/delete")
    ((id integer chk-temtx-id t))
  (with-view-page
    (let* ((op :delete)
           (temtx-table (make-instance 'temtx-table
                                       :op :delete)))
      (with-document ()
        (:head
         (:title "Πρότυπα Συναλλαγών » Διαγραφή")
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'temtx)
               (temtx-top-actions)
               (:div :class "grid_12"
                     (:div :id "temtx-window" :class "window"
                           (:div :class "title" "Διαγραφή")
                           (temtx-actions op (val id))
                           (with-form (actions/config/temtx/delete :id (val id))
                             (display temtx-table :key (val id)))))
               (footer)))))))

(defpage temtx-page actions/config/temtx/delete ("actions/config/temtx/delete" :request-type :post)
    ((id integer chk-temtx-id t))
  (with-controller-page (config/temtx/delete)
    (delete-dao (get-dao 'temtx (val id)))
    (see-other (config/temtx))))
