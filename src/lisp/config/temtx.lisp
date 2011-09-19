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
    :initform '(description debit-acc-id credit-acc-id))
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
        "Αυτή η περιγραφή Πρότυπης Συναλλαγής έχει ήδη οριστεί."))
      (from-state
       (:cheque-event-from/to/payable-exists
        "Έχει ήδη οριστεί καταστατική μεταβολή για αυτή την αρχική και τελική κατάσταση"
        :cheque-event-from-to-equal nil))
      (to-state
       (:cheque-event-from-to-equal
        "Η τελική κατάσταση δεν μπορεί να είναι ίδια με την αρχική κατάσταση."))))))



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
  (if (or (eql :null title)
          (temtx-title-exists-p title))
      nil
      :tof-title-unknown))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun temtx-menu (id &optional disabled)
  (anchor-menu (crud-actions-spec (config/temtx :id id)
                                  (config/temtx/create)
                                  (config/temtx/update :id id)
                                  (config/temtx/delete :id id))
               :id "temtx-actions"
               :css-class "hmenu actions"
               :disabled disabled))



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
  (with-db ()
    (query (:order-by (:select 'temtx.id 'temtx.title
                               (:as 'debit-account.title 'debit-account)
                               (:as 'credit-account.title 'credit-account)
                               :from 'temtx
                               :inner-join (:as 'account 'debit-account)
                               :on (:= 'debit-acc-id 'debit-account.id)
                               :inner-join (:as 'account 'credit-account)
                               :on (:= 'credit-acc-id 'credit-account.id))
                      'temtx.title)
           :plists)))


;;; rows

(defclass temtx-row (scrooge-row/plist)
  ())

(defmethod selector ((row temtx-row) selected-p)
  (simple-selector row selected-p #'config/temtx))

(defmethod payload ((row temtx-row) enabled-p)
  (let ((record (record row))
        (disabled (not enabled-p)))
    (list (make-instance 'textbox
                         :title 'title
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

(defpage temtx-page config/temtx ("config/temtx/")
    ((id integer chk-temtx-id))
  (with-view-page
    (let ((title "Πρότυπες Συναλλαγές » Κατάλογος")
          (temtx-table (make-instance 'temtx-table
                                      :op :read
                                      :id "temtx-table")))
      (with-document ()
        (:head
         (:title (str title))
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'temtx)
               (:div :id "temtx-window" :class "window grid_10"
                     (:div :class "title" (str title))
                     (temtx-menu (val id)
                                 (if (val id)
                                     '(:read)
                                     '(:read :update :delete)))
                     (display temtx-table
                              :key (val id)))
               (footer)))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------

(defpage temtx-page config/temtx/create ("config/temtx/create")
    ((title          string chk-temtx-title/create)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title))
  (with-view-page
    (let ((title "Πρότυπες Συναλλαγές » Δημιουργία")
          (temtx-table (make-instance 'temtx-table
                                      :op :create)))
      (with-document ()
        (:head
         (:title (str title))
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'temtx)
               (:div :class "window grid_12"
                     (:div :class "title" (str title))
                     (temtx-menu nil
                                 '(:create :update :delete))
                     (notifications)
                     (with-form (actions/config/temtx/create)
                       (display temtx-table :payload (params->payload))))))))))

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
     (title          string chk-temtx-title/update)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title))
  (with-view-page
    (let ((title "Πρότυπες Συναλλαγές » Επεξεργασία")
          (temtx-table (make-instance 'temtx-table
                                      :op :update)))
      (with-document ()
        (:head
         (:title (str title))
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'temtx)
               (:div :id "temtx-window" :class "window grid_12"
                     (:div :class "title" (str title))
                     (temtx-menu (val id)
                                 '(:create :update))
                     (notifications)
                     (with-form (actions/config/temtx/update :id (val id))
                       (display temtx-table :key (val id)
                                            :payload (params->payload))))
               (footer)))))))

(defpage temtx-page actions/config/temtx/update ("actions/config/temtx/update" :request-type :post)
    ((id             integer chk-temtx-id t)
     (title          string  chk-temtx-title/update)
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
    (see-other (config/temtx))))



;;; ----------------------------------------------------------------------
;;; DELETE
;;; ----------------------------------------------------------------------

(defpage temtx-page config/temtx/delete ("config/temtx/delete")
    ((id integer chk-temtx-id t))
  (with-view-page
    (let ((title "Πρότυπες Συναλλαγές » Διαγραφή")
          (temtx-table (make-instance 'temtx-table
                                      :op :delete)))
      (with-document ()
        (:head
         (:title (str title))
         (config-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header 'config)
               (config-navbar 'temtx)
               (:div :id "temtx-window" :class "window grid_10"
                     (:div :class "title" (str title))
                     (temtx-menu (val id)
                                 '(:create :delete))
                     (with-form (actions/config/temtx/delete :id (val id))
                       (display temtx-table :key (val id))))
               (footer)))))))

(defpage temtx-page actions/config/temtx/delete ("actions/config/temtx/delete" :request-type :post)
    ((id integer chk-temtx-id t))
  (with-controller-page (config/temtx/delete)
    (delete-dao (get-dao 'temtx (val id)))
    (see-other (config/temtx))))
