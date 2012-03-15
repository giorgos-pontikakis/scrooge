(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Page family
;;; ------------------------------------------------------------

(defclass company-cheque-page (cheque-page page-family-mixin)
  ((system-parameter-names
    :allocation :class
    :initform '(id cheque-id start))
   (allowed-groups
    :allocation :class
    :initform '("user" "admin"))))



;;; ------------------------------------------------------------
;;; Company cheque table
;;; ------------------------------------------------------------

(defclass company-cheque-table (cheque-table)
  ((paginator :accessor paginator :initarg :paginator)
   (header-labels :initform '("" "Σειριακός<br />Αριθμός" "<br />Τράπεζα"
                             "Ημερομηνία<br />λήξης" "<br />Ποσό")))
  (:default-initargs :item-class 'company-cheque-row
                     :paginator (make-instance 'company-cheque-paginator
                                               :id "cheque-paginator"
                                               :css-class "paginator")))

(defclass company-cheque-row (cheque-row)
  ())

(defmethod key ((row company-cheque-row))
  (getf (record row) :id))

(defclass company-cheque-paginator (scrooge-paginator)
  ())

(defmethod target-url ((pg company-cheque-paginator) start)
  (let ((table (table pg)))
    (apply #'company/details/cheque (kind table) :start start (filter table))))

(defmethod selector ((row company-cheque-row) selected-p)
  (let* ((table (collection row))
         (cheque-id (key row))
         (company-id (getf (filter table) :company-id))
         (page-filter (remove-from-plist (filter table) :company-id))
         (pg (paginator table))
         (kind (kind table))
         (start (start-index table)))
    (html ()
      (:a :href (if selected-p
                    (apply #'company/details/cheque kind :id company-id
                           :start (page-start pg (index row) start)
                           page-filter)
                    (apply #'company/details/cheque kind :id company-id
                                                         :cheque-id cheque-id
                                                         page-filter))
          (selector-img selected-p)))))

(defmethod payload ((row company-cheque-row) enabled-p)
  (let ((record (record row)))
    (list (make-instance 'textbox
                         :name 'serial
                         :value (getf record :serial)
                         :disabled (not enabled-p))
          (make-instance 'textbox
                         :name 'bank
                         :value (getf record :bank)
                         :css-class "ac-bank"
                         :disabled (not enabled-p))
          (make-instance 'textbox
                         :name 'due-date
                         :value (getf record :due-date)
                         :css-class (if enabled-p "datepicker" nil)
                         :disabled (not enabled-p))
          (make-instance 'textbox
                         :name 'amount
                         :value (getf record :amount)
                         :disabled (not enabled-p)))))



;;; ------------------------------------------------------------
;;; UI elements
;;; ------------------------------------------------------------

(defun company-cheque-top-actions (op kind id company-filter cheque-filter filter)
  (top-actions (make-instance 'menu
                              :spec `((catalogue ,(company-catalogue-link id
                                                                          company-filter))
                                      (company-create ,(company-create-link company-filter))
                                      (cheque-create ,(create-cheque-link #'company/details/cheque
                                                                          kind cheque-filter))
                                      (print
                                       ,(html ()
                                          (:a :href (apply #'company/details/cheque/print
                                                           kind :id id filter)
                                              (:img :src "/scrooge/img/printer.png")
                                              "Εκτύπωση"))))
                              :css-class "hmenu"
                              :disabled (company-disabled-actions op))
               (searchbox #'actions/company/search
                          #'(lambda (&rest args)
                              (apply #'company :id id args))
                          company-filter
                          "ac-company")))

(defun company-cheque-actions (op kind id cheque-id filter)
  (actions-menu
   (make-menu-spec
    (action-anchors/crud (apply #'company/details/cheque kind :id id :cheque-id cheque-id filter)
                         (apply #'company/details/cheque kind :id id :cheque-id cheque-id filter)))
   (enabled-actions/crud op cheque-id)))



;;; ------------------------------------------------------------
;;; VIEW
;;; ------------------------------------------------------------

(defpage company-cheque-page company/details/cheque
    (("company/details/cheque/" (kind "(receivable|payable)")))
    ((id        integer chk-company-id t)
     (cheque-id integer chk-cheque-id)
     (search    string)
     (subset    string)
     (since     date)
     (until     date)
     (start     integer)
     (cstate    string))
  (with-view-page
    (check-cheque-accounts)
    (let* ((op :catalogue)
           (filter (params->filter))
           (cheque-filter (params->cheque-filter))
           (company-filter (params->company-filter))
           (cheque-table (make-instance 'company-cheque-table
                                        :op :details
                                        :filter (list* :company-id (val id) cheque-filter)
                                        :start-index (val start)
                                        :kind kind)))
      (with-document ()
        (:head
         (:title "Εταιρία » Λεπτομέρειες » Επιταγές")
         (main-headers))
        (:body
         (:div :id "container" :class "container_12"
               (header)
               (main-navbar 'company)
               (company-cheque-top-actions :tx kind (val id)
                                           company-filter cheque-filter filter)
               (company-tabs (val id) company-filter 'cheque
                             (html ()
                               (:div :class "secondary-filter-area"
                                     (display (cheque-filters kind
                                                              (list* :id (val id) filter)
                                                              #'company/details/cheque)))
                               (:div :id "company-tx-window"
                                     (:div :class "window"
                                           (:div :class "title" "Προς είσπραξη")
                                           (company-cheque-actions op kind
                                                                   (val id) (val cheque-id)
                                                                   filter)
                                           (display cheque-table
                                                    :key (val cheque-id))))))
               (footer)))))))

(defpage company-cheque-page company/details/cheque/print (("company/details/cheque/"
                                                            (kind "(receivable|payable)")
                                                             "/print"))
    ((search    string)
     (subset    string)
     (id        integer chk-company-id t)
     (cheque-id integer)
     (since     date)
     (until     date)
     (start     integer)
     (cstate    string))
  (with-view-page
    (let* ((filter (params->filter))
           (payable-table (make-instance 'company-cheque-table
                                         :op :details
                                         :filter filter
                                         :start-index (val start)
                                         :kind "payable"))
           (receivable-table (make-instance 'company-cheque-table
                                            :op :details
                                            :filter filter
                                            :start-index (val start)
                                            :kind "receivable")))
      (with-document ()
        (:head
         (:title "Εταιρία » Λεπτομέρειες » Επιταγές » Εκτύπωση")
         (print-headers))
        (:body
         (:div :id "container" :class "container_12"
               (:div :class "grid_12"
                     (:a :id "back"
                         :href (apply #'company/details/cheque filter)
                         "« Επιστροφή"))
               (:div :id "company-tx-window"
                     (when (records payable-table)
                       (htm (:div :class "window"
                                  (:div :class "title" "Προς είσπραξη")
                                  (display payable-table
                                           :key (val cheque-id)))))
                     (when (records receivable-table)
                       (htm (:div :class "window"
                                  (:div :class "title" "Προς πληρωμή")
                                  (display receivable-table
                                           :key (val cheque-id))))))))))))



;;; ----------------------------------------------------------------------
;;; CREATE
;;; ----------------------------------------------------------------------
