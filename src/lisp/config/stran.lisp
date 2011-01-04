(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))


;;; ------------------------------------------------------------
;;; stran - Validation
;;; ------------------------------------------------------------

(defun stran-id-exists-p (id tbl)
  (with-db ()
    (query (:select 1
                    :from (symbolicate (string-upcase tbl) "-STRAN")
                    :where (:= 'id id)))))

(defun stran-tbl-exists-p (tbl)
  (with-db ()
    (query (:select 1
                    :from 'stran
                    :where (:= tbl 'id)))))

(defun stran-description-exists-p (description &optional id)
  (with-db ()
    (if id
        (query (:select 1
                        :from 'stran
                        :where (:and (:not (:= 'id id))
                                     (:= 'description description)))
               :single)
        (query (:select 1
                        :from 'stran
                        :where (:= 'description description))
               :single))))

(defun valid-status-p (status table)
  (with-db ()
    (let* ((status-table (symbolicate (string-upcase table) "-STATUS"))
           (sql (sql-compile `(:select 1
                                       :from ,status-table
                                       :where (:= status ,status)))))
      (and (stran-tbl-exists-p table)
           (query sql :single)))))

(defun chk-stran-id (id tbl)
  (if (stran-id-exists-p id tbl)
      nil
      'stran-id-unknown))

(defun chk-stran-tbl (tbl)
  (if (stran-tbl-exists-p tbl)
      nil
      'stran-tbl-unknown))

(defun chk-stran-description (description &optional id)
  (cond ((eql :null description) 'stran-description-null)
        ((stran-description-exists-p description id) 'stran-description-exists)
        (t nil)))

(defun chk-status (status tbl)
  (if (valid-status-p status tbl)
      nil
      'stran-status-unknown))



;;; ------------------------------------------------------------
;;; State transitions - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/stran/create ("actions/stran/create" :request-type :post)
    ((tbl         string chk-stran-tbl)
     (description string chk-stran-description)
     (debit-acc   string acc-exists-p)
     (credit-acc  string acc-exists-p)
     (old-status  string (chk-status old-status tbl))
     (new-status  string (chk-status new-status tbl)))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (let ((debit-acc-id (account-id (val debit-acc)))
            (credit-acc-id (account-id (val credit-acc))))
        (with-db ()
          (insert-dao (make-instance (symbolicate (string-upcase tbl) "-STRAN")
                                     :description (val description)
                                     :debit-acc-id debit-acc-id
                                     :credit-acc-id credit-acc-id
                                     :old-status (val old-status)
                                     :new-status (val new-status)))
          (see-other (stran))))
      (see-other (stran/create :tbl tbl
                               :description (raw description)
                               :debit-acc (raw debit-acc)
                               :credit-acc (raw credit-acc)
                               :old-status (raw old-status)
                               :new-status (raw new-status)))))

(define-dynamic-page actions/stran/update ("actions/stran/update" :request-type :post)
    ((id          integer chk-stran-id)
     (tbl         string  chk-stran-tbl)
     (description string  chk-stran-description)
     (debit-acc   string  acc-exists-p)
     (credit-acc  string  acc-exists-p)
     (old-status  string  (chk-status old-status tbl))
     (new-status  string  (chk-status new-status tbl)))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (let ((debit-acc-id (account-id (val debit-acc)))
            (credit-acc-id (account-id (val credit-acc))))
        (with-db ()
          (execute (:update (symbolicate (val tbl) "-STRAN") :set
                            'description (val description)
                            'debit-acc-id debit-acc-id
                            'credit-acc-id credit-acc-id
                            'old-status (val old-status)
                            'new-status (val new-status)
                            :where (:= 'id (val id))))
          (see-other (stran))))
      (see-other (stran/update :id id
                               :description description
                               :debit-acc debit-acc
                               :credit-acc credit-acc
                               :tbl tbl
                               :old-status old-status
                               :new-status new-status))))

(define-dynamic-page actions/stran/delete ("actions/stran/delete" :request-type :post)
    ((id integer chk-stran-id)
     (tbl string (valid-stran-id-p id tbl)))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-db ()
        (delete-dao (get-dao (symbolicate (string-upcase tbl) "-STRAN") id))
        (see-other (stran)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; stran menu
;;; ------------------------------------------------------------

(defun stran-menu (id filter &optional disabled-items)
  (display (make-instance 'actions-menu
                          :id "stran-actions"
                          :style "hnavbar actions grid_9 alpha"
                          :spec (standard-actions-spec (stran :id id
                                                              :filter filter)
                                                       (stran/create :filter filter)
                                                       (stran/update :id id
                                                                     :filter filter)
                                                       (if (null id)
                                                           nil
                                                           (stran/delete :id id
                                                                         :filter filter))))
           :disabled-items disabled-items))



;;; ------------------------------------------------------------
;;; stran table
;;; ------------------------------------------------------------

;;; table

(defclass stran-table (crud-table)
  ((header-labels :initform '("" "Ονομασία τράπεζας" "" ""))
   (paginator     :initform nil)))

(defun stran-data ()
  (flet ((select (table)
           `(:select (:as ,(symbolicate table '-stran.id) 'stran-id)
                     (:as 'stran.id 'tbl)
                     ,(symbolicate table '-stran.description)
                     'old-status
                     'new-status
                     (:as 'debit-account.title 'debit-acc)
                     (:as 'credit-account.title 'credit-acc)

                     :from ,(symbolicate table '-stran)

                     :left-join 'stran
                     :on (:= 'stran.id ,table)

                     :left-join (:as 'account 'debit-account)
                     :on (:= 'debit-account.id
                             ,(symbolicate table '-stran.debit-acc-id))

                     :left-join (:as 'account 'credit-account)
                     :on (:= 'credit-account.id
                             ,(symbolicate table '-stran.credit-acc-id)))))
    (with-db ()
      (query (sql-compile `(:union ,(select "project")
                                   ,(select "cheque")))
             :plists))))

(defmethod read-items ((table stran-table))
  (iter (for rec in (config-data 'bank (filter table)))
        (for i from 0)
        (collect (make-instance 'stran-row
                                :key (getf rec :id)
                                :record rec
                                :collection table
                                :index i))))




;;; ------------------------------------------------------------
;;; State transitions - Snippets
;;; ------------------------------------------------------------

(defun stran-menu (id tbl enabled-items)
  (funcall (actions-menu)
           :item-specs (standard-actions-spec (stran :id id :tbl tbl)
                                              (stran/create)
                                              (stran/update :id id :tbl tbl)
                                              (stran/delete :id id :tbl tbl))
           :enabled-items enabled-items))

(defun stran-errorbar (params)
  (funcall (generic-errorbar)
           params
           '((description "Η περιγραφή δεν πρέπει να είναι κενή")
             (debit-acc "Άκυρος λογαριασμός χρέωσης")
             (credit-acc "Άκυρος λογαριασμός πίστωσης")
             (old-status "Άκυρη αρχική κατάσταση")
             (new-status "Άκυρη τελική κατάσταση"))))





;;; ------------------------------------------------------------
;;; State transitions - Table
;;; ------------------------------------------------------------

(defun mkfn-stran-selector-states ()
  (lambda (id tbl)
    `((t   ,(stran))
      (nil ,(apply #'stran id tbl)))))

(defun stran-table (op id tbl)
  (let* ((id-keys '(:id :tbl))
         (payload-keys '(:description
                         :old-status
                         :new-status
                         :debit-acc
                         :credit-acc))
         (db-table (stran-data))
         (cancel-url (stran :id (val* id) :tbl (val* tbl)))
         (row-selected-p-fn (mkfn-row-selected-p id-keys))
         (selector-states-fn (mkfn-bank-selector-states))
         ;; op-specific
         (row-controls-p-fn (mkfn-crud-row-controls-p op))
         (row-readonly-p-fn (mkfn-crud-row-readonly-p op))
         ;; id, payload and the row itself
         (row-id-fn (mkfn-row-id id-keys))
         (row-payload-fn (mkfn-row-payload payload-keys))
         (row (mkfn-crud-row row-id-fn
                             row-payload-fn
                             row-selected-p-fn
                             row-controls-p-fn
                             row-readonly-p-fn
                             selector-states-fn
                             cancel-url)))
    (html ()
      (:table :class "table-half forms-in-row"
              (thead ""
                     "Πίνακας" "Περιγραφή"
                     "Αρχική Κατάσταση" "Τελική Κατάσταση"
                     "Λογ. Χρέωσης" "Λογ. Πίστωσης"
                     "" "")
              (:tbody
               (when (eql op 'create)
                 (funcall row nil))
               (iter (for db-row in db-table)
                     (funcall row db-row)))))))



;;; ------------------------------------------------------------
;;; Pages
;;; ------------------------------------------------------------

(define-dynamic-page stran ("config/stran/")
    ((id integer)
     (tbl string (valid-stran-id-p id tbl)))
  (no-cache)
  (if (every #'validp (parameters *page*))
      (with-document ()
        (:head
         (:title "Καταστατικές Μεταβολές")
         (head-config))
        (:body
         (config-header 'stran)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Κατάλογος Καταστατικών Μεταβολών"))
               (:div :id "stran" :class "window"
                     (stran-menu (val id) (val tbl) (if (val id)
                                                        '(create update delete)
                                                        '(create)))
                     (render (stran-table 'view id tbl)))
               (footer))))
      (see-other (notfound))))

(define-dynamic-page stran/create ("config/stran/create" )
    ((tbl         string #'chk-tbl-p)
     (description string #'not-db-null-p)
     (debit-acc   string #'acc-exists-p)
     (credit-acc  string #'acc-exists-p)
     (old-status  string (chk-status old-status tbl))
     (new-status  string (chk-status new-status tbl)))
  (no-cache)
  (if (validp tbl)
      (with-document ()
        (:head
         (:title "Καταστατικές Μεταβολές: Δημιουργία")
         (head-config))
        (:body
         (config-header 'stran)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Δημιουργία μετάβασης")
                     (stran-errorbar (list description debit-acc credit-acc old-status new-status)))
               (:div :id "stran" :class "window"
                     (stran-menu nil tbl '(view))
                     (with-form (actions/stran/create :tbl tbl)
                       (stran-table 'create nil tbl)))
               (footer))))
      (see-other (notfound))))

(define-dynamic-page stran/update ("config/stran/update")
    ((id          integer (valid-stran-id-p id tbl))
     (tbl         string #'chk-tbl-p)
     (description string #'not-db-null-p)
     (debit-acc   string #'acc-exists-p)
     (credit-acc  string #'acc-exists-p)
     (old-status  string (chk-status old-status tbl))
     (new-status  string (chk-status new-status tbl)))
  (no-cache)
  (if (validp id)
      (with-document ()
        (:head
         (:title "Καταστατικές Μεταβολές: Επεξεργασία")
         (head-config))
        (:body
         (config-header 'stran)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Επεξεργασία μετάβασης")
                     (stran-errorbar (list description debit-acc credit-acc old-status new-status)))
               (:div :id "stran" :class "window"
                     (stran-menu (val id) (val tbl) '(view delete))
                     (with-form (actions/stran/update :id id :tbl tbl)
                       (stran-table 'update id tbl)))
               (footer))))
      (see-other (notfound))))

(define-dynamic-page stran/delete ("config/stran/delete" )
    ((id integer)
     (tbl string #'chk-tbl-p (valid-stran-id-p id tbl)))
  (no-cache)
  (if (validp id)
      (with-document ()
        (:head
         (:title "Καταστατικές Μεταβολές: Διαγραφή")
         (head-config))
        (:body
         (config-header 'stran)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Διαγραφή μετάβασης"))
               (:div :id "stran" :class "window"
                     (stran-menu (val id) (val tbl) '(view update))
                     (with-form (actions/stran/delete :id id)
                       (stran-table 'delete id tbl)))
               (footer))))
      (see-other (notfound))))



;; ;;; ------------------------------------------------------------
;; ;;; Definition
;; ;;; ------------------------------------------------------------

;; ;; (defclass stran-table (table-normal-crud)
;; ;;   ((name :initform "stran-table")
;; ;;    (header :initform '(:selector    ""
;; ;;                        :tbl         "Πίνακας"
;; ;;                        :description "Περιγραφή"
;; ;;                        :old-status  "Αρχική Κατάσταση"
;; ;;                        :new-status  "Τελική Κατάσταση"
;; ;;                        :debit-acc   "Λογ. Χρέωσης"
;; ;;                        :credit-acc  "Λογ. Πίστωσης"
;; ;;                        :submit      ""
;; ;;                        :cancel      ""))
;; ;;    (styles :initform '(:active-row "active"
;; ;;                        :inactive-row ""
;; ;;                        :attention-row "attention"
;; ;;                        :table "forms-in-row table-half"))
;; ;;    ;; page interface
;; ;;    (id-keys :initform '(:stran-id :tbl))
;; ;;    (payload-keys :initform '(:description
;; ;;                              :old-status
;; ;;                              :new-status
;; ;;                              :debit-acc
;; ;;                              :credit-acc))
;; ;;    (filter-keys :initform '())
;; ;;    ;; crud mixin
;; ;;    (main-page :initform 'stran)
;; ;;    (submit-pages :initform '(:create actions/stran/create
;; ;;                              :update actions/stran/update
;; ;;                              :delete actions/stran/delete))
;; ;;    (cells-fn :initform (stran-cells-fn))
;; ;;    (data-fn :initform (stran-data-fn))))



;; ;; (defun make-stran-table (&key operation params)
;; ;;   (make-instance 'stran-table
;; ;;                  :operation operation
;; ;;                  :params params))

;; ;; (defun stran-cells-fn ()
;; ;;   (lambda (row)
;; ;;     (destructuring-bind (&key stran-id tbl description
;; ;;                               old-status new-status debit-acc credit-acc) (data row)
;; ;;       (declare (ignore stran-id))
;; ;;       (let ((pairs (with-db (query (:select 'description 'id :from 'stran)))))
;; ;;         (list (make-cell-selector :row row
;; ;;                                   :name :selector
;; ;;                                   :style "select")
;; ;;               (make-cell-dropdown :row row
;; ;;                                   :name :tbl
;; ;;                                   :pairs pairs
;; ;;                                   :value tbl
;; ;;                                   :style "data"
;; ;;                                   :operations '(:create))
;; ;;               (make-cell-textbox :row row
;; ;;                                  :name :description
;; ;;                                  :value description
;; ;;                                  :style "data"
;; ;;                                  :operations '(:create :update))
;; ;;               (make-cell-textbox :row row
;; ;;                                  :name :old-status
;; ;;                                  :value old-status
;; ;;                                  :style "data"
;; ;;                                  :operations '(:create :update))
;; ;;               (make-cell-textbox :row row
;; ;;                                  :name :new-status
;; ;;                                  :value new-status
;; ;;                                  :style "data"
;; ;;                                  :operations '(:create :update))
;; ;;               (make-cell-textbox :row row
;; ;;                                  :name :debit-acc
;; ;;                                  :value debit-acc
;; ;;                                  :style "data"
;; ;;                                  :operations '(:create :update))
;; ;;               (make-cell-textbox :row row
;; ;;                                  :name :credit-acc
;; ;;                                  :value credit-acc
;; ;;                                  :style "data"
;; ;;                                  :operations '(:create :update))
;; ;;               (make-cell-submit :row row
;; ;;                                 :name :submit
;; ;;                                 :style "button"
;; ;;                                 :operations '(:create :update :delete))
;; ;;               (make-cell-cancel :row row
;; ;;                                 :name :cancel
;; ;;                                 :style "button"
;; ;;                                 :operations '(:create :update :delete)))))))
