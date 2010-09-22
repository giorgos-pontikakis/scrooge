(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; Definition
;;; ------------------------------------------------------------

;; (defclass stran-table (table-normal-crud)
;;   ((name :initform "stran-table")
;;    (header :initform '(:selector    ""
;;                        :tbl         "Πίνακας"
;;                        :description "Περιγραφή"         
;;                        :old-status  "Αρχική Κατάσταση"  
;;                        :new-status  "Τελική Κατάσταση"
;;                        :debit-acc   "Λογ. Χρέωσης"      
;;                        :credit-acc  "Λογ. Πίστωσης"     
;;                        :submit      ""
;;                        :cancel      ""))
;;    (styles :initform '(:active-row "active"
;;                        :inactive-row ""
;;                        :attention-row "attention"
;;                        :table "forms-in-row table-half")) 
;;    ;; page interface
;;    (id-keys :initform '(:stran-id :tbl))
;;    (payload-keys :initform '(:description
;;                              :old-status
;;                              :new-status
;;                              :debit-acc
;;                              :credit-acc))
;;    (filter-keys :initform '())
;;    ;; crud mixin
;;    (main-page :initform 'stran)
;;    (submit-pages :initform '(:create actions/stran/create
;;                              :update actions/stran/update
;;                              :delete actions/stran/delete)) 
;;    (cells-fn :initform (stran-cells-fn))
;;    (data-fn :initform (stran-data-fn))))



;; (defun make-stran-table (&key operation params)
;;   (make-instance 'stran-table 
;;                  :operation operation
;;                  :params params))

;; (defun stran-cells-fn ()
;;   (lambda (row) 
;;     (destructuring-bind (&key stran-id tbl description
;;                               old-status new-status debit-acc credit-acc) (data row)
;;       (declare (ignore stran-id))
;;       (let ((pairs (with-db (query (:select 'description 'id :from 'stran)))))
;;         (list (make-cell-selector :row row
;;                                   :name :selector
;;                                   :style "select")
;;               (make-cell-dropdown :row row
;;                                   :name :tbl 
;;                                   :pairs pairs
;;                                   :value tbl
;;                                   :style "data"
;;                                   :operations '(:create))
;;               (make-cell-textbox :row row
;;                                  :name :description
;;                                  :value description
;;                                  :style "data"
;;                                  :operations '(:create :update))
;;               (make-cell-textbox :row row
;;                                  :name :old-status
;;                                  :value old-status
;;                                  :style "data"
;;                                  :operations '(:create :update))
;;               (make-cell-textbox :row row
;;                                  :name :new-status
;;                                  :value new-status
;;                                  :style "data"
;;                                  :operations '(:create :update))
;;               (make-cell-textbox :row row
;;                                  :name :debit-acc
;;                                  :value debit-acc
;;                                  :style "data"
;;                                  :operations '(:create :update))
;;               (make-cell-textbox :row row
;;                                  :name :credit-acc
;;                                  :value credit-acc
;;                                  :style "data"
;;                                  :operations '(:create :update))
;;               (make-cell-submit :row row
;;                                 :name :submit
;;                                 :style "button"
;;                                 :operations '(:create :update :delete))
;;               (make-cell-cancel :row row
;;                                 :name :cancel
;;                                 :style "button"
;;                                 :operations '(:create :update :delete)))))))



;;; ------------------------------------------------------------
;;; State transitions - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/stran/create ((tbl string)
                                           (description string #'not-db-null-p)
                                           (debit-acc string #'acc-exists-p)
                                           (credit-acc string #'acc-exists-p) 
                                           (old-status string)
                                           (new-status string))
    ("actions/stran/create" :request-type :post
                            :validators ((old-status (valid-combo tbl old-status))
                                         (new-status (valid-combo tbl new-status))))
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

(define-dynamic-page actions/stran/update ((id integer)
                                           (tbl string)
                                           (description string #'not-db-null-p)
                                           (debit-acc string #'acc-exists-p)
                                           (credit-acc string #'acc-exists-p) 
                                           (old-status string)
                                           (new-status string))
    ("actions/stran/update"
     :request-type :post
     :validators ((old-status (valid-combo tbl old-status))
		  (new-status (valid-combo tbl new-status))
		  ((id tbl) (valid-stran-id-p id tbl))))
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

(define-dynamic-page actions/stran/delete ((id integer)
                                           (tbl string))
    ("actions/stran/delete" :request-type :post
                            :validators (((id tbl) (valid-stran-id-p id tbl))))
  (no-cache) 
  (if (every #'validp (parameters *page*))
      (with-db ()
        (delete-dao (get-dao (symbolicate (string-upcase tbl) "-STRAN") id))
        (see-other (stran)))
      (see-other (notfound))))



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


(defun stran-data ()
  (flet ((select (table)
           `(:select (:as ,(symbolicate table '-stran.id) 'stran-id) ;; :stran-id
                     (:as 'stran.id 'tbl) ;; :tbl
                     ,(symbolicate table '-stran.description) ;; :description
                     'old-status   ;; :old-status
                     'new-status   ;; :new-status 
                     (:as 'debit-account.title 'debit-acc) ;; :debit-acc
                     (:as 'credit-account.title 'credit-acc) ;; :credit-acc

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
         (row-payload-fn (mkfn-row-payload op payload-keys)) 
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

(define-dynamic-page stran ((id integer)
                            (tbl string))
    ("config/stran/" :validators (((id tbl) (valid-stran-id-p id tbl))))
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

(define-dynamic-page stran/create ((tbl         string #'valid-tbl-p)
                                   (description string #'not-db-null-p) 
                                   (debit-acc   string #'acc-exists-p)
                                   (credit-acc  string #'acc-exists-p)
                                   (old-status  string)
                                   (new-status  string))
    ("config/stran/create" :validators ((old-status (valid-combo tbl old-status))
                                        (new-status (valid-combo tbl new-status))))
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

(define-dynamic-page stran/update ((id          integer)
                                   (tbl         string #'valid-tbl-p)
                                   (description string #'not-db-null-p)
                                   (debit-acc   string #'acc-exists-p) 
                                   (credit-acc  string #'acc-exists-p) 
                                   (old-status  string)
                                   (new-status  string))
    ("config/stran/update" :validators ((old-status (valid-combo tbl old-status))
                                        (new-status (valid-combo tbl new-status))
                                        ((id tbl) (valid-stran-id-p id tbl))))
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

(define-dynamic-page stran/delete ((id integer)
                                   (tbl string #'valid-tbl-p))
    ("config/stran/delete" :validators (((id tbl) (valid-stran-id-p id tbl))))
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



