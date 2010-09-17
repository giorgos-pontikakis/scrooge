(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; Bank - Definitions
;;; ------------------------------------------------------------

;; (defclass bank-table (table-normal-crud)
;;   ((name :initform "bank-table")
;;    (header :initform '(:selector ""
;;                        :title "Τράπεζα"
;;                        :submit ""
;;                        :cancel ""))
;;    (styles :initform '(:active-row "active"
;;                        :inactive-row ""
;;                        :attention-row "attention"
;;                        :table "forms-in-row table-half"
;;                        :header (:selector "select"
;;                                 :title "data"
;;                                 :submit  "button"
;;                                 :cancel "button")))
;;    ;; page interface
;;    (id-keys :initform '(:id))
;;    (payload-keys :initform '(:title))
;;    (filter-keys :initform '())
;;    (aux-keys :initform '())
;;    ;; crud mixin
;;    (main-page :initform 'bank)
;;    (submit-pages :initform '(:create actions/bank/create
;;                              :update actions/bank/update
;;                              :delete actions/bank/delete))
;;    (cells-fn :initform (config-cells-fn))
;;    (data-fn :initform (config-data-fn 'bank))))

;; (defun make-bank-table (&key operation params)
;;   (make-instance 'bank-table
;;                  :operation operation
;;                  :params params))



;;; ------------------------------------------------------------
;;; Bank - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/bank/create ((title string (complement #'bank-exists-p)))
    ("actions/bank/create" :request-type :post)
  (no-cache)
  (let ((params (parameters *page*)))
    (if (every #'validp params)
        (with-db ()
          (insert-dao (make-instance 'bank :title (val title)))
          (see-other (bank :id (bank-id (val title)))))
        (see-other (bank/create :title (raw title))))))

(define-dynamic-page actions/bank/update ((id    integer #'bank-id-exists-p)
                                          (title string  (complement #'bank-exists-p)))
    ("actions/bank/update" :request-type :post)
  (no-cache)
  (let ((params (parameters *page*)))
    (if (every #'validp params)
        (with-db ()
          (execute (:update 'bank :set
                            'title (val title)
                            :where (:= 'id (val id))))
          (see-other (bank :id (val id))))
        (see-other (bank/update :id (raw id) :title (raw title))))))

(define-dynamic-page actions/bank/delete ((id integer #'bank-id-exists-p))
    ("actions/bank/delete" :request-type :post)
  (if (validp id)
      (with-db ()
        (delete-dao (get-dao 'bank (val id)))
        (see-other (bank)))
      (see-other (notfound))))



;;; ------------------------------------------------------------
;;; Bank menus
;;; ------------------------------------------------------------

(defun standard-actions-spec (view create update delete)
  `((:view   ,view   "Προβολή"     "img/magnifier.png")
    (:create ,create "Δημιουργία"  "img/add.png")
    (:update ,update "Επεξεργασία" "img/pencil.png")
    (:delete ,delete "Διαγραφή"    "img/delete.png")))

(defun cheques-exist-p (id)
  (with-db ()
    (and id
         (query (:select 'id
                         :from 'cheque
                         :where (:= 'bank-id id))))))

(defun table-actions-menu ()
  (generic-menu :div-style "actions"
                :ul-style "hmenu"))

(defun bank-menu (id enabled-items)
  (funcall (table-actions-menu)
           :item-specs (standard-actions-spec (bank :id id)
                                              (bank/create)
                                              (bank/update :id id)
                                              (if (or (null id)
                                                      (cheques-exist-p id))
                                                  nil
                                                  (bank/delete :id id)))
           :enabled-items enabled-items))

(defun bank-errorbar (params)
  (funcall (generic-errorbar)
           params
           '((title "Αυτό το όνομα τράπεζας υπάρχει ήδη."))))



;;; ------------------------------------------------------------
;;; Bank table
;;; ------------------------------------------------------------

(defun mkfn-bank-row-controls-p (op)
  (mkfn-row-controls-p op '(:create :update :delete)))

(defun mkfn-bank-row-readonly-p (op)
  (mkfn-row-readonly-p op
                       '(:view :delete)
                       '(:create :update)))

(defun bank-selector-cell (id)
  (selector-cell `((t   ,(bank))
                   (nil ,(apply #'bank id)))))

(defun bank-row (row-id-fn row-payload-fn
                 row-selected-p-fn row-controls-p-fn row-readonly-p-fn
                 cancel-url)
  (html (row-data)
    (let* ((id (funcall row-id-fn row-data))
           (payload (funcall row-payload-fn row-data))
           (row-selected-p (funcall row-selected-p-fn id))
           (row-controls-p (funcall row-controls-p-fn row-selected-p))
           (row-readonly-p (funcall row-readonly-p-fn row-selected-p)))
      (htm (:tr (funcall (bank-selector-cell id) row-selected-p)

                (plist-map (lambda (key value)
                             (if row-readonly-p
                                 (htm (:td (str value)))
                                 (textbox-cell (symbolicate key)
                                               value
                                               nil))) ;; todo -- style missing
                           payload)
                (ok-cell row-controls-p)
                (cancel-cell cancel-url row-controls-p))))))

(defun bank-table (op id)
  (let* ((id-keys '(:id))
         (payload-keys '(:title))
         (db-table (config-data 'bank))
         (cancel-url (bank :id (val* id)))
         (row-selected-p-fn (mkfn-row-selected-p id-keys))
         ;; op-specific
         (row-controls-p-fn (mkfn-bank-row-controls-p op))
         (row-readonly-p-fn (mkfn-bank-row-readonly-p op))
         ;; id, payload and the row itself
         (row-id-fn (mkfn-row-id id-keys))
         (row-payload-fn (mkfn-row-payload op payload-keys))
         (row (bank-row row-id-fn
                        row-payload-fn
                        row-selected-p-fn
                        row-controls-p-fn
                        row-readonly-p-fn
                        cancel-url)))
    (html ()
      (:table :class "forms-in-row table-half"
              (thead "" "Ονομασία Τράπεζας" "" "")
              (:tbody
               (when (eql op :create)
                 (funcall row nil))
               (iter (for db-row in db-table)
                     (funcall row db-row)))))))



;;; ------------------------------------------------------------
;;; Bank - Pages
;;; ------------------------------------------------------------

(defun config-header (config-choice)
  (with-html
    (:div :id "header"
          (logo)
          (primary-navbar 'config)
          (config-navbar config-choice))))

(define-dynamic-page bank ((id integer #'bank-id-exists-p))
    ("config/bank")
  (no-cache)
  (if (validp id) 
      (with-document ()
        (:head
         (:title "Τράπεζες")
         (config-headers))
        (:body
         (config-header 'banks)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Κατάλογος τραπεζών"))
               (:div :id "bank" :class "window"
                     (bank-menu (val id) (if (val id)
                                             '(:create :update :delete)
                                             '(:create)))
                     (render (bank-table :view id)))
               (footer))))
      (see-other (notfound))))

(define-dynamic-page bank/create ((title string (complement #'bank-exists-p)))
    ("config/bank/create")
  (no-cache)
  (with-document ()
    (:head
     (:title "Δημιουργία τράπεζας")
     (config-headers))
    (:body
     (config-header 'banks)
     (:div :id "body"
           (:div :class "message"
                 (:h2 :class "info" "Δημιουργία τράπεζας")
                 (bank-errorbar (list title)))
           (:div :id "bank" :class "window"
                 (bank-menu nil '(:view))
                 (with-form (actions/bank/create :title title)
                   (bank-table :create nil)))
           (footer)))))

(define-dynamic-page bank/update ((id    integer #'bank-id-exists-p)
                                  (title string  (complement #'bank-exists-p)))
    ("config/bank/update")
  (no-cache) 
  (if (validp id)
      (with-document ()
        (:head
         (:title "Επεξεργασία τράπεζας")
         (config-headers))
        (:body
         (config-header 'banks)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Επεξεργασία τράπεζας")
                     (bank-errorbar (list title)))
               (:div :id "bank" :class "window"
                     (bank-menu (val id) '(:view :delete))
                     (with-form (actions/bank/update :id (val id))
                       (bank-table :update id)))
               (footer))))
      (see-other (notfound))))

(define-dynamic-page bank/delete ((id integer #'bank-id-exists-p))
    ("config/bank/delete")
  (no-cache)
  (if (validp id)
      (with-document ()
        (:head
         (:title "Διαγραφή τράπεζας")
         (config-headers))
        (:body
         (config-header 'banks)
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Διαγραφή τράπεζας"))
               (:div :id "bank" :class "window"
                     (bank-menu (val id) '(:view :update))
                     (with-form (actions/bank/delete :id (val id))
                       (bank-table :delete id)))
               (footer))))
      (see-other (notfound))))
