(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;; ;;; ------------------------------------------------------------
;; ;;; Bank - Definitions
;; ;;; ------------------------------------------------------------

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
           :item-specs (standard-actions-spec "foo" ; (bank :id id)
                                              "bar" ; (bank/create)
                                              "baz" ; (bank/update :id id)
                                              (if (or (null id)
                                                      (cheques-exist-p id))
                                                  nil
                                                  "quux"
                                                  #|(bank/delete :id id)|#))
           :enabled-items enabled-items))

(defun bank-errorbar (params)
  (render (errorbar)
          :params params
          :messages '((title "Αυτό το όνομα τράπεζας υπάρχει ήδη."))))



;;; ------------------------------------------------------------
;;; Bank table
;;; ------------------------------------------------------------

(defun mkfn-row-id (id-keys)
  (lambda (row-data)
    (plist-collect id-keys row-data)))

(defun mkfn-row-payload (data-keys operation)
  (lambda (row-data)
    (ecase operation
      ((:view :delete) (plist-collect data-keys row-data))
      ((:create :update) (plist-union (params->plist (parameters *page*))
                                      (plist-collect data-keys row-data))))))



(defun mkfn-bank-row-controls-p (operation)
  (mkfn-row-controls-p operation '(:create :update :delete)))

(defun mkfn-bank-row-selected-p (operation id-keys) 
  (mkfn-row-selected-p operation id-keys))

(defun bank-selector-cell (id)
  (selector-cell `((t   ,(bank))
                   (nil ,(apply #'bank id)))))



(defun bank-row (row-selected-p-fn row-controls-p-fn
                 row-id-fn row-payload-fn
                 cancel-url)
  (html (row-data)
    (let* ((id (funcall row-id-fn row-data))
           (payload (funcall row-payload-fn row-data))
           (row-selected-p (funcall row-selected-p-fn id))
           (row-controls-p (funcall row-controls-p-fn id)))
      (htm (:tr (funcall (bank-selector-cell id) row-selected-p)
                
                (plist-map (lambda (key value)
                             (textbox-cell (symbolicate key)
                                           value
                                           nil)) ;; todo -- style missing
                           payload)
                (ok-cell row-controls-p)
                (cancel-cell cancel-url row-controls-p))))))


(defun bank-table (operation)
  (let* ((id-keys '(:id))
         (payload-keys '(:title))
         (db-table (config-data 'bank)) 
         (cancel-url (url "bank"))
         (row-selected-p-fn (mkfn-bank-row-selected-p operation id-keys))
         (row-controls-p-fn (mkfn-bank-row-controls-p operation))
         (row-id-fn (mkfn-row-id id-keys))
         (row-payload-fn (mkfn-row-payload payload-keys operation))
         (row (bank-row row-selected-p-fn
                        row-controls-p-fn
                        row-id-fn
                        row-payload-fn
                        cancel-url)))
    (html ()
      (:table :class "forms-in-row table-half"
              (when (eql operation :create)
                (funcall row nil))
              (iter (for db-row in db-table)
                    (funcall row db-row))))))


    
;; (render (form (url (getf operation submit-urls))
;;               (list :id (val id))
;;               (body (html ()
;;                       (:table :id 'banks
;;                               :class "forms-in-row table-half"
;;                               (when (eql operation :create)
;;                                 (bank-row ni))
;;                               (iter (for db-row in db-table)
;;                                     (bank-row db-row cancel-url)))))))



;;; ------------------------------------------------------------
;;; Bank - Pages
;;; ------------------------------------------------------------

(define-dynamic-page bank ((id integer #'bank-id-exists-p))
    ("config/bank")
  (no-cache) 
  (if (validp id)
      (with-document ()
        (:head
         (:title "Τράπεζες")
         (config-headers))
        (:body
         (:div :id "header"
               (logo)
               (primary-navbar 'config)
               (config-navbar 'banks))
         (:div :id "body"
               (:div :class "message"
                     (:h2 :class "info" "Κατάλογος τραπεζών"))
               (:div :id "bank" :class "window" 
                     (bank-menu (val id) '(:create)) 
                     (funcall (bank-table :view)))
               (footer))))
      (see-other (full-url 'notfound))))



;; (define-dynamic-page bank/create ((title string (complement #'bank-exists-p)))
;;     ("config/bank/create") 
;;   (no-cache)
;;   (with-parameter-list params
;;     (config-page :name 'bank
;;                  :title "Εισαγωγή τράπεζας"
;;                  :message "Εισαγωγή τράπεζας"
;;                  :body (html ()
;;                          (bank-menu nil :view) 
;;                          (render (make-bank-table :operation :create
;;                                                   :params params))))))

;; (define-dynamic-page bank/update ((id    integer #'bank-id-exists-p) 
;;                                   (title string  (complement #'bank-exists-p)))
;;     ("config/bank/update")
;;   (no-cache)
;;   (if (validp id)
;;       (with-parameter-list params
;;         (config-page :name 'bank
;;                      :title "Επεξεργασία τράπεζας"
;;                      :message "Επεξεργασία τράπεζας"
;;                      :body (html ()
;;                              (bank-menu (val id) :view :delete) 
;;                              (render (make-bank-table :operation :update
;;                                                       :params params)))))
;;       (see-other (notfound))))

;; (define-dynamic-page bank/delete ((id integer #'bank-id-exists-p))
;;     ("config/bank/delete")
;;   (no-cache)
;;   (if (validp id)
;;       (with-parameter-list params
;;         (config-page :name 'bank
;;                      :title "Διαγραφή τράπεζας"
;;                      :message "Διαγραφή τράπεζας" 
;;                      :body (html ()
;;                              (bank-menu (val id) :view :edit) 
;;                              (render (make-bank-table :operation :delete
;;                                                       :params params)))))
;;       (see-other (notfound))))



