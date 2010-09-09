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



;; ;;; ------------------------------------------------------------
;; ;;; Bank - Actions
;; ;;; ------------------------------------------------------------

;; (define-dynamic-page actions/bank/create ((title string (complement #'bank-exists-p)))
;;     ("actions/bank/create" :request-type :post) 
;;   (no-cache)
;;   (with-parameter-list params 
;;     (if (every #'validp params)
;; 	(with-parameter-rebinding #'val
;; 	  (with-db
;; 	    (insert-dao (make-instance 'bank :title title))
;; 	    (see-other (bank :id (bank-id title)))))
;; 	(with-parameter-rebinding #'raw 
;; 	  (see-other (bank/create :title title))))))

;; (define-dynamic-page actions/bank/update ((id    integer #'bank-id-exists-p)
;;                                           (title string  (complement #'bank-exists-p)))
;;     ("actions/bank/update" :request-type :post)
;;   (no-cache) 
;;   (with-parameter-list params
;;     (if (every #'validp params)
;; 	(with-parameter-rebinding #'val
;; 	  (with-db
;; 	    (execute (:update 'bank :set 
;; 			      'title title
;; 			      :where (:= 'id id)))
;; 	    (see-other (bank :id id))))
;; 	(with-parameter-rebinding #'raw
;; 	  (see-other (bank/update :id id :title title))))))

;; (define-dynamic-page actions/bank/delete ((id integer #'bank-id-exists-p))
;;     ("actions/bank/delete" :request-type :post)
;;   (if (validp id)
;;       (with-db
;; 	(delete-dao (get-dao 'bank (val id)))
;; 	(see-other (bank)))
;;       (see-other (notfound))))



;;; ------------------------------------------------------------
;;; Bank - Snippets
;;; ------------------------------------------------------------

(defhtml generic-menu (id div-style ul-style item-specs enabled-items)
  (:div :id id
        :class div-style
        (:ul :class ul-style
             (iter (for (action-name href label &optional img-url) in item-specs)
                   (when (and (member action-name enabled-items)
                              (not (null href)))
                     (htm (:li (:a :href href
                                   (when img-url
                                     (htm (:img :src img-url)))
                                   (str label)))))))))

(defun table-actions-menu ()
  (generic-menu :div-style "actions"
                :ul-style "hmenu"))

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

;; (define-errorbar bank-errorbar (:ul-style "error") 
;;   (title "Αυτό το όνομα τράπεζας υπάρχει ήδη.")) 



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
               (:div :id "banks" :class "window" 
                     (bank-menu (val id) '(:create)) 
                     #|(banks-table :view (val filter) (val id) nil)|#)
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



