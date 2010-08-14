(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; ------------------------------------------------------------
;;; Definition
;;; ------------------------------------------------------------
(defclass stran-table-inline-form (table-inline-form)
  ((id-cols :initform '(:stran-id :tbl))
   (data-cols :initform '(:tbl
                          :description
                          :old-status
                          :new-status
                          :debit-acc
                          :credit-acc))
   (filter-keys :initform '())
   #|(cols :initform '(:id
                     :tbl
                     :tbl
                     :description
                     :debit-acc
                     :credit-acc
                     :old-status
                     :new-status
                     :submit
                     :cancel))|# 
   (header :initform '(:id          ""
                       :tbl         "Πίνακας"
                       :description "Περιγραφή"         
                       :old-status  "Αρχική Κατάσταση"  
                       :new-status  "Τελική Κατάσταση"
                       :debit-acc   "Λογ. Χρέωσης"      
                       :credit-acc  "Λογ. Πίστωσης"     
                       :submit      ""
                       :cancel      ""))
   (styles :initform '(:cols (:id "select"
                              :tbl "data"
                              :description "data"
                              :old-status "data"
                              :new-status "data"
                              :debit-acc "data"
                              :credit-acc "data"
                              :submit "button"
                              :cancel "button")
                       :table "forms-in-row"))
   (post-urls :initform '(:create actions/stran/create
                          :update actions/stran/update
                          :delete actions/stran/delete)) 
   (get-urls :initform '(:view   stran
                         :create stran/screate
                         :update stran/supdate
                         :delete stran/sdelete))))

(register-widget 'stran 'stran-table-inline-form)

(defmethod read-db ((obj stran-table-inline-form) &key filters)
  (declare (ignore filters))
  (flet ((select (table)
           `(:select (:as ,(symbolicate table '-stran.id) 'stran-id)
                     (:as 'stran.id 'tbl)
                     ,(symbolicate table '-stran.description)
                     'old-status 'new-status 
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
    (with-db
      (query (sql-compile `(:union ,(select "project")
                                   ,(select "cheque")))
             :plists))))

;; (defmethod read-db ((obj stran-table-inline-form) &key filters)
;;   (declare (ignore filters))
;;   (flet ((select (table)
;;            `(:select (:as ,(symbolicate table '-stran.id) 'stran-id)
;;                      (:as )
;;                      'description 'old-status 'new-status
;;                      (:as 'debit-account.title 'debit-acc)
;;                      (:as 'credit-account.title 'credit-acc) 
;;                      :from ,(symbolicate table '-stran) 
;;                      :left-join (:as 'account 'debit-account)
;;                      :on (:= 'debit-account.id
;;                              ,(symbolicate table '-stran.debit-acc-id))
;;                      :left-join (:as 'account 'credit-account)
;;                      :on (:= 'credit-account.id
;;                              ,(symbolicate table '-stran.credit-acc-id)))))
;;     (with-db
;;       (query (sql-compile `(:union ,(append '(:tbl "project") (select "project"))
;;                                    ,(append '(:tbl "cheque") (select "cheque"))))
;;              :plists))))

(defmethod form-row ((obj stran-table-inline-form) row-id row-data intent params) 
  (flet ((style (indicator)
           (if (validp (find indicator params :key #'name)) nil "attention"))
         (datum (indicator)
           (getf row-data indicator))
         (stran-tables ()
           (with-db
             (query (:select 'description 'id :from 'stran))))) 
    (with-db
      (let ((actionfn (getf (post-urls obj) intent))
            (viewfn (getf (get-urls obj) :view))) 
        (make-form (funcall actionfn)
                   (html ()
                     (:tr :style "active"
                          (cell-selector obj
                                         :col :id
                                         :href (funcall viewfn)
                                         :activep t)
                          (cell-dropdown obj
                                         :col :tbl
                                         :value (datum :tbl)
                                         :pairs (stran-tables))
                          (cell-textbox  obj
                                         :col :description
                                         :value (datum :description)
                                         :style (style :description))
                          (cell-textbox obj
                                        :col :old-status
                                        :value (datum :old-status)
                                        :style (style :old-status))
                          (cell-textbox obj
                                        :col :new-status
                                        :value (datum :new-status)
                                        :style (style :new-status))
                          (cell-textbox obj
                                        :col :debit-acc
                                        :value (datum :debit-acc)
                                        :style (style :debit-acc))
                          (cell-textbox obj
                                        :col :credit-acc
                                        :value (datum :credit-acc)
                                        :style (style :credit-acc)) 
                          (cell-submit obj :col :submit)
                          (cell-anchor obj
                                       :col :cancel
                                       :href (funcall viewfn)))))))))



;;; ------------------------------------------------------------
;;; State transitions - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/stran/create ((tbl string)
                                           (description string #'not-db-null-p)
                                           (debit-acc string #'account-exists-p)
                                           (credit-acc string #'account-exists-p) 
                                           (old-status string)
                                           (new-status string))
    ("actions/stran/create" :request-type :post
                            :validators ((old-status (valid-combo tbl old-status))
                                         (new-status (valid-combo tbl new-status))))
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (let ((debit-acc-id (account-id debit-acc))
		(credit-acc-id (account-id credit-acc))) 
	    (with-db
	      (insert-dao (make-instance (symbolicate (string-upcase tbl) "-STRAN")
					 :description description
					 :debit-acc-id debit-acc-id
					 :credit-acc-id credit-acc-id
					 :old-status old-status
					 :new-status new-status))
	      (redirect (stran) :code +http-see-other+))))
	(with-parameter-rebinding #'raw 
	  (redirect (stran/create :tbl tbl
                                  :description description
                                  :debit-acc debit-acc
                                  :credit-acc credit-acc
                                  :old-status old-status
                                  :new-status new-status)
		    :code +http-see-other+)))))

(define-dynamic-page actions/stran/delete ((stran-id integer)
                                           (tbl string))
    ("actions/stran/delete" :request-type :post
                            :validators (((stran-id tbl) (valid-stran-id-p stran-id tbl))))
  (no-cache) 
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (with-db 
	    (delete-dao (get-dao (symbolicate (string-upcase tbl) "-STRAN") stran-id))
	    (redirect (stran) :code +http-see-other+)))
	(redirect (notfound) :code +http-see-other+))))

(define-dynamic-page actions/stran/update ((stran-id integer)
                                           (tbl string)
                                           (description string #'not-db-null-p)
                                           (debit-acc string #'account-exists-p)
                                           (credit-acc string #'account-exists-p) 
                                           (old-status string)
                                           (new-status string))
    ("actions/stran/update"
     :request-type :post
     :validators ((old-status (valid-combo tbl old-status))
		  (new-status (valid-combo tbl new-status))
		  ((stran-id tbl) (valid-stran-id-p stran-id tbl))))
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
	(with-parameter-rebinding #'val
	  (let ((debit-acc-id (account-id debit-acc))
		(credit-acc-id (account-id credit-acc)))
	    (with-db
	      (execute (:update (symbolicate tbl "-STRAN") :set
				'description description
				'debit-acc-id debit-acc-id
				'credit-acc-id credit-acc-id 
				'old-status old-status
				'new-status new-status
				:where (:= 'id stran-id)))
	      (redirect (stran) :code +http-see-other+))))
	(with-parameter-rebinding #'raw 
	  (redirect (stran/update :stran-id stran-id
                                  :description description
                                  :debit-acc debit-acc
                                  :credit-acc credit-acc
                                  :tbl tbl
                                  :old-status old-status
                                  :new-status new-status)
		    :code +http-see-other+)))))


;;; ------------------------------------------------------------
;;; State transitions - Snippets
;;; ------------------------------------------------------------

(define-menu stran-menu (stran-id tbl) (:div-style "actions" :ul-style "hmenu")
  (:create (with-html
	     (:li (:a :href (stran/create)
		      (:img :src (url "img/add.png")) "Δημιουργία"))))
  (:view (with-html
	   (:li (:a :href (stran :stran-id stran-id :tbl tbl)
		    (:img :src (url "img/magnifier.png")) "Προβολή"))))
  (:update (if stran-id
	       (with-html
		 (:li (:a :href (stran/update :stran-id stran-id :tbl tbl)
			  (:img :src (url "img/pencil.png")) "Επεξεργασία")))
	       nil))
  (:delete (if stran-id
	       (with-html
		 (:li (:a :href (stran/delete :stran-id stran-id :tbl tbl)
			  (:img :src (url "img/delete.png")) "Διαγραφή")))
	       nil)))

(define-errorbar stran-errorbar (:ul-style "error")
  (description "Η περιγραφή δεν πρέπει να είναι κενή")
  (debit-acc "Άκυρος λογαριασμός χρέωσης")
  (credit-acc "Άκυρος λογαριασμός πίστωσης")
  (old-status "Άκυρη αρχική κατάσταση")
  (new-status "Άκυρη τελική κατάσταση"))


;;; ------------------------------------------------------------
;;; State transitions - Pages
;;; ------------------------------------------------------------

(define-dynamic-page stran ((stran-id integer)
                            (tbl string))
    ("config/stran/" :validators (((stran-id tbl) (valid-stran-id-p stran-id tbl))))
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
        (with-page ()
          (:head
           (:title "Καταστατικές Μεταβολές")
           (config-headers))
          (:body
           (:div :id "header"
                 (logo)
                 (primary-navbar 'config)
                 (config-navbar 'stran))
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Κατάλογος Καταστατικών Μεταβολών"))
                 (:div :id "stran" :class "window"
                       (stran-menu (val stran-id) (val tbl) :create :update :delete) 
                       (render (find-widget 'stran)
                               :intent :view :params params)))
           (footer)))
	(redirect (notfound) :code +http-see-other+))))

(define-dynamic-page stran/create ((tbl         string #'valid-tbl-p)
                                   (description string #'not-db-null-p) 
                                   (debit-acc   string #'account-exists-p)
                                   (credit-acc  string #'account-exists-p)
                                   (old-status  string)
                                   (new-status  string))
    ("config/stran/create" :validators ((old-status (valid-combo tbl old-status))
                                        (new-status (valid-combo tbl new-status))))
  (no-cache)
  (if (validp tbl)
      (with-parameter-list params
        (with-page ()
          (:head
           (:title "Καταστατικές Μεταβολές: Δημιουργία")
           (config-headers))
          (:body
           (:div :id "header"
                 (logo)
                 (primary-navbar 'config)
                 (config-navbar 'stran))
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Δημιουργία μετάβασης")
                       (stran-errorbar description debit-acc credit-acc old-status new-status)) 
                 (:div :id "stran" :class "window"
                       (stran-menu nil (val tbl) :view)
                       (render (find-widget 'stran)
                               :intent :create :params params))))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page stran/update ((stran-id      integer)
                                   (tbl         string #'valid-tbl-p)
                                   (description string #'not-db-null-p)
                                   (debit-acc   string #'account-exists-p) 
                                   (credit-acc  string #'account-exists-p) 
                                   (old-status  string)
                                   (new-status  string))
    ("config/stran/update" :validators ((old-status (valid-combo tbl old-status))
                                        (new-status (valid-combo tbl new-status))
                                        ((stran-id tbl) (valid-stran-id-p stran-id tbl))))
  (no-cache)
  (if (validp stran-id)
      (with-parameter-list params
        (with-page ()
          (:head
           (:title "Καταστατικές Μεταβολές: Επεξεργασία")
           (config-headers))
          (:body
           (:div :id "header"
                 (logo)
                 (primary-navbar 'config)
                 (config-navbar 'stran))
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Επεξεργασία μετάβασης")
                       (stran-errorbar description debit-acc credit-acc old-status new-status))
                 (:div :id "stran" :class "window" 
                       (stran-menu (val stran-id) (val tbl) :view :delete)
                       (render (find-widget 'stran)
                               :intent :update :params params))))))
      (redirect (notfound) :code +http-see-other+)))

(define-dynamic-page stran/delete ((stran-id integer)
                                   (tbl string #'valid-tbl-p))
    ("config/stran/delete" :validators (((stran-id tbl) (valid-stran-id-p stran-id tbl))))
  (no-cache) 
  (if (validp stran-id)
      (with-parameter-list params
	(with-page ()
	  (:head
	   (:title "Καταστατικές Μεταβολές: Διαγραφή")
	   (config-headers))
	  (:body
	   (:div :id "header"
		 (logo)
		 (primary-navbar 'config)
		 (config-navbar 'stran))
	   (:div :id "body"
		 (:div :class "message"
		       (:h2 :class "info" "Διαγραφή μετάβασης")) 
		 (:div :id "stran" :class "window"
		       (stran-menu (val stran-id) (val tbl) :view :update)
		       (render (find-widget 'stran)
                                 :intent :delete :params params))))))
      (redirect (notfound) :code +http-see-other+)))



;;; Form row

;; (defun stran-form-row (intent submit cancel values styles &rest args)
;;   (make-form (apply submit args)
;;              (html ()
;;                (:tr :class (if (eql intent :delete) "attention" "active")
;;                     (selector-td t (funcall cancel))
;;                     (:td :class "data" (dropdown 'tbl (str (with-db
;;                                                              (query (:select 'id 'description
;;                                                                              :from 'stran-tables))))
;;                                                  :selected (getf args :tbl)
;;                                                  :disabledp (not (eql intent :create))
;;                                                  :readonlyp (not (eql intent :create))))
;;                     (row-td *stran-td-keys* *stran-td-styles*
;;                             intent values styles) 
;;                     (:td :class "button" (ok-button))
;;                     (:td :class "button" (cancel-button (funcall cancel)))))))

;; (defun stran-row-create (tbl values styles)
;;   (stran-form-row :create
;;                   #'actions/stran/create
;;                   #'stran
;;                   values
;;                   styles
;;                   :tbl tbl))

;; (defun stran-row-update (id tbl values styles)
;;   (stran-form-row :update
;;                   #'actions/stran/update
;;                   #'stran
;;                   values
;;                   styles
;;                   :stran-id id :tbl tbl))

;; (defun stran-row-delete (id tbl values styles) 
;;   (stran-form-row :delete
;;                   #'actions/stran/delete
;;                   #'stran
;;                   values
;;                   styles
;;                   :stran-id id :tbl tbl))

;;; Table

;; (defun stran-html-table (intent active-id active-tbl &rest params) 
;;   (let* ((header '("" "Πίνακας" "Περιγραφή" "Αρχική Κατάσταση"
;;                    "Τελική Κατάσταση" "Λογ. Χρέωσης" "Λογ. Πίστωσης" "" "")) 
;;          (in-values (zip params params :key1 #'name :key2 #'val*))
;;          (in-styles (zip params params :key1 #'name :key2 #'style-invalid-p))
;;          (stran-tables (with-db (query (:select 'id :from 'stran-tables )
;;                                        :column))))
;;     (with-html
;;       (:table :id "stran-table" :class "forms-in-row"
;;               (:thead
;;                (:tr (iter (for label in header) 
;;                           (htm (:th (str label))))))
;;               (:tbody
;;                (when (eql intent :create) 
;;                  (stran-row-create active-tbl in-values in-styles))
;;                (iter (for tbl in stran-tables)
;;                      (iter (for db-values in (get-stran-data tbl)) 
;;                            (let ((activep (and active-id active-tbl
;;                                                (equal active-id (getf db-values :id))
;;                                                (equal active-tbl tbl)))) 
;;                              (if activep
;;                                  (let ((values (unionf in-values db-values))) 
;;                                    (case intent
;;                                      (:view (stran-row-view activep tbl values in-styles))
;;                                      (:update (stran-row-update active-id tbl values in-styles))
;;                                      (:delete (stran-row-delete active-id tbl values in-styles))))
;;                                  (stran-row-view activep tbl db-values in-styles))))))))))


;; (setf (db-getter (get-widget 'stran))
;;       #'(lambda (ids filters)
;;           (let ((table (getf ids :tbl)))
;;             (with-db
;;               (query (:select (symbolicate table '-stran.id)
;;                               'description 'old-status 'new-status
;;                               (:as 'debit-account.title 'debit-acc)
;;                               (:as 'credit-account.title 'credit-acc) 
;;                               :from (symbolicate table '-stran) 
;;                               :left-join (:as 'account 'debit-account)
;;                               :on (:= 'debit-account.id
;;                                       (symbolicate table '-stran.debit-acc-id))
;;                               :left-join (:as 'account 'credit-account)
;;                               :on (:= 'credit-account.id
;;                                       (symbolicate table '-stran.credit-acc-id)))
;;                      :plists)))))

;; (defun get-stran-data (table)
  
;;   )



;; (defun stran-simple-row (intent activep cancel values styles &key tbl) 
;;   (with-html 
;;     (:tr :class (if activep "active" nil)
;;          (selector-td activep (if activep
;;                                   (funcall cancel)
;;                                   (funcall cancel :stran-id (getf values :id) :tbl tbl)))
;;          (:td :class "data" (str (with-db
;;                                    (query (:select 'description :from 'stran-tables))))) 
;;          (row-td *stran-td-keys* *stran-td-styles*
;;                  intent values styles) 
;;          (:td :class "button" "")
;;          (:td :class "button" ""))))

;; (defun stran-row-view (activep tbl values styles) 
;;   (stran-simple-row :view
;;                     activep
;;                     #'stran
;;                     values
;;                     styles
;;                     :tbl tbl))
