(in-package :scrooge)


;;; --- Config --------------------

(defun config-head ()
  (with-html
    (:head
     (:title "Σκρουτζ: Ρυθμίσεις")
     (css "reset.css" "scrooge.css"))))

(defun config-tables (&key bank-id city-id tof-id)
  (with-db
    (let ((bank-data (query (:order-by (:select 'id 'title :from 'bank) 'title)))
	  (city-data (query (:order-by (:select 'id 'title :from 'city) 'title)))
	  (tof-data  (query (:order-by (:select 'id 'title :from 'tof) 'title))))
      (with-html
	(:div :id "body"
	      (:div :class "column"
		    (:ul :class "inline"
			 (:li (link "Εισαγωγή" :href (bank/insert)))
			 (:li (link "Επεξεργασία" :href (if bank-id
							    (bank/edit :bank-id bank-id)
							    nil)))
			 (:li (link "Διαγραφή" :href (if bank-id
							 (bank/remove :bank-id bank-id)
							 nil))))
		    (table bank-data
			   :id-fn #'first
			   :td-fn #'rest 
			   :header '("Τράπεζες") 
			   :href-fn (lambda (bank-id)
				      (config :bank-id bank-id))
			   :active-id bank-id
			   :style "dbtable"
			   :id "banks"))
	      (:div :class "column"
		    (:ul :class "inline"
			 (link "Εισαγωγή" :href (city/insert))
			 (link "Επεξεργασία" :href (if city-id
						       (city/edit :city-id city-id)
						       nil))
			 (link "Διαγραφή" :href (if city-id
						    (city/remove :city-id city-id)
						    nil)))
		    (table city-data   
			   :id-fn #'first
			   :td-fn #'rest 
			   :header '("Πόλεις")
			   :href-fn (lambda (city-id)
				      (config :city-id city-id))
			   :active-id city-id
			   :style "dbtable"
			   :id "cities")) 
	      (:div :class "column"
		    (:ul :class "inline"
			 (link "Εισαγωγή" :href (tof/insert))
			 (link "Επεξεργασία" :href (if tof-id
						       (tof/edit :tof-id tof-id)
						       nil))
			 (link "Διαγραφή" :href (if tof-id
						    (tof/remove :tof-id tof-id)
						    nil)))
		    (table tof-data  
			   :id-fn #'first
			   :td-fn #'rest 
			   :header '("Δ.Ο.Υ.")
			   :href-fn (lambda (tof-id)
				      (config :tof-id tof-id))
			   :active-id tof-id
			   :style "dbtable"
			   :id "tofs")))))))

(define-dynamic-page config ((bank-id integer) (city-id integer) (tof-id integer)) ("config/") 
  (with-auth "root"
    (with-page ()
      (config-head)
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Ρυθμίσεις"))
       (config-tables :bank-id bank-id :city-id city-id :tof-id tof-id)))))


;;; --- Banks, cities, tofs --------------------

(define-dynamic-page bank/insert () ("config/bank/insert")
  (with-auth "root"
    (with-page ()
      (config-head)
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Ρυθμίσεις"))
       (:div :id "prompt"
	     (with-form (insert-bank) 
	       (:ul :class "inline" 
		    (label 'title "Νέα τράπεζα: ")
		    (textbox 'title)
		    (submit "Εισαγωγή")
		    (link "Ακύρωση" :href (config)))))
       (config-tables)))))

(define-dynamic-page city/insert () ("config/city/insert")
  (with-auth "root"
    (with-page ()
      (config-head)
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Ρυθμίσεις"))
       (:div :id "prompt"
	     (with-form (insert-city) 
	       (:ul :class "inline" 
		    (label 'title "Νέα πόλη:")
		    (textbox 'title)
		    (submit "Εισαγωγή")
		    (link "Ακύρωση" :href (config)))))
       (config-tables)))))

(define-dynamic-page tof/insert () ("config/tof/insert")
  (with-auth "root"
    (with-page ()
      (config-head)
      (:body
       (:div :id "header"
	     (logo)
	     (navbar "Ρυθμίσεις"))
       (:div :id "prompt"
	     (with-form (insert-tof) 
	       (:ul :class "inline" 
		    (label 'title "Νέα Δ.Ο.Υ.:")
		    (textbox 'title)
		    (submit "Εισαγωγή")
		    (link "Ακύρωση" :href (config)))))
       (config-tables)))))


(define-dynamic-page bank/edit ((bank-id integer)) ("config/bank/edit")
  (with-auth "root"
    (with-db
      (let ((bank (get-dao 'bank bank-id)))
	(with-page () 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (edit-bank :id bank-id) 
		   (:ul :class "inline" 
			(label 'title "Επεξεργασία τράπεζας: ")
			(textbox 'title :value (title bank))
			(submit "Ενημέρωση")
			(link "Ακύρωση" :href (config :bank-id bank-id)))))
	   (config-tables :bank-id bank-id)))))))

(define-dynamic-page city/edit ((city-id integer)) ("config/city/edit")
  (with-auth "root"
    (with-db
      (let ((city (get-dao 'city city-id)))
	(with-page () 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (edit-city :id city-id) 
		   (:ul :class "inline" 
			(label 'title "Επεξεργασία πόλης: ")
			(textbox 'title :value (title city))
			(submit "Ενημέρωση")
			(link "Ακύρωση" :href (config :city-id city-id)))))
	   (config-tables :city-id city-id)))))))

(define-dynamic-page tof/edit ((tof-id integer)) ("config/tof/edit")
  (with-auth "root"
    (with-db
      (let ((tof (get-dao 'tof tof-id)))
	(with-page () 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (edit-tof :id tof-id) 
		   (:ul :class "inline" 
			(label 'title "Επεξεργασία Δ.Ο.Υ.: ")
			(textbox 'title :value (title tof))
			(submit "Ενημέρωση")
			(link "Ακύρωση" :href (config :tof-id tof-id)))))
	   (config-tables :tof-id tof-id)))))))


(define-dynamic-page bank/remove ((bank-id integer)) ("config/bank/remove")
  (with-auth "root"
    (with-db
      (let ((bank (get-dao 'bank  bank-id)))
	(with-page () 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (remove-bank :id bank-id) 
		   (:ul :class "inline" 
			(label 'title "Διαγραφή τράπεζας: ")
			(textbox 'title :disabledp t :value (title bank))
			(submit "Διαγραφή")
			(link "Ακύρωση" :href (config :bank-id bank-id)))))
	   (config-tables :bank-id bank-id)))))))

(define-dynamic-page city/remove ((city-id integer)) ("config/city/remove")
  (with-auth "root"
    (with-db
      (let ((city (get-dao 'city city-id)))
	(with-page () 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (remove-city :id city-id) 
		   (:ul :class "inline" 
			(label 'title "Διαγραφή πόλης: ")
			(textbox 'title :disabledp t :value (title city))
			(submit "Διαγραφή")
			(link "Ακύρωση" :href (config :city-id city-id)))))
	   (config-tables :city-id city-id)))))))

(define-dynamic-page tof/remove ((tof-id integer)) ("config/tof/remove")
  (with-auth "root"
    (with-db
      (let ((tof (get-dao 'tof tof-id)))
	(with-page () 
	  (config-head)
	  (:body
	   (:div :id "header"
		 (logo)
		 (navbar "Ρυθμίσεις"))
	   (:div :id "prompt"
		 (with-form (remove-tof :id tof-id) 
		   (:ul :class "inline" 
			(label 'title "Διαγραφή Δ.Ο.Υ.: ")
			(textbox 'title :disabledp t :value (title tof))
			(submit "Διαγραφή")
			(link "Ακύρωση" :href (config :tof-id tof-id)))))
	   (config-tables :tof-id tof-id)))))))







(defmacro with-defaults (args &body body)
  (let ((params (mapcar #'(lambda (sym)
			    (concatenate 'string (string-downcase sym) "-default"))
			args)))
    `(let ,(mapcar #'(lambda (arg param)
		       `(,arg (get-parameter ,param)))
		   args params)
       ,@body)))


