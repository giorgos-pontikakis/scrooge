(in-package :scrooge)

(disable-sql-reader-syntax)
(enable-sql-reader-syntax)

(define-cfg '(:dbhost "localhost"
	      :dbname "scrooge"
	      :dbadapter 'postgresql
	      :dbuser "gnp"
	      :dbpass ""
	      :access-log-path  #p"/home/gnp/www/scrooge/logs/hunchentoot-scrooge-access.log" 
	      :message-log-path #p"/home/gnp/www/scrooge/logs/hunchentoot-scrooge-message.log"
	      :port 3001
	      :webroot "/scrooge/"
	      :debug t))

(defun acc-grp-id (basename)
  (with-db 
    (select-unique [id]
		   :from [account-group]
		   :where [= [basename] basename]
		   :flatp t)))

(defun acc-id (basename)
  (with-db
    (select-unique [id]
		   :from [account]
		   :where [= [basename] basename]
		   :flatp t)))





;; --------------------------------------------------------------------------------
;; Account Groups
;; --------------------------------------------------------------------------------
(defun create-account-groups ()
  (with-db
    (flet ((ensure-account-group (basename title debit-p)
	     (let ((exists (select-unique [id]
					  :from [account-group]
					  :where [= [basename] basename]
					  :flatp t))
		   (debit-account-p (bool debit-p)))
	       (unless exists
		 (create-account-group basename title debit-account-p)))))
      (iter (for (basename title debit-p) in '(("assets" "Περιουσιακά στοιχεία" t)
					       ("expenses" "Έξοδα" t)
					       ("drawings" "Αναλήψεις" t)
					       ;; credit account groups
					       ("liabilities" "Υποχρεώσεις" nil)
					       ("revenues" "Έσοδα" nil)
					       ("capital" "Κεφάλαιο" nil)))
	    (ensure-account-group basename title debit-p)))))


;; --------------------------------------------------------------------------------
;; Account Groups
;; --------------------------------------------------------------------------------
;; (defun create-accounts ()
;;   (with-db
;;     (flet ((ensure-account (basename title acc-grp-basename)
;; 	     (let ((exists (select-unique [id]
;; 					  :from [account]
;; 					  :where [= [basename] basename]
;; 					  :flatp t)))
;; 	       (unless exists
;; 		 (create-account basename title (acc-grp-id acc-grp-basename))))))
;;       (iter (for (acc-grp-name acc-names) in '(("assets"
;; 						(("cash"       "Μετρητά")
;; 						 ("cheques-in" "Επιταγές προς είσπραξη")
;; 						 ("receivable"  "Λογαριασμοί προς είσπραξη")))
;; 					       ("liabilities"
;; 						(("payable"     "Λογαριασμοί προς εξόφληση")
;; 						 ("cheques-out" "Επιταγές προς εξόφληση")))))
;; 	    (iter (for (basename title) in acc-names)
;; 		  (ensure-account basename title acc-grp-name))))))

(defun create-accounts ()
  (with-db
    (flet ((ensure-account (basename title debit-account-p)
	     (let ((exists (select-unique [id]
					  :from [account]
					  :where [= [basename] basename]
					  :flatp t)))
	       (unless exists
		 (create-account nil basename title (bool debit-account-p))))))
      (iter (for (basename title debit-account-p) in  '(("cash"        "Μετρητά" t)
							("cheques-in"  "Επιταγές προς είσπραξη" t)
							("receivable"  "Λογαριασμοί προς είσπραξη" t)
							("payable"     "Λογαριασμοί προς εξόφληση" nil)
							("cheques-out" "Επιταγές προς εξόφληση" nil)))
	    (ensure-account basename title debit-account-p)))))

;; --------------------------------------------------------------------------------
;; Account Groups
;; --------------------------------------------------------------------------------
(defun create-banks ()
  (with-db
    (flet ((ensure-bank (title)
	     (let ((exists (select-unique [id]
					  :from [bank]
					  :where [= [title] title]
					  :flatp t)))
	       (unless exists
		 (create-bank title)))))
      (mapc #'ensure-bank '("Alpha Bank" "Eurobank" "Εθνική" "Γενική")))))

;; --------------------------------------------------------------------------------
;; Cities
;; --------------------------------------------------------------------------------
(defun create-cities ()
  (with-db
    (flet ((ensure-city (title)
	     (let ((exists (select-unique [id]
					  :from [city]
					  :where [= [title] title]
					  :flatp t)))
	       (unless exists
		 (create-city title)))))
      (mapc #'ensure-city '("Θεσσαλονίκη"
			    "Αθήνα"
			    "Καβάλα"
			    "Ξάνθη"
			    "Κομοτηνή"
			    "Αλεξανδρούπολη"
			    "Κιλκίς"
			    "Σέρρες"
			    "Έδεσσα"
			    "Αριδαία"
			    "Σκύδρα"
			    "Πολύγυρος"
			    "Νέα Μουδανιά")))))


;; --------------------------------------------------------------------------------
;; Taxation Offices
;; --------------------------------------------------------------------------------
(defun create-tofs ()
  (with-db
    (flet ((ensure-tof (title)
	     (let ((exists (select-unique [id]
					  :from [tof]
					  :where [= [title] title]
					  :flatp t)))
	       (unless exists
		 (create-tof title)))))
      (mapc #'ensure-tof '("A΄ Θεσσαλονίκης"
			   "Β΄ Θεσσαλονίκης" 
			   "Δ΄ Θεσσαλονίκης"
			   "Ε΄ Θεσσαλονίκης"
			   "ΣΤ΄ Θεσσαλονίκης"
			   "Ζ΄ Θεσσαλονίκης"
			   "Η΄ Θεσσαλονίκης"
			   "Θ΄ Θεσσαλονίκης"
			   "Ι΄ Θεσσαλονίκης"
			   "Αμπελοκήπων Θεσσαλονίκης"
			   "Τούμπας Θεσσαλονίκης"
			   "Ιωνίας Θεσσαλονίκης"
			   "Καλαμαριάς"
			   "Νεάπολης" 
			   "Πολυγύρου"
			   "Α΄ Ξάνθης")))))


;; (create-tx-type "Επιταγή"
;; 		 (acc-id "cheques-in")
;; 		 (acc-id "receivable"))

;; (create-tx-type "Μετρητά"
;; 		 (acc-id "cash")
;; 		 (accr-id "receivable"))

(defun initall ()
  (with-db
  (let ((user (select-unique [username]
			     :from [webuser]
			     :where [= [username] "gnp"])))
    (unless user
      (create-webuser "gnp" "gnp!scrooge" "root"))))
  (create-banks)
  (create-tofs)
  (create-cities)
  (create-accounts))

