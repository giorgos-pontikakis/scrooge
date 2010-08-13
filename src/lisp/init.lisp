(in-package :scrooge)

(declaim (optimize (safety 3) (debug 3) (compilation-speed 0) (speed 0) (space 0)))


;; (defparameter *stran-tables* '(("Επιταγή" "cheque")
;;                                ("Έργο" "project")))

(defun stran-table-label (tbl)
  (first (find tbl *stran-tables* :key #'second :test #'string-equal)))

;; (defun stran-tables ()
;;   (mapcar #'second *stran-tables*))

(defun init-accounts ()
  )

;; (defun init-fsm ()
;;   (mapc (lambda (rec)
;; 	  (bind (((description old-status new-status debit-acc-id credit-acc-id))) (create-row )))))

(define-cfg '(:dbhost "localhost"
	      :dbname "scrooge"
	      :dbadapter 'postgresql
	      :dbuser "gnp"
	      :dbpass "" 
	      :port 3001
	      :webroot "/scrooge/"
	      :debug t))

(defun init-banks ()
  (mapc (lambda (arg)
	  (ignore-errors
	    (create-row 'bank :title arg)))
	'("Alpha Bank"
          "Αγροτική Τράπεζα της Ελλάδος"
          "Aspis Bank"
          "Εθνική Τράπεζα της Ελλάδος"
          "Τράπεζα Κύπρου"
          "Ταχυδρομικό Ταμιευτήριο"
          "Τράπεζα Πειραιώς"
          "Marfin Egnatia Bank"
          "Attica Bank"
          "EFG Eurobank Εργασίας"
          "Barclays Bank"
          "HSBC"
          "Citibank Ελλάδα"
          "Probank"
          "Ελληνική Τράπεζα"
          "Millenium bank"
          "Proton Bank")))

(defun init-tofs ()
  (mapc (lambda (arg)
	  (ignore-errors
	    (create-row 'tof :title arg)))
	'("Α' Θεσσαλονίκης"
	  "Β' Θεσσαλονίκης"
	  "Γ' Θεσσαλονίκης"
	  "Δ' Θεσσαλονίκης"
	  "Ε' Θεσσαλονίκης"
	  "ΣΤ' Θεσσαλονίκης"
	  "Ζ' Θεσσαλονίκης"
	  "Η' Θεσσαλονίκης"
	  "Θ' Θεσσαλονίκης"
	  "Ι' Θεσσαλονίκης"
	  "Αμπελοκήπων Θεσσαλονίκης"
	  "Τούμπας Θεσσαλονίκης"
	  "Ιωνίας Θεσσαqλονίκης"
	  "Καλαμαριάς"
	  "Νεάπολης" 
	  "Πολυγύρου"
	  "Α' Ξάνθης"
	  "Φ.Α.Ε. Θεσσαλονίκης")))

(defun init-companies ()
  (mapc (lambda (rec)
	  (let ((list (iter (for field-name in '(:title :occupation :tof-id :tin :address :city :pobox :zipcode))
			    (for field-value in rec)
			    (collect field-name)
			    (collect field-value))))
	    (apply #'create-row 'company list)))
	'(("Ε. Καφφές Α.Ε."
	   "Εμπορία"
	   1
	   "094307730"
	   "Προέκταση Σμύρνης"
	   "Θεσσαλονίκη"
	   :null
	   :null)
	  ("Κωνσταντίνα Γαβριήλ"
	   "Εμπορία"
	   2
	   "063565898"
	   "Λευκίππου 1Α"
	   "Ξάνθη"
	   :null
	   :null)
	  ("Κλιμαμηχανική Α.Ε."
	   "Κλιματισμός"
	   1
	   "123456789"
	   "Βενιζέλου 5, Καλαμαριά"
	   "Θεσσαλονίκη"
	   :null
	   :null))))

(defun init-all ()
  (init-banks)
  (init-tofs)
  (init-companies))

