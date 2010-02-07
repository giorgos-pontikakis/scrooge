(in-package :scrooge)

(proclaim '(optimize (safety 3) (debug 3) (compilation-speed 0) (speed 0) (space 0)))

(define-cfg '(:dbhost "localhost"
	      :dbname "scrooge"
	      :dbadapter 'postgresql
	      :dbuser "gnp"
	      :dbpass "" 
	      :port 3001
	      :webroot "/scrooge/"
	      :debug t))

(defun init-banks ()
  (mapc (lambda (pair)
	  (ignore-errors
	    (create-row 'bank :id (first pair) :title (second pair))))
	'(("nbg" "National Bank of Greece")
	  ("alpha" "Alpha Bank")
	  ("euro" "Eurobank")
	  ("general" "General Bank"))))

(defun init-tofs ()
  (mapc (lambda (pair)
	  (ignore-errors
	    (create-row 'tof :id (string-downcase (first pair)) :title (second pair))))
	'((a-thes "Α' Θεσσαλονίκης")
	  (b-thes "Β' Θεσσαλονίκης")
	  (g-thes "Γ' Θεσσαλονίκης")
	  (d-thes "Δ' Θεσσαλονίκης")
	  (e-thes "Ε' Θεσσαλονίκης")
	  (st-thes "ΣΤ' Θεσσαλονίκης")
	  (z-thes "Ζ' Θεσσαλονίκης")
	  (h-thes "Η' Θεσσαλονίκης")
	  (th-thes "Θ' Θεσσαλονίκης")
	  (i-thes "Ι' Θεσσαλονίκης")
	  (ampelokipon "Αμπελοκήπων Θεσσαλονίκης")
	  (toumpas "Τούμπας Θεσσαλονίκης")
	  (ionias "Ιωνίας Θεσσαλονίκης")
	  (kalamarias "Καλαμαριάς")
	  (neapolis "Νεάπολης") 
	  (polygyrou "Πολυγύρου")
	  (a-xan "Α' Ξάνθης")
	  (faeth "Φ.Α.Ε. Θεσσαλονίκης"))))

(defun init-companies ()
  (mapc (lambda (rec)
	  (let ((list (iter (for field-name in '(:title :occupation :tof-id :tin :address :city :pobox :zipcode))
			    (for field-value in rec)
			    (collect field-name)
			    (collect field-value))))
	    (apply #'create-row 'company list)))
	'(("Ε. Καφφές Α.Ε."
	   "Εμπορία"
	   "faeth"
	   "094307730"
	   "Προέκταση Σμύρνης"
	   "Θεσσαλονίκη"
	   :null
	   :null)
	  ("Κωνσταντίνα Γαβριήλ"
	   "Εμπορία"
	   "a-xan"
	   "063565898"
	   "Λευκίππου 1Α"
	   "Ξάνθη"
	   :null
	   :null)
	  ("Κλιμαμηχανική Α.Ε."
	   "Κλιματισμός"
	   "faeth"
	   "123456789"
	   "Βενιζέλου 5, Καλαμαριά"
	   "Θεσσαλονίκη"
	   :null
	   :null))))

(defun init-all ()
  (init-banks)
  (init-tofs)
  (init-companies))

