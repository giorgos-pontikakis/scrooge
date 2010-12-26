(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; ----------------------------------------------------------------------
;;; Default table values (only for development)
;;; ----------------------------------------------------------------------
(defun seed-banks ()
  (mapc (lambda (arg)
          (ignore-errors
            (create-row 'bank :title (string-upcase-gr arg))))
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
          "Citibank Ελλάδος"
          "Probank"
          "Ελληνική Τράπεζα"
          "Millenium bank"
          "Proton Bank"))
  nil)

(defun seed-tofs ()
  (mapc (lambda (arg)
          (ignore-errors
            (create-row 'tof :title (string-upcase-gr arg))))
        '("ΆΝΔΡΟΥ"
          "Α΄ ΑΘΗΝΩΝ"
          "Β΄ ΑΘΗΝΩΝ"
          "Γ΄ ΑΘΗΝΩΝ"
          "Δ΄ ΑΘΗΝΩΝ"
          "Ε΄ ΑΘΗΝΩΝ"
          "ΣΤ΄ ΑΘΗΝΩΝ"
          "Ζ΄ ΑΘΗΝΩΝ"
          "Η΄ ΑΘΗΝΩΝ"
          "Θ΄ ΑΘΗΝΩΝ"
          "Ι΄ ΑΘΗΝΩΝ"
          "ΙΑ΄ ΑΘΗΝΩΝ"
          "ΙΒ΄ ΑΘΗΝΩΝ"
          "ΙΓ΄ ΑΘΗΝΩΝ"
          "ΙΔ΄ ΑΘΗΝΩΝ"
          "ΙΕ΄ ΑΘΗΝΩΝ"
          "ΙΣΤ΄ ΑΘΗΝΩΝ"
          "ΙΖ΄ ΑΘΗΝΩΝ"
          "ΙΗ΄ ΑΘΗΝΩΝ"
          "ΙΘ΄ ΑΘΗΝΩΝ"
          "Κ΄ ΑΘΗΝΩΝ"
          "ΚΑ΄ ΑΘΗΝΩΝ"
          "ΚΒ΄ ΑΘΗΝΩΝ"
          "ΚΓ΄ ΑΘΗΝΩΝ"
          "ΦΑΒΕ ΑΘΗΝΩΝ"
          "ΦΑΕΕ ΑΘΗΝΩΝ"
          "ΚΑΤΟΙΚΩΝ ΕΞΩΤΕΡΙΚΟΥ"
          "ΑΓΙΑΣ ΠΑΡΑΣΚΕΥΗΣ"
          "ΑΓΙΟΥ ΔΗΜΗΤΡΙΟΥ"
          "ΑΓΙΟΥ ΣΤΕΦΑΝΟΥ"
          "ΑΓΙΩΝ ΑΝΑΡΓΥΡΩΝ"
          "ΑΙΓΑΛΕΩ"
          "ΑΜΑΡΟΥΣΙΟΥ"
          "ΆΝΩ ΛΙΟΣΙΩΝ"
          "ΑΡΓΥΡΟΥΠΟΛΗΣ"
          "ΑΧΑΡΝΩΝ"
          "ΒΥΡΩΝΑ"
          "ΓΑΛΑΤΣΙΟΥ"
          "ΓΛΥΦΑΔΑΣ"
          "ΔΑΦΝΗΣ"
          "ΕΛΕΥΣΙΝΑΣ"
          "ΖΩΓΡΑΦΟΥ"
          "ΗΛΙΟΥΠΟΛΗΣ"
          "ΙΛΙΟΥ"
          "Α΄ ΚΑΛΛΙΘΕΑΣ"
          "Β΄ ΚΑΛΛΙΘΕΑΣ"
          "ΚΗΦΙΣΙΑΣ"
          "ΚΟΡΥΔΑΛΛΟΥ"
          "ΚΟΡΩΠΙΟΥ"
          "ΜΟΣΧΑΤΟΥ"
          "Ν. ΙΩΝΙΑΣ"
          "Ν. ΣΜΥΡΝΗΣ"
          "Ν. ΦΙΛΑΔΕΛΦΕΙΑΣ"
          "Ν. ΗΡΑΚΛΕΙΟΥ"
          "ΝΙΚΑΙΑΣ"
          "ΠΑΛ. ΦΑΛΗΡΟΥ"
          "ΠΑΛΛΗΝΗΣ"
          "Α΄ ΠΕΙΡΑΙΑ"
          "Β΄ ΠΕΙΡΑΙΑ"
          "Γ΄ ΠΕΙΡΑΙΑ"
          "Δ΄ ΠΕΙΡΑΙΑ"
          "Ε΄ ΠΕΙΡΑΙΑ"
          "ΣΤ΄ ΠΕΙΡΑΙΑ"
          "ΦΑΕ ΠΕΙΡΑΙΑ"
          "ΠΛΟΙΩΝ ΠΕΙΡΑΙΑ"
          "Α΄ ΠΕΡΙΣΤΕΡΙΟΥ"
          "Β΄ ΠΕΡΙΣΤΕΡΙΟΥ"
          "ΠΕΤΡΟΥΠΟΛΗΣ"
          "ΧΑΪΔΑΡΙΟΥ"
          "ΧΑΛΑΝΔΡΙΟΥ"
          "ΧΟΛΑΡΓΟΥ"
          "ΨΥΧΙΚΟΥ"
          "ΑΓΙΑΣ"
          "ΑΓ. ΑΘΑΝΑΣΙΟΥ"
          "ΑΓΙΟΥ ΚΗΡΥΚΟΥ ΙΚΑΡΙΑΣ"
          "ΑΓΙΟΥ ΝΙΚΟΛΑΟΥ"
          "ΑΓΡΙΝΙΟΥ"
          "ΑΙΓΙΝΑΣ"
          "ΑΙΓΙΝΙΟΥ"
          "ΑΙΓΙΟΥ"
          "ΑΚΡΑΤΑΣ"
          "ΑΛΕΞΑΝΔΡΕΙΑΣ"
          "ΑΛΕΞΑΝΔΡΟΥΠΟΛΗΣ"
          "ΑΛΜΥΡΟΥ"
          "ΑΜΑΛΙΑΔΑΣ"
          "ΑΜΠΕΛΟΚΗΠΩΝ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "ΑΜΥΝΤΑΙΟΥ"
          "ΑΜΦΙΚΛΕΙΑΣ"
          "ΑΜΦΙΛΟΧΙΑΣ"
          "ΆΜΦΙΣΣΑΣ"
          "ΑΝΔΡΙΤΣΑΙΝΑΣ"
          "ΑΡΓΟΣΤΟΛΙΟΥ"
          "ΆΡΓΟΥΣ"
          "ΆΡΓΟΥΣ ΟΡΕΣΤΙΚΟΥ"
          "ΑΡΕΟΠΟΛΗΣ"
          "ΑΡΙΔΑΙΑΣ"
          "ΑΡΚΑΛΟΧΩΡΙΟΥ"
          "ΑΡΝΑΙΑΣ"
          "ΆΡΤΑΣ"
          "ΑΣΤΑΚΟΥ"
          "ΑΤΑΛΑΝΤΗΣ"
          "ΒΑΡΔΑΣ"
          "ΒΕΡΟΙΑΣ"
          "Α΄ΒΟΛΟΥ"
          "Β΄ΒΟΛΟΥ"
          "ΒΟΝΙΤΣΑΣ"
          "ΓΑΡΓΑΛΙΑΝΩΝ"
          "ΓΑΣΤΟΥΝΗΣ"
          "ΓΙΑΝΝΙΤΣΩΝ"
          "ΓΟΥΜΕΝΙΣΣΑΣ"
          "ΓΡΕΒΕΝΩΝ"
          "ΓΥΘΕΙΟΥ"
          "ΔΕΛΒΙΝΑΚΙΟΥ"
          "ΔΕΡΒΕΝΙΟΥ"
          "ΔΕΣΚΑΤΗΣ"
          "ΔΗΜΗΤΣΑΝΑΣ"
          "ΔΙΔΥΜΟΤΕΙΧΟΥ"
          "ΔΟΜΟΚΟΥ"
          "ΔΡΑΜΑΣ"
          "ΈΔΕΣΣΑΣ"
          "ΕΛΑΣΣΟΝΑΣ"
          "ΕΛΕΥΘΕΡΟΥΠΟΛΗΣ"
          "ΖΑΓΚΛΙΒΕΡΙΟΥ"
          "ΖΑΚΥΝΘΟΥ"
          "ΖΑΧΑΡΩΣ"
          "ΗΓΟΥΜΕΝΙΤΣΑΣ"
          "ΗΡΑΚΛΕΙΑΣ"
          "Α΄ ΗΡΑΚΛΕΙΟΥ"
          "Β΄ ΗΡΑΚΛΕΙΟΥ"
          "ΘΑΣΟΥ"
          "ΘΕΡΜΟΥ"
          "Α΄ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Β΄ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Γ΄ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Δ΄ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Ε΄ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "ΣΤ΄ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Ζ΄ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Η΄ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Θ΄ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Ι΄ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "ΦΑΕ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "ΘΗΒΑΣ"
          "ΘΗΡΑΣ"
          "ΙΕΡΑΠΕΤΡΑΣ"
          "ΙΘΑΚΗΣ"
          "ΙΣΤΙΑΙΑΣ"
          "Α΄ ΙΩΑΝΝΙΝΩΝ"
          "Β΄ ΙΩΑΝΝΙΝΩΝ"
          "ΙΩΝΙΑΣ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Α΄ ΚΑΒΑΛΑΣ"
          "Β΄ ΚΑΒΑΛΑΣ"
          "ΚΑΛΑΒΡΥΤΩΝ"
          "ΚΑΛΑΜΑΡΙΑΣ"
          "ΚΑΛΑΜΑΤΑΣ"
          "ΚΑΛΑΜΠΑΚΑΣ"
          "ΚΑΛΛΟΝΗΣ"
          "ΚΑΛΥΜΝΟΥ"
          "ΚΑΡΔΙΤΣΑΣ"
          "ΚΑΡΛΟΒΑΣΙΟΥ"
          "ΚΑΡΠΑΘΟΥ"
          "ΚΑΡΠΕΝΗΣΙΟΥ"
          "ΚΑΡΥΣΤΟΥ"
          "ΚΑΣΣΑΝΔΡΑΣ"
          " ΚΙΣΣΑΜΟΥ"
          "ΚΑΣΤΕΛΙΟΥ ΠΕΔΙΑΔΟΣ"
          "ΚΑΣΤΟΡΙΑΣ"
          "Α΄ ΚΑΤΕΡΙΝΗΣ"
          "Β΄ ΚΑΤΕΡΙΝΗΣ"
          "ΚΑΤΩ ΑΧΑΙΑΣ"
          "ΚΕΑΣ"
          "Α΄ ΚΕΡΚΥΡΑΣ"
          "Β΄ ΚΕΡΚΥΡΑΣ"
          "ΚΙΑΤΟΥ"
          "ΚΙΛΚΙΣ"
          "ΚΛΕΙΤΟΡΙΑΣ"
          "ΚΟΖΑΝΗΣ"
          "ΚΟΜΟΤΗΝΗΣ"
          "ΚΟΝΙΤΣΑΣ"
          "ΚΟΡΙΝΘΟΥ"
          "ΚΡΑΝΙΔΙΟΥ"
          "ΚΡΕΣΤΕΝΩΝ"
          "ΚΡΟΚΕΩΝ"
          "ΚΥΘΗΡΩΝ"
          "ΚΥΜΗΣ"
          "ΚΥΠΑΡΙΣΣΙΑΣ"
          "ΚΩ"
          "ΛΑΓΚΑΔΑ"
          "ΛΑΜΙΑΣ"
          "Α΄ ΛΑΡΙΣΑΣ"
          "Β΄ ΛΑΡΙΣΑΣ"
          "Γ΄ ΛΑΡΙΣΑΣ"
          "ΛΑΥΡΙΟΥ"
          "ΛΕΡΟΥ"
          "ΛΕΥΚΑΔΑΣ"
          "ΛΕΧΑΙΝΩΝ"
          "ΛΕΩΝΙΔΙΟΥ"
          "ΛΗΜΝΟΥ"
          "ΛΗΞΟΥΡΙΟΥ"
          "ΛΙΒΑΔΕΙΑΣ"
          "ΛΙΔΟΡΙΚΙΟΥ"
          "ΛΙΜΕΝΟΣ ΧΕΡΣΟΝΗΣΟΥ"
          "ΛΙΜΝΗΣ"
          "ΜΑΚΡΑΚΩΜΗΣ"
          "ΜΕΓΑΛΟΠΟΛΗΣ"
          "ΜΕΓΑΡΩΝ"
          "ΜΕΛΙΓΑΛΑ"
          "ΜΕΣΟΛΟΓΓΙΟΥ"
          "ΜΕΣΣΗΝΗΣ"
          "ΜΕΤΣΟΒΟΥ"
          "ΜΗΘΥΜΝΑΣ"
          "ΜΗΛΟΥ"
          "ΜΟΙΡΩΝ"
          "ΜΟΛΑΩΝ"
          "ΜΟΥΖΑΚΙΟΥ"
          "ΜΥΚΟΝΟΥ"
          "ΜΥΤΙΛΗΝΗΣ"
          "ΝΑΞΟΥ"
          "ΝΑΟΥΣΑΣ"
          "ΝΑΥΠΑΚΤΟΥ"
          "ΝΑΥΠΛΙΟΥ"
          "ΝΕΑΠΟΛΗΣ (ΚΡΗΤΗΣ)"
          "ΝΕΑΠΟΛΗΣ ΒΟΙΟΥ (ΚΟΖΑΝΗΣ)"
          "ΝΕΑΠΟΛΗΣ ΒΟΙΩΝ (ΛΑΚΩΝΙΑΣ)"
          "ΝΕΑΠΟΛΗΣ (ΘΕΣΣΑΛΟΝΙΚΗΣ)"
          "ΝΕΑΣ ΖΙΧΝΗΣ"
          "Ν. ΙΩΝΙΑΣ ΜΑΓΝΗΣΙΑΣ"
          "ΝΕΜΕΑΣ"
          "ΝΕΣΤΟΡΙΟΥ"
          "ΝΕΥΡΟΚΟΠΙΟΥ"
          "ΝΕΩΝ ΜΟΥΔΑΝΙΩΝ"
          "ΝΙΓΡΙΤΑΣ"
          "Α΄ ΞΑΝΘΗΣ"
          "Β΄ ΞΑΝΘΗΣ"
          "ΞΥΛΟΚΑΣΤΡΟΥ"
          "ΟΡΕΣΤΙΑΔΑΣ"
          "ΠΑΛΑΜΑ"
          "ΠΑΞΩΝ"
          "ΠΑΡΑΛΙΟΥ ΆΣΤΡΟΥΣ"
          "ΠΑΡΑΜΥΘΙΑΣ"
          "ΠΑΡΓΑΣ"
          "ΠΑΡΟΥ"
          "Α΄ ΠΑΤΡΩΝ"
          "Β΄ ΠΑΤΡΩΝ"
          "Γ΄ ΠΑΤΡΩΝ"
          "ΠΛΩΜΑΡΙΟΥ"
          "ΠΟΛΥΓΥΡΟΥ"
          "ΠΟΡΟΥ"
          "ΠΡΕΒΕΖΑΣ"
          "ΠΤΟΛΕΜΑΙΔΑΣ"
          "ΠΥΛΗΣ"
          "ΠΥΛΟΥ"
          "ΠΥΡΓΟΥ"
          "ΡΕΘΥΜΝΟΥ"
          "ΡΟΔΟΥ"
          "ΣΑΛΑΜΙΝΑΣ"
          "ΣΑΜΟΥ"
          "ΣΑΠΩΝ"
          "ΣΕΡΒΙΩΝ"
          "Α΄ ΣΕΡΡΩΝ"
          "Β΄ ΣΕΡΡΩΝ"
          "ΣΗΤΕΙΑΣ"
          "ΣΙΑΤΙΣΤΑΣ"
          "ΣΙΔΗΡΟΚΑΣΤΡΟΥ"
          "ΣΚΑΛΑΣ ΛΑΚΩΝΙΑΣ"
          "ΣΚΙΑΘΟΥ"
          "ΣΚΟΠΕΛΟΥ"
          "ΣΚΥΔΡΑΣ"
          "ΣΟΥΦΛΙΟΥ"
          "ΣΟΦΑΔΩΝ"
          "ΣΟΧΟΥ"
          "ΣΠΑΡΤΗΣ"
          "ΣΠΕΤΣΩΝ"
          "ΣΤΥΛΙΔΑΣ"
          "ΣΥΡΟΥ"
          "ΤΗΝΟΥ"
          "ΤΟΥΜΠΑΣ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "ΤΡΙΚΑΛΩΝ"
          "ΤΡΙΠΟΛΗΣ"
          "ΤΡΟΠΑΙΩΝ"
          "ΤΥΜΠΑΚΙΟΥ"
          "ΤΥΡΝΑΒΟΥ"
          "ΎΔΡΑΣ"
          "ΦΑΡΚΑΔΟΝΑΣ"
          "ΦΑΡΣΑΛΩΝ"
          "ΦΙΛΙΑΤΡΩΝ"
          "ΦΙΛΙΑΤΩΝ"
          "ΦΙΛΙΠΠΙΑΔΑΣ"
          "ΦΛΩΡΙΝΑΣ"
          "ΧΑΛΚΙΔΑΣ"
          "Α΄ ΧΑΝΙΩΝ"
          "Β΄ ΧΑΝΙΩΝ"
          "ΧΙΟΥ"
          "ΧΡΥΣΟΥΠΟΛΗΣ"))
  nil)



(defun seed-companies ()
  (mapc (lambda (rec)
          (let ((list (iter (for field-name
                                 in '(:title :occupation :tof-id :tin :address :city :pobox :zipcode))
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

(defun seed-accounts ()
  (with-db ()
    (let ((debit-accounts
           '((assets "Ενεργητικό"
              ((current-assets "Κυκλοφορούν Ενεργητικό"
                               ((cash "Μετρητά" ())
                                (cheques-receivable "Επιταγές εισπρακτέες" ())
                                (receivables "Απαιτήσεις"
                                             ((accounts-receivable "Εισπρακτέοι λογαριασμοί" ())
                                              (bad-debts "Φέσια" ())))
                                (bank-checking-account "Λογαριασμός όψεως" ())
                                (supplies "Αναλώσιμα" ())
                                (raw-materials "Πρώτες ύλες" ())
                                (manufacturing-supplies "Εξοπλισμός παραγωγής" ())
                                (prepaid-expenses "Προπληρωμένα έξοδα" ())))
               (property "Περιουσιακά στοιχεία"
                         ((vehicles "Οχήματα" ())
                          (machinery "Εργαλεία και εργαλειομηχανές" ())
                          (equipment "Λοιπός εξοπλισμός" ())
                          (depreciation "Απαξίωση περιουσιακών στοιχείων" ())))))
             (expenses "Έξοδα" ((operating-expenses "Λειτουργικά έξοδα" ())
                                (manufacturing-expenses "Έξοδα παραγωγής" ())))))
          (credit-accounts '((liabilities "Παθητικό"
                              ((current-liabilities "Τρέχουσες Υποχρεώσεις" ())
                               (accounts-payable "foobar")))
                             (revenues "Έσοδα"
                              ((services "Υπηρεσίες" ())
                               (sales "Πωλήσεις" ())
                               (projects "Έργα" ())))))
          (*debitp* nil))
      (declare (special *debitp*))
      (labels ((create-accounts (account-list parent-id)
                 (let (list)
                   (mapc (lambda (acc)
                           (let* ((db-id (if (eql (first acc) :null)
                                             :null
                                             (string-downcase (first acc))))
                                  (db-pid (if (eql parent-id :null)
                                              :null
                                              (string-downcase parent-id)))
                                  (inst (make-instance 'account
                                                       :id db-id
                                                       :title (second acc)
                                                       :parent-id db-pid
                                                       :debit-p *debitp*)))
                             (insert-dao inst)
                             (push inst list)
                             (create-accounts (third acc) db-id)))
                         account-list)
                   list)))
        (setf *debitp* nil)
        (create-accounts credit-accounts :null)
        (setf *debitp* t)
        (create-accounts debit-accounts :null)))))

(defun seed-all ()
  (seed-banks)
  (seed-tofs)
  (seed-companies)
  (seed-accounts))
