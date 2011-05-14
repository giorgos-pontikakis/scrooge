(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Default table values (only for development)
;;; ----------------------------------------------------------------------

(defun seed-usr ()
  (with-db ()
    (mapc (lambda (username password authgroup)
            (ignore-errors
              (save-dao
               (make-instance 'usr
                              :username username
                              :password (md5sum-sequence->string password)
                              :authgroup authgroup))))
          '("gnp" "litsa" "admin")
          '("gnp" "litsa" "admin")
          '("configuration" "configuration" "configuration"))))

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
          "Α' ΑΘΗΝΩΝ"
          "Β' ΑΘΗΝΩΝ"
          "Γ' ΑΘΗΝΩΝ"
          "Δ' ΑΘΗΝΩΝ"
          "Ε' ΑΘΗΝΩΝ"
          "ΣΤ' ΑΘΗΝΩΝ"
          "Ζ' ΑΘΗΝΩΝ"
          "Η' ΑΘΗΝΩΝ"
          "Θ' ΑΘΗΝΩΝ"
          "Ι' ΑΘΗΝΩΝ"
          "ΙΑ' ΑΘΗΝΩΝ"
          "ΙΒ' ΑΘΗΝΩΝ"
          "ΙΓ' ΑΘΗΝΩΝ"
          "ΙΔ' ΑΘΗΝΩΝ"
          "ΙΕ' ΑΘΗΝΩΝ"
          "ΙΣΤ' ΑΘΗΝΩΝ"
          "ΙΖ' ΑΘΗΝΩΝ"
          "ΙΗ' ΑΘΗΝΩΝ"
          "ΙΘ' ΑΘΗΝΩΝ"
          "Κ' ΑΘΗΝΩΝ"
          "ΚΑ' ΑΘΗΝΩΝ"
          "ΚΒ' ΑΘΗΝΩΝ"
          "ΚΓ' ΑΘΗΝΩΝ"
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
          "Α' ΚΑΛΛΙΘΕΑΣ"
          "Β' ΚΑΛΛΙΘΕΑΣ"
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
          "Α' ΠΕΙΡΑΙΑ"
          "Β' ΠΕΙΡΑΙΑ"
          "Γ' ΠΕΙΡΑΙΑ"
          "Δ' ΠΕΙΡΑΙΑ"
          "Ε' ΠΕΙΡΑΙΑ"
          "ΣΤ' ΠΕΙΡΑΙΑ"
          "ΦΑΕ ΠΕΙΡΑΙΑ"
          "ΠΛΟΙΩΝ ΠΕΙΡΑΙΑ"
          "Α' ΠΕΡΙΣΤΕΡΙΟΥ"
          "Β' ΠΕΡΙΣΤΕΡΙΟΥ"
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
          "Α'ΒΟΛΟΥ"
          "Β'ΒΟΛΟΥ"
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
          "Α' ΗΡΑΚΛΕΙΟΥ"
          "Β' ΗΡΑΚΛΕΙΟΥ"
          "ΘΑΣΟΥ"
          "ΘΕΡΜΟΥ"
          "Α' ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Β' ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Γ' ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Δ' ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Ε' ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "ΣΤ' ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Ζ' ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Η' ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Θ' ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Ι' ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "ΦΑΕ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "ΘΗΒΑΣ"
          "ΘΗΡΑΣ"
          "ΙΕΡΑΠΕΤΡΑΣ"
          "ΙΘΑΚΗΣ"
          "ΙΣΤΙΑΙΑΣ"
          "Α' ΙΩΑΝΝΙΝΩΝ"
          "Β' ΙΩΑΝΝΙΝΩΝ"
          "ΙΩΝΙΑΣ ΘΕΣΣΑΛΟΝΙΚΗΣ"
          "Α' ΚΑΒΑΛΑΣ"
          "Β' ΚΑΒΑΛΑΣ"
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
          "Α' ΚΑΤΕΡΙΝΗΣ"
          "Β' ΚΑΤΕΡΙΝΗΣ"
          "ΚΑΤΩ ΑΧΑΙΑΣ"
          "ΚΕΑΣ"
          "Α' ΚΕΡΚΥΡΑΣ"
          "Β' ΚΕΡΚΥΡΑΣ"
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
          "Α' ΛΑΡΙΣΑΣ"
          "Β' ΛΑΡΙΣΑΣ"
          "Γ' ΛΑΡΙΣΑΣ"
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
          "Α' ΞΑΝΘΗΣ"
          "Β' ΞΑΝΘΗΣ"
          "ΞΥΛΟΚΑΣΤΡΟΥ"
          "ΟΡΕΣΤΙΑΔΑΣ"
          "ΠΑΛΑΜΑ"
          "ΠΑΞΩΝ"
          "ΠΑΡΑΛΙΟΥ ΆΣΤΡΟΥΣ"
          "ΠΑΡΑΜΥΘΙΑΣ"
          "ΠΑΡΓΑΣ"
          "ΠΑΡΟΥ"
          "Α' ΠΑΤΡΩΝ"
          "Β' ΠΑΤΡΩΝ"
          "Γ' ΠΑΤΡΩΝ"
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
          "Α' ΣΕΡΡΩΝ"
          "Β' ΣΕΡΡΩΝ"
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
          "ΥΔΡΑΣ"
          "ΦΑΡΚΑΔΟΝΑΣ"
          "ΦΑΡΣΑΛΩΝ"
          "ΦΙΛΙΑΤΡΩΝ"
          "ΦΙΛΙΑΤΩΝ"
          "ΦΙΛΙΠΠΙΑΔΑΣ"
          "ΦΛΩΡΙΝΑΣ"
          "ΧΑΛΚΙΔΑΣ"
          "Α' ΧΑΝΙΩΝ"
          "Β' ΧΑΝΙΩΝ"
          "ΧΙΟΥ"
          "ΧΡΥΣΟΥΠΟΛΗΣ"))
  nil)



(defun seed-companies ()
  (mapc (lambda (rec)
          (let ((list (iter (for field-name
                                 in '(:title :occupation :tof-id :tin
                                      :address :city :pobox :zipcode))
                            (for field-value in rec)
                            (collect field-name)
                            (collect field-value))))
            (apply #'create-row 'company list)))
        '(("Ε. Καφφές Α.Ε."
           "Εμπορία"
           1
           "094307730"
           "Προέκταση Σμύρνης"
           3
           :null
           :null)
          ("Κωνσταντίνα Γαβριήλ"
           "Εμπορία"
           2
           "063565898"
           "Λευκίππου 1Α"
           7
           :null
           :null)
          ("Κλιμαμηχανική Α.Ε."
           "Κλιματισμός"
           1
           "095706159"
           "Βενιζέλου 5, Καλαμαριά"
           9
           :null
           :null))))



(defun seed-accounts ()
  (with-db ()
    (let ((debit-accounts '((debit-accounts-root "debit-accounts-root"
                             ((assets "Ενεργητικό"
                                      ((current-assets "Κυκλοφορούν Ενεργητικό"
                                                       ((cash "Μετρητά" ())
                                                        (accounts-receivable "Τρέχουσες απαιτήσεις (Ανοιχτοί λογαριασμοί)" ())
                                                        (cheques-receivable "Επιταγές εισπρακτέες" ())
                                                        (bank-checking-account "Τραπεζικός λογαριασμός όψεως" ())
                                                        (bank-deposit-account "Τραπεζικός λογαριασμός καταθέσεων" ())))
                                       (property "Περιουσιακά στοιχεία"
                                                 ((vehicles "Οχήματα" ())
                                                  (machinery "Εργαλεία και εργαλειομηχανές" ())
                                                  (equipment "Λοιπός εξοπλισμός" ())))))
                              (expenses "Έξοδα"
                                        ((operating-expenses "Λειτουργικά έξοδα"
                                                             ((personnel "Προσωπικό"
                                                                         ((salaries "Μισθοί" ())
                                                                          (travel "Εκτός έδρας" ())
                                                                          (bonuses "Πριμ" ())
                                                                          (insuranse "Ασφαλιστικά ταμεία" ())
                                                                          (severances "Αποζημιώσεις" ())))
                                                              (misc-expenses "Γενικά Έξοδα"
                                                                             ((supplies "Αναλώσιμα" ())
                                                                              (utilities "Επιχειρήσεις κοινής ωφέλειας"
                                                                                         ((electricity "Δ.Ε.Η." ())
                                                                                          (phones "Τηλέφωνα και Διαδίκτυο" ())
                                                                                          (mail "Ταχυδρομείο και ταχυμεταφορές" ())))
                                                                              (auxiliary-services "Βοηθητικές υπηρεσίες"
                                                                                                  ((lawyer "Δικηγόρος" ())
                                                                                                   (accountant "Λογιστής" ())))))
                                                              (depreciation "Απαξίωση περιουσιακών στοιχείων" ())
                                                              (bad-debts "Διεγραμμένες ανείσπρακτες οφειλές" ())))
                                         (manufacturing-expenses "Έξοδα παραγωγής"
                                                                 ((raw-material "Πρώτες ύλες" ())
                                                                  (machinery "Μηχανήματα" ())
                                                                  (installation "Έξοδα εγκαταστάσεων"
                                                                                ((fuel "Καύσιμα" ())
                                                                                 (tolls "Διόδια" ())
                                                                                 (external-services "Εξωτερικοί συνεργάτες" ()))))
                                                                 (other-manufacturing-expenses "Διάφορα έξοδα παραγωγής" ()))
                                         (non-operating-expenses "Μή λειτουργικά έξοδα"
                                                                 ((various-non-op-expenses "Διάφορα μη λειτουργικά έξοδα" ())
                                                                  (taxes "Φόροι" ())
                                                                  (vat "Φ.Π.Α." ())
                                                                  (fines "Πρόστιμα" ())))))))))
          (credit-accounts '((credit-accounts-root "credit-accounts-root"
                              ((liabilities "Παθητικό"
                                            ((current-liabilities "Τρέχουσες Υποχρεώσεις" ())
                                             (cheques-payable "Επιταγές πληρωτέες" ())
                                             (taxes-payable "Φόροι πληρωτέοι" ())))
                               (revenues "Έσοδα" ((supplied-services "Παροχή υπηρεσιών" ())
                                                  (products "Προϊόντα" ())
                                                  (projects "Έργα" ())))))))
          (*debitp* nil))
      (declare (special *debitp*))
      (labels ((create-accounts (account-list parent-id)
                 (let (list
                       (rank 1))
                   (mapc (lambda (acc)
                           (let ((inst (make-instance 'account
                                                       :title (second acc)
                                                       :parent-id parent-id
                                                       :debit-p *debitp*
                                                       :rank rank)))
                             (insert-dao inst)
                             (push inst list)
                             (incf rank)
                             (create-accounts (third acc)
                                              (id (select-dao-unique 'account (:= 'title
                                                                                  (second acc)))))))
                         account-list)
                   list)))
        (setf *debitp* nil)
        (create-accounts credit-accounts :null)
        (setf *debitp* t)
        (create-accounts debit-accounts :null)))))


(defparameter *cities* '("Αθήνα"
                         "Θεσσαλονίκη"
                         "Πειραιάς"
                         "Πάτρα"
                         "Περιστέρι"
                         "Ηράκλειο"
                         "Λάρισα"
                         "Καλλιθέα"
                         "Νίκαια"
                         "Καλαμαριά"
                         "Βόλος"
                         "Ίλιον"
                         "Γλυφάδα"
                         "Ζωγράφου"
                         "Κερατσίνι"
                         "Ηλιούπολη"
                         "Αχαρνές"
                         "Αιγάλεω"
                         "Νέα Σμύρνη"
                         "Χαλάνδρι"
                         "Μαρούσι"
                         "Κορυδαλλός"
                         "Νέα Ιωνία"
                         "Άγιος Δημήτριος"
                         "Παλαιό Φάληρο"
                         "Ιωάννινα"
                         "Βύρωνας"
                         "Καβάλα"
                         "Γαλάτσι"
                         "Αγία Παρασκευή"
                         "Σέρρες"
                         "Χαλκίδα"
                         "Χανιά"
                         "Εύοσμος"
                         "Ρόδος"
                         "Κατερίνη"
                         "Καλαμάτα"
                         "Αλεξανδρούπολη"
                         "Τρίκαλα"
                         "Πετρούπολη"
                         "Λαμία"
                         "Ηράκλειο Αττικής"
                         "Χαϊδάρι"
                         "Ξάνθη"
                         "Κηφισιά"
                         "Κομοτηνή"
                         "Βέροια"
                         "Δράμα"
                         "Αγρίνιο"
                         "Συκιές"
                         "Σταυρούπολη"
                         "Αμπελόκηποι Θεσσαλονίκης"
                         "Άλιμος(Καλαμάκι)"
                         "Πολίχνη Θεσσαλονίκης"
                         "Κοζάνη"
                         "Αργυρούπολη"
                         "Άγιοι Ανάργυροι Αττικής"
                         "Χολαργός"
                         "Καρδίτσα"
                         "Νέα Ιωνία Μαγνησίας"
                         "Αγία Βαρβάρα"
                         "Νεάπολη"
                         "Κόρινθος"
                         "Πτολεμαΐδα"
                         "Κέρκυρα"
                         "Ρέθυμνο"
                         "Ασπρόπυργος"
                         "Μυτιλήνη"
                         "Μεταμόρφωση Αττικής"
                         "Άνω Λιόσια"
                         "Καισαριανή"
                         "Γιαννιτσά"
                         "Ελευσίνα"
                         "Σαλαμίνα"
                         "Πέραμα"
                         "Βριλήσσια"
                         "Βούλα"
                         "Τρίπολη"
                         "Άργος"
                         "Νέα Φιλαδέλφεια"
                         "Χίος"
                         "Δάφνη"
                         "Πύργος"
                         "Μοσχάτο"
                         "Μέγαρα"
                         "Πυλαία"
                         "Καματερό"
                         "Ελευθέριο - Κορδελιό"
                         "Θήβα"
                         "Αίγιο"
                         "Λιβαδειά"
                         "Πεύκη"
                         "Νάουσα"
                         "Μελίσσια"
                         "Άρτα"
                         "Αμαλιάδα"
                         "Έδεσσα"
                         "Κως"
                         "Κιλκίς"
                         "Αρτέμιδα"
                         "Ελληνικό"
                         "Πρέβεζα"
                         "Κορωπί"
                         "Νέα Ερυθραία"
                         "Ορεστιάδα"
                         "Άγιος Ιωάννης Ρέντης"
                         "Ταύρος"
                         "Μενεμένη"
                         "Σπάρτη"
                         "Καστοριά"
                         "Πανόραμα"
                         "Φλώρινα"
                         "Νέα Μάκρη"
                         "Γέρακας"
                         "Ναύπλιο"
                         "Περαία"
                         "Αλεξάνδρεια"
                         "Παπάγος"
                         "Δραπετσώνα"
                         "Ναύπακτος"
                         "Παιανία"
                         "Παλλήνη"
                         "Μεσολόγγι"
                         "Καλύβια Θορικού"
                         "Ωραιόκαστρο"
                         "Ερμούπολη"
                         "Ιεράπετρα"
                         "Νέα Αλικαρνασσός"
                         "Λουτράκι"
                         "Θέρμη"
                         "Ραφήνα"
                         "Τριανδρία"
                         "Ζάκυνθος"
                         "Υμηττός"
                         "Τύρναβος"
                         "Βάρη"
                         "Μάνδρα"
                         "Παλαιό Ψυχικό"
                         "Νέο Ψυχικό"
                         "Γρεβενά"
                         "Κάλυμνος"
                         "Νέα Χαλκηδόνα"
                         "Ιαλυσός"
                         "Άγιος Νικόλαος"))

(defparameter *subcities* '("Αγιά"
                            "Αγία Βαρβάρα"
                            "Αγία Μαρίνα Ανατολικής Αττικής"
                            "Αγία Μαρίνα Δωδεκανήσου"
                            "Αγία Παρασκευή"
                            "Αγία Τριάδα"
                            "Αγιάσος"
                            "Άγιοι Απόστολοι"
                            "Άγιοι Θεόδωροι"
                            "Άγιος Αθανάσιος Δράμας"
                            "Άγιος Αθανάσιος Θεσσαλονίκης"
                            "Άγιος Βασίλειος"
                            "Άγιος Γεώργιος"
                            "Άγιος Κωνσταντίνος Αιτωλοακαρνανίας"
                            "Άγιος Κωνσταντίνος Φθιώτιδας"
                            "Άγιος Νικόλαος"
                            "Άγιος Πάυλος"
                            "Άγιος Στέφανος"
                            "Αγριά"
                            "Άδενδρο"
                            "Αθίκια"
                            "Αιανή"
                            "Αιάντειο"
                            "Αίγινα"
                            "Αιγίνιο"
                            "Αιτωλικό"
                            "Αλίαρτος"
                            "Αλιβέρι"
                            "Αλιστράτη"
                            "Αλμυρός"
                            "Αμάρυνθος"
                            "Αμπελάκια"
                            "Αμπελώνας"
                            "Αμύνταιο"
                            "Αμφίκλεια"
                            "Αμφιλοχία"
                            "Άμφισσα"
                            "Ανάβυσσος"
                            "Ανατολή"
                            "Ανατολικό"
                            "Ανδραβίδα"
                            "Ανθούσα"
                            "Άνοιξη"
                            "Αντικύρα"
                            "Αντιμάχεια"
                            "Ανώγεια"
                            "Αξιούπολη"
                            "Αράχωβα"
                            "Άργος Ορεστικό"
                            "Αργοστόλι"
                            "Αριδαία"
                            "Αρκαλοχώρι"
                            "Αρναία"
                            "Αρχάγγελος"
                            "Ασβεστοχώρι"
                            "Ασπροβάλτα"
                            "Άσσηρος"
                            "Άσσος"
                            "Αστακός"
                            "Άστρος"
                            "Αταλάντη"
                            "Αυλώνα"
                            "Αφάντου"
                            "Βάγια"
                            "Βαθύ Εύβοιας"
                            "Βαθύ Σάμου"
                            "Βαθύλακκος"
                            "Βάρδα"
                            "Βαρθολομιό"
                            "Βασιλικά Εύβοιας"
                            "Βασιλικά Θεσσαλονίκης"
                            "Βελβεντός"
                            "Βελεστίνο"
                            "Βέλο"
                            "Βλαχιώτης"
                            "Βόνιτσα"
                            "Βουλιαγμένη"
                            "Βραχάτι"
                            "Βραχναίικα"
                            "Βροντάδος"
                            "Βροντού"
                            "Γάζι"
                            "Γαλατάδες"
                            "Γαλατάς Πειραιά"
                            "Γαλατάς Χανίων"
                            "Γαλατινή"
                            "Γαλάτιστα"
                            "Γαργαλιάνοι"
                            "Γαστούνη"
                            "Γέφυρα"
                            "Γιάννουλη"
                            "Γλυκά Νερά"
                            "Γόννοι"
                            "Γουμένισσα"
                            "Γύθειο"
                            "Γυμνό"
                            "Δαράτσος"
                            "Δελφοί"
                            "Δεμένικα"
                            "Δεσκάτη"
                            "Δεσφίνα"
                            "Δήλεσι"
                            "Διαβατά"
                            "Διακοπτό"
                            "Διδυμότειχο"
                            "Διμήνιο"
                            "Διόνυσος"
                            "Δίστομο"
                            "Δοξάτο"
                            "Δροσιά Ανατολικής Αττικής"
                            "Δροσιά Εύβοιας"
                            "Δρυμός"
                            "Εκάλη"
                            "Ελασσώνα"
                            "Ελάτεια"
                            "Ελεούσα"
                            "Ελευθερούπολη"
                            "Επανομή"
                            "Επάνω Αρχάνες"
                            "Ερέτρια"
                            "Ερμιόνη"
                            "Ερυθρές"
                            "Ευκαρπία"
                            "Ευξεινούπολη"
                            "Ευρωπός"
                            "Εχίνος"
                            "Ζαγκλιβέρι"
                            "Ζαγορά"
                            "Ζαρός"
                            "Ζαχάρω"
                            "Ζευγολατειό"
                            "Ζεφύρι"
                            "Ζηπάριο"
                            "Ηγουμενίτσα"
                            "Ηράκλεια"
                            "Θήρα"
                            "Θρακομακεδόνες"
                            "Ίασμος"
                            "Ιερισσός"
                            "Ιστιαία"
                            "Ιτέα"
                            "Καινούργιο"
                            "Καλαμπάκα"
                            "Καλαμπάκι"
                            "Καλλιθέα"
                            "Καλοχώρι"
                            "Καλυθιές"
                            "Καμένα Βούρλα"
                            "Κανάλι"
                            "Καναλλάκι"
                            "Καπανδρίτι"
                            "Καρδιτσομαγούλα"
                            "Καρίτσα"
                            "Κάρπαθος"
                            "Καρπενήσι"
                            "Κάρυστος"
                            "Κασσάνδρεια"
                            "Κατούνα"
                            "Κατοχή"
                            "Κατσικάς"
                            "Κάτω Αχαΐα"
                            "Κάτω Νευροκόπι"
                            "Κάτω Τιθορέα"
                            "Κένταυρος"
                            "Κερατέα"
                            "Κέφαλος"
                            "Κιάτο"
                            "Κιμμέρια"
                            "Κίσσαμος"
                            "Κίτσι"
                            "Κολινδρός"
                            "Κομπότι"
                            "Κόνιτσα"
                            "Κοπανός"
                            "Κορινός"
                            "Κοσκινού"
                            "Κουνουπιδιανά"
                            "Κουφάλια"
                            "Κρανέα Ελασσόνας"
                            "Κρανίδι"
                            "Κρεμαστή"
                            "Κρέστενα"
                            "Κρηνίδες"
                            "Κρόκος"
                            "Κρουσώνας"
                            "Κρύα Βρύση"
                            "Κρυονέρι"
                            "Κύμη"
                            "Κύμινα"
                            "Κυπαρισσία"
                            "Κυριάκιο"
                            "Λαγκαδάς"
                            "Λαγυνά"
                            "Λαύριο"
                            "Λεοντάρι"
                            "Λεπενού"
                            "Λεπτοκαρυά"
                            "Λευκάδα"
                            "Λευκίμμη"
                            "Λευκώνας"
                            "Λεχαινά"
                            "Λέχαιο"
                            "Λεωνίδιο"
                            "Ληξούρι"
                            "Λητή"
                            "Λάρισας"
                            "Λιβανάτες"
                            "Λιμενάρια"
                            "Λιμένας Θάσου"
                            "Λιμένας Μαρκοπούλου"
                            "Λιμένας Χερσονήσου"
                            "Λίμνη"
                            "Λιτόχωρο"
                            "Λούρος"
                            "Λουτρά Αιδηψού"
                            "Λυγούριο"
                            "Λυκόβρυση"
                            "Μαγούλα"
                            "Μακρακώμη"
                            "Μακροχώρι"
                            "Μαλεσίνα"
                            "Μάλια"
                            "Μανιάκοι"
                            "Μαραθώνας"
                            "Μαρκόπουλο Μεσογαίας"
                            "Μαρκόπουλο Ωρωπού"
                            "Μαρτίνο"
                            "Μεγάλα Καλύβια"
                            "Μεγάλη Παναγία"
                            "Μεγαλόπολη"
                            "Μελίκη"
                            "Μεσσήνη"
                            "Μέτσοβο"
                            "Μοίρες"
                            "Μολάοι"
                            "Μουζάκι"
                            "Μουρνιές"
                            "Μύκονος"
                            "Μύρινα"
                            "Μυτιληνιοί"
                            "Μώλος"
                            "Νάξος"
                            "Νάουσα"
                            "Νέα Αγχίαλος"
                            "Νέα Αρτάκη"
                            "Νέα Βρασνά"
                            "Νέα Βύσσα"
                            "Νέα Ζίχνη"
                            "Νέα Καλλικράτεια"
                            "Νέα Καρβάλη"
                            "Νέα Κίος"
                            "Νέα Μαγνησία"
                            "Νέα Μάλγαρα"
                            "Νέα Μεσημβρία"
                            "Νέα Μηχανιώνα"
                            "Νέα Μουδανιά"
                            "Νέα Παλάτια"
                            "Νέα Πεντέλη"
                            "Νέα Πέραμος Δυτικής Αττικής"
                            "Νέα Πέραμος Καβάλας"
                            "Νέα Τρίγλια"
                            "Νεάπολη Κοζάνης"
                            "Νεάπολη Λασιθίου"
                            "Νεάπολη Βοιών"
                            "Νεμέα"
                            "Νέο Καρλοβάσι"
                            "Νέο Πετρίτσι"
                            "Νέο Ρύσιο"
                            "Νέο Σούλι"
                            "Νέοι Επιβάτες"
                            "Νέος Μαρμαράς"
                            "Νέος Μυλότοπος"
                            "Νέος Σκοπός"
                            "Νεοχώρι Αιτωλοακαρνανίας"
                            "Νεοχώρι Άρτας"
                            "Νεροκούρος"
                            "Νιγρίτα"
                            "Νίκαια"
                            "Νικήσιανη"
                            "Νικήτη"
                            "Ξηροπόταμος"
                            "Ξυλόκαστρο"
                            "Οβρυά"
                            "Οινόφυτα"
                            "Οιχαλία"
                            "Ορμύλια"
                            "Ορχομενός"
                            "Παλαιά Φώκαια"
                            "Παλαιοχώρα"
                            "Πάλαιρος"
                            "Παλαμάς"
                            "Παναιτώλιο"
                            "Παραδείσι"
                            "Παραλία"
                            "Παραλία Αυλίδας"
                            "Παραλία Βέργας"
                            "Παραμυθιά"
                            "Πάργα"
                            "Πάρος"
                            "Πέλλα"
                            "Πεντέλη"
                            "Πέραμα"
                            "Περιβόλια"
                            "Περίσταση"
                            "Πεύκα"
                            "Πλαγιάρι"
                            "Πλάκα Δήλεσι Βοιωτίας"
                            "Πλαταμώνας"
                            "Πλατύ"
                            "Πλωμάρι"
                            "Πολίχνιτος"
                            "Πολύγυρος"
                            "Πολύκαστρο"
                            "Πόρος"
                            "Ποταμός"
                            "Προσοτσάνη"
                            "Πυλίο"
                            "Πύλος"
                            "Ρίο"
                            "Ροδίτσα"
                            "Ροδοδάφνη"
                            "Ροδολίβος"
                            "Ροδόπολη"
                            "Σάμος"
                            "Σάπες"
                            "Σαρωνίδα"
                            "Σελήνια"
                            "Σέρβια"
                            "Σήμαντρα"
                            "Σητεία"
                            "Σιάτιστα"
                            "Σιδηρόκαστρο"
                            "Σίνδος"
                            "Σκάλα"
                            "Σκιάθος"
                            "Σκόπελος"
                            "Σκούταρι"
                            "Σκύδρα"
                            "Σούδα"
                            "Σούρπη"
                            "Σουφλί"
                            "Σοφάδες"
                            "Σοχός"
                            "Σπάτα"
                            "Σπερχειάδα"
                            "Σπέτσες"
                            "Σταμάτα"
                            "Σταυρός Θεσσαλονίκης"
                            "Σταυρός Φθιώτιδας"
                            "Στυλίδα"
                            "Συκιά"
                            "Συκούριο"
                            "Σύμη"
                            "Σχηματάρι"
                            "Τερπνή"
                            "Τήνος"
                            "Τραγανό"
                            "Τρίλοφος"
                            "Τσαρίτσανη"
                            "Τυμπάκι"
                            "Τυρός"
                            "Τυχερό"
                            "Ύδρα"
                            "Φάλαννα"
                            "Φαρκαδώνα"
                            "Φάρσαλα"
                            "Φέρες"
                            "Φιλιάτες"
                            "Φιλιατρά"
                            "Φιλιππιάδα"
                            "Φιλοθέη"
                            "Φίλυρο"
                            "Φυλή"
                            "Χαλάστρα"
                            "Χαλκηδών"
                            "Χαλκούτσι"
                            "Χορτιάτης"
                            "Χρυσό"
                            "Χρυσούπολη"
                            "Χώρα Μεσσηνίας"
                            "Χωρίο"
                            "Χωριστή"
                            "Ψαχνά"))

(defun seed-cities ()
  (mapc (lambda (arg)
          (ignore-errors
            (create-row 'city :title (string-upcase-gr arg))))
        (append *cities* *subcities*))
  nil)

(defun seed-all ()
  (seed-usr)
  (seed-banks)
  (seed-tofs)
  (seed-cities)
  (seed-companies)
  (seed-accounts))
