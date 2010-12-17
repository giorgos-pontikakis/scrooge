(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))


;;; --- Home --------------------

(define-dynamic-page home ("") ()
  (with-document ()
    (:head
     (:title "Αρχική")
     (head-css-std))
    (:body
     (:div :id "header"
           (logo)
           (primary-navbar 'home))
     (:div :id "body"
           (:p "Home content not yet available")))))



;;; --- Autocomplete --------------------

;; (define-dynamic-page autocomplete ((table symbol)
;;                                 (column symbol)
;;                                 (term string))
;;     ("autocomplete" :content-type "text/plain")
;;   (with-db ()
;;     (with-parameter-rebinding #'val
;;       (let ((results (query (:select column :from table
;;                                   :where (:like column (concatenate 'string
;;                                                                     "%" term "%")))
;;                          :column)))
;;      (if results
;;          (with-html-output (*standard-output* nil :indent nil :prologue nil)
;;            (write-json (map 'vector #'identity results)))
;;          (with-html-output (*standard-output* nil :indent nil :prologue nil)
;;            "[]"))))))



;;; --- Error pages --------------------


;;; :TODO: This should be a static page

(define-dynamic-page notfound ("notfound") ()
  (no-cache)
  (with-document ()
    (:head
     (:title "Άγνωστη σελίδα")
     (head-css-std))
    (:body
     (:div :id "header"
           (logo)
           (primary-navbar 'companies))
     (:div :id "body"
           (:div :id "content" :class "summary"
                 (:p "Η σελίδα που προσπαθείτε να προσπελάσετε δεν υπάρχει.")
                 (:p "Επιστρέψτε στο κεντρικό μενού και προσπαθήστε ξανά."))))))

(define-dynamic-page error-page ("error-page") ()
  (no-cache)
  (with-document ()
    (:head
     (:title "Generic error page")
     (head-css-std))
    (:body
     (:div :id "header"
           (logo))
     (:div :id "body"
           (:div :id "content" :class "summary"
                 (:p "An error has occured.")
                 (:p "You are supposed to see this page because of illegal URL manipulation"))))))


;; (define-dynamic-page no-fsm-data () ("no-fsm-data")
;;   (no-cache)
;;   (with-page ()
;;     (:head
;;      (:title "No fsm data")
;;      (head-css-std))
;;     (:body
;;      (:div :id "header"
;;         (logo)
;;         (primary-navbar 'companies))
;;      (:div :id "body"
;;         (:div :id "content" :class "summary"
;;               (:p "No FSM Transitions data found."))))))


;;; --- dataTables --------------------



;; (define-dynamic-page banks-data ((iDisplayStart integer)
;;                                  (idisplaylength integer)
;;                                  (sSearch string)
;;                                  (sEcho integer))
;;     ("data/banks")
;;   (no-cache)
;;   (with-parameter-rebinding #'val
;;     (with-html-output (*standard-output* nil :indent nil :prologue nil)
;;       (with-db ()
;;         (let* ((pattern (concatenate 'string
;;                                      "%"
;;                                      (if (or (null sSearch) (eq :null sSearch))
;;                                          ""
;;                                          sSearch)
;;                                      "%"))
;;                (table-len (query (:select (:count 'id) :from 'bank) :single))
;;                (results (subseq (query (:select 'id 'title
;;                                                 :from 'bank
;;                                                 :where (:or (:ilike 'title pattern)
;;                                                             (:ilike 'title pattern))))
;;                                 iDisplayStart (+ iDisplayStart iDisplayLength)))
;;                (results-len (length results)))
;;           (let ((s *standard-output*))
;;             (format s
;;                     "{\"sEcho\":~D,\"iTotalRecords\":~D,\"iTotalDisplayRecords\":~D,\"aaData\":[~&"
;;                     sEcho table-len results-len)
;;             (iter (for rec in results)
;;                   (unless (first-iteration-p)
;;                     (princ #\, s ))
;;                   (format s "[~{~S~^, ~}]" rec))
;;             (format s "]}")))))))


;; (defun banks-data (display-start
;;                    display-length
;;                    columns
;;                    search
;;                    escape-regex
;;                    echo)
;;   (with-db ()
;;     (let* ((pattern (concatenate 'string "%" search "%"))
;;            (table-len (query (:select (:count 'id) :from 'bank) :single))
;;            (results (query (:select 'id 'title
;;                                     :from 'bank
;;                                     :where (:or (:ilike 'title pattern)
;;                                                 (:ilike 'title pattern)))))
;;            (results-len (length results)))
;;       (with-output-to-string (s)
;;         (format s
;;                 "{\"sEcho\":~D,\"iTotalRecords\":~D,\"iTotalDisplayRecords\":~D,\"aaData\":[~&"
;;                 echo table-len results-len)
;;         (iter (for rec in results)
;;               (unless (first-iteration-p)
;;                 (princ #\, s ))
;;               (format s "[~{~S~^, ~}]" rec))
;;         (format s "]")))))
