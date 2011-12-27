(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Time
;;; ----------------------------------------------------------------------

(define-timezone +UTC+ #p"/usr/share/zoneinfo/UTC")

(cl-postgres:set-sql-datetime-readers :timestamp-with-timezone
                                      (lambda (sec)
                                        (timestamp+ (encode-timestamp 0 0 0 0 1 1 2000
                                                                      :timezone +UTC+)
                                                    (/ sec 1000000) :sec)))


;;; ----------------------------------------------------------------------
;;; Acceptor configuration
;;; ----------------------------------------------------------------------

(defclass scrooge-acceptor (veil-acceptor)
  ())

(define-acceptor *scrooge* (scrooge-acceptor)
  :document-root #p"/home/gnp/www/scrooge/public"
  :access-log-destination nil
  :message-log-destination nil
  :packages '(:scrooge)
  :port 3001
  :db-connection-spec '(:dbname "scrooge"
                        :dbhost "localhost"
                        :dbuser "gnp"
                        :dbpass "gnp!p0stgresql")
  :web-root "/scrooge/"
  :web-paths '((css  . "css/")
               (js   . "js/")
               (lib  . "lib/")
               (img  . "img/"))
  :fs-root #p"/home/gnp/www/scrooge/"
  :fs-paths '()
  :debug-p (not (member (machine-instance) (list "www" "pulsar") :test #'string-equal)))



;;; ------------------------------------------------------------
;;; Globals
;;; ------------------------------------------------------------

(defparameter *project-states* (lists->alist
                                (with-db ()
                                  (query (:select 'id 'description
                                                  :from 'project-state)))))

(defparameter *cheque-states*
  (lists->alist
   (with-db ()
     (query (:select 'id 'description
                     :from 'cheque-state)))))



(defparameter *default-project-state* "quoted")
(defparameter *default-cheque-state* "pending")


(with-db ()
  ;; cash
  (defparameter *cash-acc-id*
    (account-id (get-dao 'account-role "cash-account")))
  (defparameter *revenues-root-acc-id*
    (account-id (get-dao 'account-role "revenues-root-account")))
  (defparameter *expenses-root-acc-id*
    (account-id (get-dao 'account-role "expenses-root-account")))
  ;; invoices
  (defparameter *invoice-receivable-acc-id*
    (account-id (get-dao 'account-role "invoice-receivable-account")))
  (defparameter *invoice-payable-acc-id*
    (account-id (get-dao 'account-role "invoice-payable-account")))
  ;; cheques
  (defparameter *cheque-receivable-acc-id*
    (account-id (get-dao 'account-role "cheque-receivable-account")))
  (defparameter *cheque-payable-acc-id*
    (account-id  (get-dao 'account-role "cheque-payable-account"))))
