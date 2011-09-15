(in-package :scrooge)

;;; ----------------------------------------------------------------------
;;; Acceptor configuration
;;; ----------------------------------------------------------------------

(define-acceptor *scrooge* (veil-acceptor)
  :packages '(:scrooge)
  :port 3001
  :db-connection-spec '(:dbname "scrooge"
                        :dbhost "localhost"
                        :dbuser "gnp"
                        :dbpass "gnp!p0stgresql")
  :doc-root #p"/home/gnp/www/scrooge/public"
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



;; ;; cash
;; (defparameter *cash-account* (get-option "cash-account"))
;; (defparameter *revenues-root-account* (get-option "revenues-root-account"))
;; (defparameter *expenses-root-account* (get-option "expenses-root-account"))

;; ;; invoices
;; (defparameter *invoice-receivable-account* (get-option "invoice-receivable-account"))
;; (defparameter *invoice-payable-account* (get-option "invoice-payable-account"))

;; ;; cheques
;; (defparameter *cheque-receivable-account* (get-option "cheque-receivable-account"))
;; (defparameter *cheque-payable-account* (get-option "cheque-payable-account"))
