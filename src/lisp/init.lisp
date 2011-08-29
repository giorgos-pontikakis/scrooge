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

(defparameter *default-project-status* "quoted")
(defparameter *default-cheque-status* "pending")
;; (defparameter *cheque-statuses*
;;   (load-time-value
;;    (with-db ()
;;      (query (:select 'description 'id
;;                      :from 'cheque-status))))
;;   "Label-value alist indended for use with dropdown lists")

;; ;; invoices
;; (defparameter *invoice-receivable-account* (get-option "invoice-receivable-account"))
;; (defparameter *invoice-payable-account* (get-option "invoice-payable-account"))

;; ;; cash
;; (defparameter *cash-account* (get-option "cash-account"))
;; (defparameter *revenues-root-account* (get-option "revenues-root-account"))
;; (defparameter *expenses-root-account* (get-option "expenses-root-account"))

;; ;; cheques
;; (defparameter *cheque-receivable-account* (get-option "cheque-receivable-account"))
;; (defparameter *cheque-payable-account* (get-option "cheque-payable-account"))
