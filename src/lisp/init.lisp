(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; integration for local-time and cl-postgres
;;; ----------------------------------------------------------------------

(set-local-time-cl-postgres-readers)

(define-timezone "+GREEK-ZONE+" #p"/usr/share/zoneinfo/Europe/Athens")



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
