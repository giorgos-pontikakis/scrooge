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

(define-webapp *scrooge* ()
  :acceptor (make-instance  'scrooge-acceptor
                            :port 3001
                            :access-log-destination nil
                            :message-log-destination nil
                            :document-root #p"/home/gnp/www/scrooge/public")
  :packages '(:scrooge)
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
  :autostart t
  :debug-p (not (member (machine-instance) (list "www" "pulsar") :test #'string-equal)))
