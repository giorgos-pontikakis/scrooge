(in-package :scrooge)


;;; ----------------------------------------------------------------------
;;; Acceptor configuration
;;; ----------------------------------------------------------------------

(defclass scrooge-acceptor (veil-acceptor)
  ())

(define-webapp *scrooge* (webapp/db)
  :acceptor (make-instance 'scrooge-acceptor
                           :port 3001
                           :access-log-destination nil
                           :message-log-destination nil
                           :document-root #p"/home/gnp/www/scrooge/public")
  :packages '(:scrooge)
  :db-connection-spec '(:dbname "scrooge"
                        :dbhost "localhost"
                        :dbuser "gnp"
                        :dbpass "gnp!p0stgresql")
  :uri-scheme :https
  :uri-host (concatenate 'string (machine-instance) ".minorhack.com")
  :uri-root "/scrooge/"
  :uri-paths '((css  . "css/")
               (js   . "js/")
               (lib  . "lib/")
               (img  . "img/"))
  :fs-root #p"/home/gnp/www/scrooge/"
  :fs-paths '()
  :autostart-p t
  :debug-p (not (member (machine-instance) '("www" "pulsar") :test #'string-equal)))
