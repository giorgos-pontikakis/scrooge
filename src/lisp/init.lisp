(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; Application configuration
;;; ----------------------------------------------------------------------
(define-webapp *scrooge* (webapp)
  :name 'scrooge
  :port 3001
  :web-root "/scrooge/"
  :fs-root #p"/home/gnp/www/scrooge/public/"
  :web-paths '((css  . "css/")
               (js   . "js/")
               (lib  . "lib/")
               (img  . "img/"))
  :debug-p (not (string-equal (machine-instance) "www"))
  :database (make-instance 'database
                           :dbname "scrooge"
                           :dbhost "localhost"
                           :dbuser "gnp"
                           :dbpass "gnp!p0stgresql"
                           :adapter "postgres"))

(when (debug-p (package-webapp))
  (setf *catch-errors-p* nil))

(defparameter *default-project-status* "quoted")