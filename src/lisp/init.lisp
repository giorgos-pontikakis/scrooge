(in-package :scrooge)

(declaim (optimize (safety 3) (debug 3) (compilation-speed 0) (speed 0) (space 0)))


;;; ----------------------------------------------------------------------
;;; Application configuration
;;; ----------------------------------------------------------------------
(define-webapp *scrooge* (webapp)
    :name 'scrooge
    :port 3001
    :web-root "/scrooge/"
    :fs-root #p"/home/gnp/www/scrooge/public/"
    :fs-paths '()
    :debug-p (not (string-equal (machine-instance) "www"))
    :database (make-instance 'database
                             :dbname "scrooge"
                             :dbhost "localhost"
                             :dbuser "gnp"
                             :dbpass ""
                             :adapter "postgres"))
