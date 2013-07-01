(in-package :scrooge)

(defparameter *credentials*
  (with-open-file (in (fs-path "config/credentials.lisp") :direction :input)
    (read in)))

(defparameter *parameter-sets* `("company-id" (388 nil ,+html-null+ "asdf")
                                              "subset" ("foo" "bar" "baz")
                                              "search" (nil :null "123456789")))

(defun tagged-set (tag-name tag-values)
  (loop for val in tag-values
        collect (list tag-name val)))

(defun tagged-sets (sets-spec)
  (plist-map #'tagged-set sets-spec))

(defun tagged-combinations (tagged-sets)
  (labels ((tagged-combinations-2 (set1 set2)
             (mapcan (lambda (x)
                       (mapcar (lambda (y)
                                 (append x y))
                               set2))
                     set1)))
    (if tagged-sets
        (cond ((= (length tagged-sets) 1)
               tagged-sets)
              ((= (length tagged-sets) 2)
               (apply #'tagged-combinations-2 tagged-sets))
              (t
               (tagged-combinations-2 (first tagged-sets)
                                      (tagged-combinations (rest tagged-sets)))))
        nil)))

(defun uri->string (uri)
  (with-output-to-string (s)
    (render-uri uri s)))

(defun query-parameters (parameter-sets)
  (mapcar #'plist-alist (tagged-combinations (tagged-sets parameter-sets))))

(defun default-uri (path &optional query)
  (make-instance 'uri :scheme (webapp-uri-scheme (default-webapp))
                      :host (webapp-uri-host (default-webapp))
                      :path path
                      :query query))

(let ((cookie-jar (make-instance 'drakma:cookie-jar)))
  (drakma:http-request (default-uri (login))
                       :method :post
                       :parameters ()
                       :cookie-jar cookie-jar)
  #|(drakma:http-request (default-uri (company) (first (query-parameters *parameter-sets*))))|#)
