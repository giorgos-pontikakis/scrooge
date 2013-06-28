(in-package :scrooge)

(defparameter *params* (list (list :a 1 +html-true+ +html-false+ +html-null+)
                             (list :b 2 "beta" nil +html-null+)
                             (list :c 3 "foo" "bar" "baz" 1 :null)))

(defun powerset-2 (list1 list2)
  (let ((param1 (first list1))
        (param2 (first list2)))
    (mapcan (lambda (i)
              (mapcar (lambda (j)
                        (list param1 i param2 j))
                      (rest list2)))
            (rest list1))))



(defun powerset (parameter-list)
  (cond ((< (length parameter-list) 2)
         (error "Powerset needs at least two parameter lists to operate."))
        ((= (length parameter-list) 2)
         (apply #'powerset-2 parameter-list))
        (t
         (powerset-2 parameter-list
                     (powerset (rest parameter-list))))))




(defparameter *parameter-sets* `(:company-id (388 nil ,+html-null+ "asdf")
                                 :subset ("foo" "bar" "baz")
                                 :search (nil :null "123456789")))

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


(defun batch-request-urls (page-fn parameter-sets)
  (mapply page-fn (tagged-combinations (tagged-sets parameter-sets))))
