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




(defparameter *requests* `(:company-id (388 nil ,+html-null+ "asdf")
                           :subset ("foo" "bar" "baz")
                           :tin (nil :null "123456789")))

(defun parameter-vector (parameter-name parameter-values)
  (loop for val in parameter-values
        collect (list parameter-name val)))

(defun parameter-vectors (requests)
  (plist-map #'parameter-vector requests))

(defun combinations (vectors)
  (labels ((combinations-2 (vec1 vec2)
             (mapcan (lambda (x)
                       (mapcar (lambda (y)
                                 (append x y))
                               vec2))
                     vec1)))
    (if vectors
        (cond ((= (length vectors) 1)
               vectors)
              ((= (length vectors) 2)
               (apply #'combinations-2 vectors))
              (t
               (combinations-2 (first vectors)
                               (combinations (rest vectors)))))
        nil)))
