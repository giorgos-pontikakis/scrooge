(in-package :scrooge)

(defun params->plist (bag params) 
  (mapcan (lambda (key)
            (let ((par (find key params :key #'name)))
              (list key par)))
          bag))


(defun style (indicator params intent)
  (if (eql intent :delete)
      nil
      (let ((p (find indicator params :key #'name)))
        (if (or (null p) (validp p)) "" "attention"))))

(defun datum (indicator row-data)
  (getf row-data indicator))
