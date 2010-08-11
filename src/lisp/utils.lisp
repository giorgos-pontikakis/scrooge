(in-package :scrooge)

(defparameter *styles*
  '((attention . "attention")))

(defun style (symbol)
  (cdr (assoc symbol *styles*)))

(defun style-invalid-p (param)
  (if (or (null param) (validp param)) nil (style 'attention)))

(defparameter *stran-td-styles*
  '(:description "data" :old-status "data" :new-status "data" :debit-acc "data" :credit-acc "data"))

(defparameter *stran-td-keys*
  '(:description :old-status :new-status :debit-acc :credit-acc))



(defun plist-subset (plist keys)
  (mapcan (lambda (key)
            (list key (second (member key plist))))
          keys))

(defun plist-keys (plist)
  (iter (for key in plist by #'cddr)
        (collect key)))

(defun plist-vals (plist)
  (iter (for key in (rest plist) by #'cddr)
        (collect key)))

(defun params->plist (objlist fn keys)
  (mapcan (lambda (key)
            (let ((param (find key objlist :key fn)))
              (list key param)))
          keys))







