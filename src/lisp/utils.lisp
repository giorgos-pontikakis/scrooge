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


(defun objects->plist (objlist fn keys) 
  (mapcan (lambda (key)
            (let ((obj (find key objlist :key fn)))
              (list key obj)))
          keys))




