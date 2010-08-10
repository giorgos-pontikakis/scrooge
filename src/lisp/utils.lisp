(in-package :scrooge)

(defparameter *styles*
  '((attention . "attention")))

(defun style (symbol)
  (cdr (assoc symbol *styles*)))

(defun style-invalid-p (param)
  (if (or (null param) (validp param)) nil (style 'attention)))

;; (defun intent->widget (intent)
;;   (ecase intent
;;     ((:view :delete) :str)
;;     ((:create :update) :textbox)))


(defparameter *stran-td-styles*
  '(:description "data" :old-status "data" :new-status "data" :debit-acc "data" :credit-acc "data"))

(defparameter *stran-td-keys*
  '(:description :old-status :new-status :debit-acc :credit-acc))