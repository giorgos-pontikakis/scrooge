(in-package :scrooge)

(defparameter *styles*
  '((attention . "attention")))

(defun style (symbol)
  (cdr (assoc symbol *styles*)))

(defun style-invalid-p (param)
  (if (or (null param) (validp param)) nil (style 'attention)))

(defun intent->widget (intent)
  (case intent
    ((:view :delete) :str)
    ((:create :update) :textbox)))


