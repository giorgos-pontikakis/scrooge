(in-package :scrooge)


(defclass root-page (auth-dynamic-page family-mixin)
  ())


;;; --- Autocomplete --------------------

(defun autocomplete-xhr-auth-error ()
  (with-html-output (*standard-output* nil :indent nil :prologue nil)
    "[\"Session expired.\"]"))

(defpage root-page autocomplete ("autocomplete" :content-type "text/plain"
                                                :parameter-groups '(:system (table column term)))
    ((table  symbol nil t)
     (column symbol nil t)
     (term   string nil t))
  (with-xhr-page (autocomplete-xhr-auth-error)
    (let ((results (query (:select (val column) :distinct
                                   :from (val table)
                                   :where (:ilike (val column)
                                                  (ilike (val term))))
                          :column)))
      (if results
          (with-html-output (*standard-output* nil :indent nil :prologue nil)
            (write-json (coerce results 'vector)))
          (with-html-output (*standard-output* nil :indent nil :prologue nil)
            "[]")))))
