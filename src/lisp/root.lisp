(in-package :scrooge)


(defclass root-page (auth-dynamic-page family-mixin)
  ())


;;; --- Autocomplete --------------------

(defun autocomplete-xhr-auth-error ()
  (with-html-output (*standard-output* nil :indent nil :prologue nil)
    "[\"Session expired.\"]"))

(defpage root-page autocomplete
    ("autocomplete" :content-type "text/plain"
                    :parameter-groups '(:system (table column term)))
    ((table  symbol nil t)
     (column symbol nil t)
     (term   string nil t))
  (with-xhr-page (autocomplete-xhr-auth-error)
    (let ((results (sort (query (:select (val column) :distinct
                                  :from (val table)
                                  :where (:ilike (val column)
                                                 (ilike (val term))))
                                :column)
                         #'string<)))
      (if results
          (with-html-output (*standard-output* nil :indent nil :prologue nil)
            (write-json (coerce results 'vector)))
          (with-html-output (*standard-output* nil :indent nil :prologue nil)
            "[]")))))

(defpage root-page autocomplete-accounts
    ("autocomplete-accounts" :content-type "text/plain"
                             :parameter-groups '(:system (root term)))
    ((root symbol nil t)
     (term string nil t))
  (with-xhr-page (autocomplete-xhr-auth-error)
    (let ((results (mapcan #'(lambda (rec)
                               (let ((title (getf rec :title)))
                                 (if (search (val term) title :test #'string-equal)
                                     (list title)
                                     nil)))
                           (subtree-records *accounts*
                                            (account-id (val root))))))
      (if results
          (with-html-output (*standard-output* nil :indent nil :prologue nil)
            (write-json (coerce results 'vector)))
          (with-html-output (*standard-output* nil :indent nil :prologue nil)
            "[]")))))

(defpage root-page autocomplete-temtx
    ("autocomplete-temtx" :content-type "text/plain"
                          :parameter-groups '(:system (customer-p term)))
    ((customer-p boolean nil t)
     (term       string  nil t))
  (with-xhr-page (autocomplete-xhr-auth-error)
    (let ((results (query (:select 'title
                            :from 'temtx
                            :where (:and (:= 'customer-p (val customer-p))
                                         (:ilike 'title (ilike (val term)))))
                          :column)))
      (if results
          (with-html-output (*standard-output* nil :indent nil :prologue nil)
            (write-json (coerce results 'vector)))
          (with-html-output (*standard-output* nil :indent nil :prologue nil)
            "[]")))))
