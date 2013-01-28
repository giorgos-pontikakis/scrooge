(in-package :scrooge)


(defclass root-page (auth-dynamic-page family-mixin)
  ())

(defpage root-page home ("")
    ()
  (see-other (company)))

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

(defpage root-page autocomplete/accounts
    ("autocomplete/accounts" :content-type "text/plain"
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

(defpage root-page autocomplete/temtx
    ("autocomplete/temtx" :content-type "text/plain"
                          :parameter-groups '(:system (customer-p term force-chequing-p)))
    ((customer-p       boolean nil t)
     (term             string  nil t)
     (force-chequing-p boolean nil))
  (with-xhr-page (autocomplete-xhr-auth-error)
    ;; When force-chequing-p is true, we return temtxs
    ;; that reference at least one chequing account
    (let ((sql `(:select title
                 :from ,(if (val force-chequing-p) 'temtx-chq 'temtx)
                 :where (:and (:= customer-p ,(val customer-p))
                              (:ilike title ,(ilike (val term)))))))
      (let ((results (query (sql-compile sql)
                            :column)))
        (if results
            (with-html-output (*standard-output* nil :indent nil :prologue nil)
              (write-json (coerce results 'vector)))
            (with-html-output (*standard-output* nil :indent nil :prologue nil)
              "[]"))))))
