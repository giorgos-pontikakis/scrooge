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
    (let ((sql (if (val force-chequing-p)
                   `(:select temtx.title
                      :from temtx
                      :inner-join (:as account debit-account)
                      :on (:= debit-account.id temtx.debit-account-id)
                      :inner-join (:as account credit-account)
                      :on (:= credit-account.id temtx.credit-account-id)
                      :where (:and (:= customer-p ,(val customer-p))
                                   (:ilike temtx.title ,(ilike (val term)))
                                   (:or (:= debit-account.chequing-p t)
                                        (:= credit-account.chequing-p t))))
                   `(:select temtx.title
                      :from temtx
                      :where (:and (:= customer-p ,(val customer-p))
                                   (:ilike temtx.title ,(ilike (val term))))))))
      (let ((results (query (sql-compile sql)
                            :column)))
        (if results
            (with-html-output (*standard-output* nil :indent nil :prologue nil)
              (write-json (coerce results 'vector)))
            (with-html-output (*standard-output* nil :indent nil :prologue nil)
              "[]"))))))
