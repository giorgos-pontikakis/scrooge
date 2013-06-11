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
      (with-html-output (*standard-output* nil :indent nil :prologue nil)
        (write-json (coerce results 'vector))))))

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
      (with-html-output (*standard-output* nil :indent nil :prologue nil)
        (write-json (coerce results 'vector))))))

(defpage root-page autocomplete/temtx
  ("autocomplete/temtx" :content-type "text/plain"
                        :parameter-groups '(:system (customer-p term force-chequing-p)))
  ((customer-p       boolean nil t)
   (term             string  nil t)
   (force-chequing-p boolean nil))
  (with-xhr-page (autocomplete-xhr-auth-error)
    ;; When force-chequing-p is true, we return temtxs
    ;; that reference at least one chequing account
    (let* ((sql `(:select title
                  :from ,(if (val force-chequing-p) 'temtx-chq 'temtx)
                  :where (:and (:= customer-p ,(val customer-p))
                               (:ilike title ,(ilike (val term))))))
           (results (query (sql-compile sql) :column)))
      (with-html-output (*standard-output* nil :indent nil :prologue nil)
        (write-json (coerce results 'vector))))))

(defpage root-page autocomplete/project
  ("autocomplete/project" :content-type "text/plain"
                          :parameter-groups '(:system (company-title state)))
  ((company-title string chk-company-title    t)
   (state         string chk-project-state-id))
  (with-xhr-page (autocomplete-xhr-auth-error)
    (let* ((sql `(:select project.id project.description
                          :from project
                          :inner-join company
                          :on (:= company.id project.company-id)
                          :where (:and (:= company.title ,(val company-title))
                                       (:= project.state-id ,(if (suppliedp state)
                                                                 (val state)
                                                                 "ongoing")))))
           (results (query (sql-compile sql))))
      (if results
          (with-html
              (:select :name "project-id"
                       (loop for (id description) in results
                             do (htm (:option :value id (str description))))))
          (with-html
              (:input :type "hidden" :name "project-id" :value +html-null+)
            (:p "Δεν υπάρχει ενεργό για αυτή την εταίρια. "
                (:a :href (project/create :company (val company-title)) "Νέο έργο »")))))))
