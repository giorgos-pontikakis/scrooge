(in-package :scrooge)


(defclass root-page (dynamic-page page-family-mixin)
  ((system-parameter-names :initarg  :system-parameter-names)
   (user-parameter-names   :initarg  :user-parameter-names)
   (filter-parameter-names :initarg  :filter-parameter-names)
   (allowed-groups         :initform '("user" "admin")))
  (:default-initargs :system-parameter-names ()
                     :user-parameter-names ()
                     :filter-parameter-names ()))


;;; --- login --------------------

(defpage root-page home ("") ()
  (with-view-page
    (with-document ()
      (:head
       (:title "Αρχική")
       (global-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'home)
             (:div :class "clear")
             (:div :id "content"
                   (:p "Home content not yet available"))
             (footer))))))



;;; --- Autocomplete --------------------

(defpage dynamic-page autocomplete ("autocomplete" :content-type "text/plain")
    ((table  symbol nil t)
     (column symbol nil t)
     (term   string nil t))
  (with-auth ("configuration")
    (if (every #'validp (parameters *page*))
        (with-db ()
          (let ((results (query (:select (val column) :distinct
                                         :from (val table)
                                         :where (:ilike (val column)
                                                        (ilike (val term))))
                                :column)))
            (if results
                (with-html-output (*standard-output* nil :indent nil :prologue nil)
                  (write-json (coerce results 'vector)))
                (with-html-output (*standard-output* nil :indent nil :prologue nil)
                  "[]"))))
        (see-other (error-page)))))
