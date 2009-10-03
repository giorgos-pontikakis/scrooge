(in-package :scrooge)

(disable-sql-reader-syntax)
(enable-sql-reader-syntax)

(defun css (&rest files)
  (with-html
    (mapc (lambda (file)
	    (htm (:link :href (url "css/" file) :rel "stylesheet" :type "text/css")))
	  files)))

(defun logo ()
  (with-html
    (:h1 "Scrooge")))

(defmacro navbar (active-label)
  (let ((items (iter (for (label href) in '(("Αρχική" (home))
					    ("Ρυθμίσεις" (config))
					    ("Συναλλασσόμενοι" (companies))
					    ("Λογαριασμοί" (accounts))
					    ("Συναλλαγές" (transactions)))) 
		     (collect `(link ,label
				     :href ,(if (string-equal label active-label)
						nil
						href)
				     :style '(:p "active"))))))
    `(with-ul (:id "navbar") 
       ,@items)))




