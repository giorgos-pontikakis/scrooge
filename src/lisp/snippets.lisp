(in-package :scrooge)


;;; HTML Header

(defun js-headers (&rest urls)
  (mapc #'(lambda (js-url)
	    (with-html (:script :type "text/javascript"
				:src (url js-url))))
	urls))

(defun css-headers (&rest urls)
  (mapc #'(lambda (css-url)
	    (with-html (:link :rel "stylesheet"
			      :type "text/css"
			      :href (url css-url))))
	urls))

(defun js-standard-headers ()
  (js-headers "lib/jquery/jquery-1.3.2.min.js"
	      "lib/jquery.autocomplete/jquery.autocomplete.pack.js"
	      "js/main.js"))


(defun css-standard-headers ()
  (css-headers "css/reset.css"
	       "css/main.css"
	       "lib/jquery.autocomplete/jquery.autocomplete.css"))


;;; User interface elements

(defun ok-button ()
  (with-html
    (:button :type "submit" (:img :src (url "img/tick.png")))))

(defun cancel-button (href)
  (with-html
    (:a :href href (:img :src (url "img/cancel.png")))))

(defun logo ()
  (with-html
    (:h1 "Scrooge")))

(defun footer ()
  (with-html
    (:div :id "footer" "Powered by lisp")))

(defun primary-navbar (active-item)
  (let ((options 
	 (list 'home (lambda (class)
		       (with-html
			 (:li (:a :class class :href (home) "Αρχική"))))
	       'config (lambda (class)
			 (with-html
			   (:li (:a :class class :href (config) "Ρυθμίσεις"))))
	       'companies (lambda (class)
			    (with-html
			      (:li (:a :class class :href (companies) "Εταιρίες"))))
	       'cheques (lambda (class)
			  (with-html
			    (:li (:a :class class :href (cheques) "Επιταγές"))))
	       'transactions (lambda (class)
			       (with-html
				 (:li (:a :class class :href (transactions) "Συναλλαγές")))))))
    (with-html
      (:div :id "navbar"
	    (:ul :class "hmenu"
		 (iter (for item in options by #'cddr)
		       (for fn in (rest options) by #'cddr)
		       (funcall fn (if (eql item active-item) "active" nil))))))))



;;; Breadcrumb navigation

(defparameter *pagetree*
  '(home "Start" ((companies "Εταιρίες" ((company/view "Προβολή" nil)
					 (company/create "Δημιουργία" nil)
					 (company/update "Επεξεργασία" nil)
					 (company/delete "Διαγραφή" nil)))
		  (config "Ρυθμίσεις" ((banks "Τράπεζες" ((bank/view "Προβολή" nil)
							  (bank/create "Δημιουργία" nil)
							  (bank/update "Επεξεργασία" nil)
							  (bank/delete "Διαγραφή" nil)))
				       (accounts "Λογαριασμοί" ((account/view "Προβολή" nil)
								(account/create "Δημιουργία" nil)
								(account/update "Επεξεργασία" nil)
								(account/delete "Διαγραφή" nil))))))))


;;; node = (page-name parent-list label children-spec)

(defun target-node-p (target node)
  (eql target (first node)))

(defun find-node (target fringe)
  (let ((node (first fringe)))
    (if (null node)
	:not-found
	(if (target-node-p target node)
	    (append (second node) (list (first node) (third node)))
	    (find-node target 
		       (append (fourth node)
			       (rest fringe)))))))




(defun make-node (spec parent)
  (list (first spec)
	parent
	(second spec)
	(mapcar (lambda (child-spec)
		  (make-node child-spec (append parent
						(list (first spec)
						      (second spec)))))
		(third spec))))

