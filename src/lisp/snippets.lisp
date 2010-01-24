(in-package :scrooge)


;;; HTML Header

(defun css (&rest files)
  (with-html
    (mapc (lambda (file)
	    (htm (:link :href (url "css/" file) :rel "stylesheet" :type "text/css")))
	  files)))


(defun js-headers ()
  (insert-js-headers "lib/jquery/jquery-1.3.2.min.js"
		     "lib/jquery.autocomplete/jquery.autocomplete.pack.js"
		     "js/main.js")
  (insert-css-headers "lib/jquery.autocomplete/jquery.autocomplete.css"))

(defun insert-css-headers (&rest files)
  (with-html
    (mapc (lambda (file)
	    (htm (:link :href (url file) :rel "stylesheet" :type "text/css")))
	  files)))

(defun insert-js-headers (&rest js-pathnames)
  (mapc #'(lambda (pathname)
	    (with-html (:script :type "text/javascript"
				:src (url pathname))))
	js-pathnames))



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

(defun cheque-statuses ()
  '(("Εκκρεμεί" "pending")
    ("Πληρώθηκε" "paid")
    ("Επιστράφηκε" "returned")
    ("Σφραγίστηκε" "bounced")))

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
			   (:li (:a :class class :href (cheques) "Επιταγές")))))))
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


;;; (find-node 'banks (list (make-node *pagetree* nil)))




;; (defclass navbar () (
;;    (id    :accessor id    :initarg :id)
;;    (class :accessor class :initarg :class) 
;;    (items :accessor items :initarg :items)))

;; (defclass navbar-item ()
;;   ((id     :accessor id     :initarg :id) 
;;    (title  :accessor title  :initarg :title)
;;    (fn     :accessor fn     :initarg :fn)))

;; (defparameter *navbars*
;;   (list (make-instance 'navbar
;; 		       :id 'company
;; 		       :class "hmenu"
;; 		       :items )))

;; (defun make-navbar-company (company-id)
;;   (mapcar (lambda (id title fn)
;; 	    (make-instance 'navbar-item
;; 			   :id id 
;; 			   :title title
;; 			   :fn fn))
;; 	  '(company-list overview cheques)
;; 	  '("Εταιρίες" "Επισκόπηση" "Επιταγές")
;; 	  (list (lambda () (companies))
;; 		(lambda () (company/view :id company-id))
;; 		(lambda () (company/cheques :id company-id)))))





;; (defgeneric render (widget &key &allow-other-keys))

;; (defmethod render ((navbar navbar) &key &allow-other-keys)
;;   (with-html
;;     (:div :id (id navbar)
;; 	  (:ul :class (class navbar)
;; 	       (iter (for item in (items navbar))
;; 		     (htm (:li (render item))))))))

;; (defmethod render ((item navbar-item) &key active-id)
;;   (with-html
;;     (:a :class (if (eql (id item) active-id) "active" nil)
;; 	:href (funcall (fn item) (id item))
;; 	(str (title item)))))

;; (defmacro define-navbar ((&rest params) &body body)
;;   )