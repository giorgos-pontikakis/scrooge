(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

;;; Static References

(defun css (path)
  (with-html (:link :rel "stylesheet" :type "text/css" :href (url path))))

(defun js (path)
  (with-html (:script :type "application/javascript" :src (url  path))))

(defun img (path)
  (with-html
    (:img :src (url "img" path))))

(defun lib (path)
  (with-html (:script :type "application/javascript"
                      :src (url "lib" path))))

;;; HTML Headers
(defun js-standard-headers ()
  (mapc #'js '("lib/jquery/jquery-1.4.2.min.js"
               "lib/jquery-ui/js/jquery-ui-1.8.2.custom.min.js" 
               "js/main.js")))


(defun css-standard-headers ()
  (mapc #'css '("css/reset.css"
                "css/main.css"
                "lib/jquery-ui/css/smoothness/jquery-ui-1.8.2.custom.css")))

(defun data-tables-headers ()
  (js "lib/dataTables-1.6/media/js/jquery.dataTables.min.js")
  (css "css/table.css"))

(defun config-headers ()
  (css-standard-headers)
  (js-standard-headers)
  (data-tables-headers))

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

(define-navbar primary-navbar () (:id "navbar" :ul-style "hmenu")
  (home         (home)         "Αρχική")
  (config       (config)       "Ρυθμίσεις")
  (companies    (companies)    "Εταιρίες")
  (cheques      (cheques)      "Επιταγές")
  (transactions (transactions) "Συναλλαγές")
  (projects     (projects)     "Έργα"))



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



;;;----------------------------------------------------------------------
;;; sql utilities

(defun ilike (filter)
  (if (or (null filter)
          (eq filter :null))
      "%"
      (concatenate 'string "%" filter "%")))



;;;----------------------------------------------------------------------
;;; Helper macros

(defmacro define-row-display (name fn id-keys data-keys css-classes) 
  (let ((id-syms (mapcar #'symbolicate id-keys)))
    `(defun ,name (,@id-syms values)
       (let ((activep (and ,@(mapcar (lambda (id-key id-sym)
                                       `(equal ,id-sym (getf values ,id-key)))
                                     id-keys id-syms)))) 
         (with-html
           (:tr :class (if activep "active" nil)
                (:td :class "select"
                     (if activep
                         (active-row-anchor (,fn))
                         (inactive-row-anchor (,fn ,@(mapcan (lambda (id-key)
                                                               `(,id-key (getf values ,id-key)))
                                                             id-keys)))))
                ,@(data-rows-td data-keys css-classes) 
                (:td :class "button" "")
                (:td :class "button" "")))))))

(defmacro define-row-create (name fn action id-keys data-keys css-classes)
  (let ((id-syms (mapcar #'symbolicate id-keys)))
    `(defun ,name (,@id-syms values styles)
       (declare (ignore ,@id-syms))
       (with-html
         (:tr :class "active"
              (with-form (,action)
                (:td :class "select" (active-row-anchor (,fn)))
                ,@(data-rows-input data-keys css-classes)
                (:td :class "button" (ok-button))
                (:td :class "button" (cancel-button (,fn)))))))))

(defmacro define-row-update (name fn action id-keys data-keys css-classes)
  (let ((id-syms (mapcar #'symbolicate id-keys)))
   `(defun ,name (,@id-syms values styles) 
      (with-html
        (:tr :class "active"
             (with-form (,action ,@(zip id-keys id-syms))
               (:td :class "select"
                    (active-row-anchor (,fn ,@(zip id-keys id-syms))))
               ,@(data-rows-input data-keys css-classes)
               (:td :class "button" (ok-button))
               (:td :class "button" (cancel-button (,fn ,@(zip id-keys id-syms))))))))))

(defmacro define-row-delete (name fn action id-keys data-keys css-classes) 
  (let ((id-syms (mapcar #'symbolicate id-keys)))
    `(defun ,name (,@id-syms values) 
       (with-html
         (:tr :class "attention"
              (with-form (,action ,@(zip id-keys id-syms))
                (:td :class "select"
                     (active-row-anchor (,fn ,@(zip id-keys id-syms))))
                ,@(data-rows-td data-keys css-classes)
                (:td :class "button" (ok-button))
                (:td :class "button" (cancel-button (,fn ,@(zip id-keys id-syms))))))))))

(defun data-rows-td (data-keys css-classes)
  (mapcar (lambda (key class)
            `(:td :class ,class (str (lisp-to-html (getf values ,key)))))
          data-keys css-classes))

(defun data-rows-input (data-keys css-classes)
  (mapcar (lambda (key class)
            `(:td :class ,class (textbox ,key
                                         :value (getf values ,key)
                                         :style (getf styles ,key))))
          data-keys css-classes))

;;; ------------------------------------------------------------
;;; Row selectors
;;; ------------------------------------------------------------

(defun active-row-anchor (href)
  (with-html
    (:a :href href (active-row-img))))

(defun inactive-row-anchor (href)
  (with-html
    (:a :href href (inactive-row-img))))

(defun active-row-img ()
  (with-html
    (:img :src (url "img/bullet_red.png"))))

(defun inactive-row-img ()
  (with-html
    (:img :src (url "img/bullet_blue.png"))))

;; (defun selector-td (activep &key active-href inactive-href)
;;   (with-html
;;     (if activep
;;         (:td :class "active"   (active-row-anchor active-href))
;;         (:td :class "inactive" (inactive-row-anchor inactive-href)))))

(defun selector-td (activep href)
  (with-html
    (:td :class (if activep "active" nil)
         (:a :href href
             (if activep
                 (htm (:img :src (url "img/bullet_red.png")))
                 (htm (:img :src (url "img/bullet_blue.png"))))))))

;;; ------------------------------------------------------------
;;; Generate TD tags for a row
;;; ------------------------------------------------------------

;; (defgeneric row-td (widget values styles))

;; (defmethod row-td ((widget (eql 'textbox)) values styles) 
;;   (with-html
;;     (iter (for key in values by #'cddr)
;;           (for val in (rest values) by #'cddr)
;;           (for sty in (rest styles) by #'cddr)
;;           (break)
;;           (htm
;;            (:td :class sty (textbox key :value val :style sty))))))

;; (defmethod row-td ((widget (eql 'str)) values styles) 
;;   (with-html
;;     (iter (for key in values by #'cddr)
;;           (for val in (rest values) by #'cddr)
;;           (for sty in (rest styles) by #'cddr)
;;           (break)
;;           (htm
;;            (:td :class sty (str (lisp-to-html val)))))))



(defun row-td (keys widget values styles) 
  (iter (for key in keys) 
        (let ((val (getf values key))
              (sty (getf styles key)))
          #|(print (list "***" key val sty values styles))|#
          (ecase widget
            (:str (with-html
                    (:td :class sty (str (lisp-to-html val)))))
            (:textbox (with-html
                        (:td :class sty
                             (textbox key :value val :style sty))))))))










