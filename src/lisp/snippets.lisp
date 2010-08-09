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

(defmacro html-fn (&rest forms)
  `(lambda ()
    (with-html
      ,@forms)))

(defun parallel (list1 list2 item1 &key (test #'eql))
  (let ((pos (position item1 list1 :test test)))
    (when pos
      (nth pos list2))))

(defun durl (url)
  (destructuring-bind (script-string query-string) (cl-ppcre:split "\\?" url)
    (let* ((script-sym (symbolicate
                        (string-upcase
                         (second (split (cfg :webroot) script-string)))))
           (query-pairs (mapcar (lambda (pair-string)
                                  (destructuring-bind (param value) (split "=" pair-string)
                                    (let* ((keyparam (make-keyword (string-upcase param)))
                                           (page (page script-sym))
                                           (keyparams (www-toolkit::keyparams page))
                                           (types (www-toolkit::types page))
                                           (type (parallel keyparams types keyparam)))
                                      (if keyparam
                                          (cons keyparam (html-to-lisp value type))
                                          (error "Unknown parameter in query string.")))))
                                (split "&" query-string))))
      (list script-sym query-pairs))))

;; (defun make-form (call-form html-fn) 
;;   (destructuring-bind (page-sym &rest keyword-args) call-form
;;     (let ((page (page page-sym)))
;;      (with-html
;;        (:form :method (www-toolkit::request-type page)
;;               :action (url (www-toolkit::base-url page))
;;               (iter (for key in keyword-args by #'cddr)
;;                     (for val in (rest keyword-args) by #'cddr)
;;                     (with-html
;;                       (:input :type "hidden"
;;                               :id (string-downcase key)
;;                               :class "display-none"
;;                               :name (string-downcase key)
;;                               :value (lisp-to-html val))))
;;               (funcall html-fn))))))

(defun make-form (url html-fn) 
  (destructuring-bind (page-sym param-alist) (durl url)
    (let ((page (page page-sym)))
     (with-html
       (:form :method (www-toolkit::request-type page)
              :action (url (www-toolkit::base-url page))
              (iter (for (key . val) in param-alist)
                    (with-html
                      (:input :type "hidden"
                              :id (string-downcase key)
                              :class "display-none"
                              :name (string-downcase key)
                              :value (lisp-to-html val))))
              (funcall html-fn))))))


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

(defun active-row-img ()
  (with-html
    (:img :src (url "img/bullet_red.png"))))

(defun inactive-row-img ()
  (with-html
    (:img :src (url "img/bullet_blue.png"))))


(defun active-row-anchor (href &optional name)
  (with-html
    (:a :href href :name name (active-row-img))))

(defun inactive-row-anchor (href &optional name)
  (with-html
    (:a :href href :name name (inactive-row-img))))

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


;; (defun data-rows-td (data-keys css-classes input-widgets)
;;   (mapcar (lambda (key class widget)
;;             (case widget
;;               (:text `(:td :class ,class (str (lisp-to-html (getf values ,key)))))
;;               (:textbox `(:td :class ,class (textbox ,key
;;                                                      :value (getf values ,key)
;;                                                      :style (getf styles ,key))))
;;               (:dropdown `)
;;               (otherwise (error "Unimplemented widget."))))
;;           data-keys css-classes))

;; (defmacro define-row-display (name fn id-key data-keys css-classes) 
;;   `(defun ,name (active-id values)
;;      (let ((activep (and active-id (eql active-id (getf values ,id-key))))) 
;;        (with-html
;;          (:tr :class (if activep "active" nil)
;;               (:td :class "select"
;;                    (if activep
;;                        (active-row-anchor (,fn) active-id)
;;                        (inactive-row-anchor (,fn ,id-key (getf values ,id-key)))))
;;               ,@(data-rows-td data-keys css-classes) 
;;               (:td :class "button" "")
;;               (:td :class "button" ""))))))

;; (defmacro define-row-create (name fn action data-keys css-classes)
;;   `(defun ,name (active-id values styles)
;;      (with-html
;;        (:tr :class "active"
;;             (with-form (,action)
;;               (:td :class "select" (active-row-anchor (,fn) active-id))
;;               ,@(data-rows-input data-keys css-classes)
;;               (:td :class "button" (ok-button))
;;               (:td :class "button" (cancel-button (,fn))))))))

;; (defmacro define-row-update (name fn action id-key data-keys css-classes)
;;   `(defun ,name (active-id values styles) 
;;      (with-html
;;        (:tr :class "active"
;;             (with-form (,action ,id-key active-id)
;;               (:td :class "select"
;;                    (active-row-anchor (,fn ,id-key active-id) active-id))
;;               ,@(data-rows-input data-keys css-classes)
;;               (:td :class "button" (ok-button))
;;               (:td :class "button" (cancel-button (,fn ,id-key active-id))))))))

;; (defmacro define-row-delete (name fn action id-key data-keys css-classes) 
;;   `(defun ,name (active-id values) 
;;      (with-html
;;        (:tr :class "attention"
;;             (with-form (,action ,id-key active-id)
;;               (:td :class "select"
;;                    (active-row-anchor (,fn ,id-key active-id) active-id))
;;               ,@(data-rows-td data-keys css-classes)
;;               (:td :class "button" (ok-button))
;;               (:td :class "button" (cancel-button (,fn ,id-key active-id))))))))


