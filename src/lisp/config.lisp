(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

(define-navbar config-navbar () (:id "subnavbar" :ul-style "hmenu")
  (banks    (banks)    "Τράπεζες")
  (tofs     (tofs)     "Δ.Ο.Υ.")
  (city     (city)     "Πόλεις")
  (accounts (accounts) "Λογαριασμοί")
  (temtx    (temtx)    "Πρότυπες Συναλλαγές")
  (stran    (stran)    "Καταστατικές Μεταβολές"))



;;; Config main page

(define-dynamic-page config () ("config/")
  (no-cache)
  (with-page ()
    (:head
     (:title "Ρυθμίσεις")
     (css-standard-headers))
    (:body
     (:div :id "header"
	   (logo)
	   (primary-navbar 'config)
	   (config-navbar nil))
     (:div :id "body" 
	   (:div :id "content" :class "window"
		 "Don't touch")
	   (footer)))))

(defun make-sql-config (table-name)
  (sql-compile
   `(:select 'id 'title :from ,table-name)))



;;; Configuration pages Widget

(defclass config-page (widget)
  ((title   :accessor title   :initarg :title)
   (message :accessor message :initarg :message) 
   (body    :accessor body    :initarg :body)))

(defmethod render ((obj config-page) &key)
  (with-page ()
    (:head
     (:title (title obj))
     (config-headers))
    (:body
     (:div :id "header"
           (logo)
           (primary-navbar 'config)
           (config-navbar (name obj)))
     (:div :id "body"
           (:div :class "message"
                 (:h2 :class "info" (message obj)))
           (:div :id (string-downcase (name obj))
                 :class "window"
                 (render (body obj)))) 
     (footer))))

(defun make-config-page (&key name title message body)
  (make-instance 'config-page
                 :name name
                 :title title
                 :message message 
                 :body body))

(defun simple-table-cells ()
  (lambda (row) 
    (list (make-cell-selector :row row
                              :name :selector
                              :style "select") 
          (make-cell-textbox :row row
                             :name :title
                             :value (getf (data row) :title)
                             :style "data"
                             :operations '(:create :update)) 
          (make-cell-submit :row row
                            :name :submit
                            :style "button"
                            :operations '(:create :update :delete))
          (make-cell-cancel :row row
                            :name :cancel
                            :style "button"
                            :operations '(:create :update :delete)))))