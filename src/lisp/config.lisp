(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))

(define-navbar config-navbar () (:id "subnavbar" :ul-style "hmenu")
  (bank     (bank)     "Τράπεζες")
  (tof      (tof)      "Δ.Ο.Υ.")
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


;;; Configuration pages Widget

(defun config-page (&key name title message body)
  (with-page ()
    (:head
     (:title title)
     (config-headers))
    (:body
     (:div :id "header"
           (logo)
           (primary-navbar 'config)
           (config-navbar name))
     (:div :id "body"
           (:div :class "message"
                 (:h2 :class "info" message))
           (:div :id (string-downcase name)
                 :class "window"
                 (render body))) 
     (footer))))

(defun config-cells-fn ()
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

(defun config-data-fn (table-name)
  (lambda (filters)
    (declare (ignore filters))
    (with-db
      (query (sql-compile
              `(:select 'id 'title :from ,table-name))
             :plists))))