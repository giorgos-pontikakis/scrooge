(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))


;;; Widgets

(defun config-navbar (active)
  (render (hnavbar)
          :id "subnavbar"
          :page-specs '((bank     "Τράπεζες")
                        ;; (tof	   "Δ.Ο.Υ.")
                        ;; (city	   "Πόλεις")
                        ;; (accounts "Λογαριασμοί")
                        ;; (temtx	   "Πρότυπες Συναλλαγές")
                        ;; (stran	   "Καταστατικές Μεταβολές")
                        )
          :active-page-name active))



;;; Config main page

(define-dynamic-page config () ("config/")
  (no-cache)
  (with-document ()
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

;; (defun config-page (&key name title message body)
;;   (with-page ()
;;     (:head
;;      (:title title)
;;      (config-headers))
;;     (:body
;;      (:div :id "header"
;;            (logo)
;;            (primary-navbar 'config)
;;            (config-navbar name))
;;      (:div :id "body"
;;            (:div :class "message"
;;                  (:h2 :class "info" (str message)))
;;            (:div :id (string-downcase name)
;;                  :class "window"
;;                  (render body))) 

;;      (footer))))

;; (defun config-row-fn ()
;;   (lambda (row)
;;     (let ((data (data row))
;;           (operation (operation row))) 
;;       (list (make-instance 'cell-selector
;;                            :name :selector
;;                            :style "select"
;;                            :href (selector-href row)) 
;;             (make-instance 'cell-textbox :name :title
;;                            :value (getf data :title)
;;                            :style "data"
;;                            :enabledp (member operation '(:create :update))) 
;;             (make-instance 'cell-submit
;;                            :name :submit 
;;                            :style "button"
;;                            :operations (member operation '(:create :update :delete)))
;;             (make-instance 'cell-cancel
;;                            :row row
;;                            :name :cancel
;;                            :href 
;;                            :style "button"
;;                            :enabled (member operation '(:create :update :delete)))))))


(defun config-data-fn (table-name)
  (lambda () 
    (with-db ()
      (query (sql-compile
              `(:select 'id 'title :from ,table-name))
             :plists))))


(defun config-data (table-name)
  (with-db ()
    (query (sql-compile
            `(:select 'id 'title :from ,table-name))
           :plists)))


;; (defun standard-page (&key name title message body)
;;   (with-page ()
;;     (:head
;;      (:title title)
;;      (config-headers))
;;     (:body
;;      (:div :id "header"
;;            (logo)
;;            (primary-navbar 'config)
;;            (config-navbar name))
;;      (:div :id "body"
;;            (:div :class "message"
;;                  (:h2 :class "info" (str message)))
;;            (render body)) 
;;      (footer))))

;; (defun window (&key name body)
;;   (html ()
;;     (:div :id (string-downcase name)
;;           :class "window"
;;           (render body))))


