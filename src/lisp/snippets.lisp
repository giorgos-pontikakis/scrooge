(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; Static References

(defun css (path)
  (with-html (:link :rel "stylesheet" :type "text/css" :href (url path))))

(defun js (path)
  (with-html (:script :type "application/javascript" :src (url  path))))

(defun img (path)
  (with-html
    (:img :src (url "img/" path))))

(defun lib (path)
  (with-html (:script :type "application/javascript"
                      :src (url "lib" path))))



;;; HTML Head

(defun head-js-std ()
  (mapc #'js '("lib/jquery/jquery-1.4.2.min.js"
               "lib/jquery-ui/js/jquery-ui-1.8.2.custom.min.js"
               "js/main.js")))


(defun head-css-std ()
  (mapc #'css '("css/reset.css"
                "css/main.css"
                "lib/jquery-ui/css/smoothness/jquery-ui-1.8.2.custom.css")))

(defun head-config ()
  (head-css-std)
  (css '"css/table.css")
  (head-js-std))



;;; User interface elements

(defun ok-button ()
  (with-html
    (:button :type "submit" (img "tick.png"))))

(defun cancel-button (href)
  (with-html
    (:a :href href (img "cancel.png"))))

(defun logo ()
  (with-html
    (:h1 "Scrooge")))

(defun footer ()
  (with-html
    (:div :id "footer" "Powered by lisp")))



;;; CRUD actions menu

(defun standard-actions-spec (view create update delete)
  `((view   ,view   "Προβολή"     "magnifier.png")
    (create ,create "Δημιουργία"  "add.png")
    (update ,update "Επεξεργασία" "pencil.png")
    (delete ,delete "Διαγραφή"    "delete.png")))

;; (defun actions-menu ()
;;   (generic-menu :div-style "actions"
;;                 :ul-style "hmenu"))



;;; CRUD Tables

;; (defun mkfn-crud-row-controls-p (op)
;;   (mkfn-row-controls-p op '(create update delete)))

;; (defun mkfn-crud-row-readonly-p (op)
;;   (mkfn-row-readonly-p op
;;                        '(view delete)
;;                        '(create update)))

;; (defun mkfn-crud-row (row-id-fn row-payload-fn
;;                       row-selected-p-fn row-controls-p-fn row-readonly-p-fn
;;                       selector-states-fn cancel-url)
;;   (html (row-data)
;;     (let* ((id (funcall row-id-fn row-data))
;;            (row-selected-p (funcall row-selected-p-fn id))
;;            (row-controls-p (funcall row-controls-p-fn row-selected-p))
;;            (row-readonly-p (funcall row-readonly-p-fn row-selected-p)))
;;       (htm (:tr :class (if row-selected-p "active" nil)
;;                 (:td (funcall (selector-link (funcall selector-states-fn id))
;;                               row-selected-p))
;;                 (plist-map (lambda (key value)
;;                              (if row-readonly-p
;;                                  (htm (:td (str value)))
;;                                  (htm (:td (textbox (symbolicate key)
;;                                                     :value value
;;                                                     :style nil))))) ;; todo -- style missing
;;                            (funcall row-payload-fn row-data row-readonly-p))
;;                 (:td (ok-link row-controls-p))
;;                 (:td (cancel-link cancel-url row-controls-p)))))))
