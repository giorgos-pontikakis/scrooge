(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; ------------------------------------------------------------
;;; Static references
;;; ------------------------------------------------------------

(defun css (file)
  (with-html
    (:link :href file
           :rel "stylesheet"
           :type "text/css")))

(defun js (file)
  (with-html
    (:script :type "text/javascript"
             :src file)))

(defun img (file)
  (with-html
    (:img :src (url 'img file))))



;;; ------------------------------------------------------------
;;; HTML Head
;;; ------------------------------------------------------------

(defun jquery ()
  (js (url 'lib "jquery-1.4.4/jquery-1.4.4.min.js")))

(defun 960gs ()
  (mapc #'(lambda (filename)
            (css (url 'lib "960gs/code/css/" filename ".css")))
        (list "reset" "960" "text")))

(defun error-headers ()
  (css (url 'css "global.css")))

(defun global-headers ()
  (jquery)
  (960gs)
  (css (url 'css "global.css")))



;;; ------------------------------------------------------------
;;; User interface elements
;;; ------------------------------------------------------------

(defun ok-button ()
  (with-html
    (:button :type "submit" (img "tick.png"))))

(defun cancel-button (href)
  (with-html
    (:a :href href (img "cancel.png"))))

(defun logo ()
  (with-html
    (:div :id "logo"
          :class "grid_2"
          (:h1 "Scrooge"))))

(defun header (active-item)
  (with-html
    (:div :id "header"
          (logo)
          (primary-navbar active-item)
          (logout-menu))))

(defun footer ()
  (with-html
    (:div :id "footer" "Powered by lisp")))

(defun primary-navbar (active-page-name)
  (display (make-instance 'horizontal-navbar
                          :id "navbar"
                          :style "hnavbar grid_8 prefix_1"
                          :spec '((home   "Αρχική")
                                  (main   "Οικονομικά")
                                  (admin  "Διαχείριση")
                                  (config "Ρυθμίσεις")))
           :active-page-name active-page-name))

(defun logout-menu ()
  (display (make-instance 'menu
                          :id "logout"
                          :style "hnavbar grid_1"
                          :spec `((logout "" "Έξοδος")))))

(defun main-menu (active-page-name)
  (display (make-instance 'horizontal-navbar
                          :id "main-navbar"
                          :spec '((companies "Εταιρίες")
                                  (projects "Έργα")
                                  (transactions "Συναλλαγές")))
           :active-page-name active-page-name))



;;; ------------------------------------------------------------
;;; CRUD actions menu
;;; ------------------------------------------------------------

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
