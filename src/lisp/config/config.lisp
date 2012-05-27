(in-package :scrooge)


(defclass config-family (family-mixin)
  ())

(defmethod top-level-actions ((widget config-family))
  (top-actions
   (make-instance 'menu
                  :spec `((create ,(html ()
                                     (:a :href (apply (action-url-fn widget :create) (filter widget))
                                         (:img :src "/scrooge/img/add.png"
                                               (str (action-label widget :create)))))))
                  :css-class "hmenu")
   (searchbox (action-url-fn widget :catalogue)
              #'(lambda (&rest args)
                  (apply (action-url-fn widget :catalogue)
                         (make-keyword (first (getf (parameter-groups widget) :system)))
                         (selected-key widget)
                         args))
              (filter widget)
              (ac-class widget))))


;;; config crud table and row classes

(defclass config-table (scrooge-table)
  ())

(defclass config-row (scrooge-row/obj)
  ())

(defmethod payload ((row config-row) enabled-p)
  (make-instance 'textbox
                 :name 'title
                 :value (title (record row))
                 :disabled (not enabled-p)))

;;; Config utilities

(defun config-data (table-name search)
  (select-dao table-name (:ilike 'title (ilike search)) 'title))



;;; UI elements

(defun config-navbar (&optional active)
  (with-html
    (:div :class "grid_12"
          (:div :class "section-navbar"
                (navbar `((city          ,(config/city)                      "Πόλεις")
                          (bank          ,(config/bank)                      "Τράπεζες")
                          (tof           ,(config/tof)                       "Δ.Ο.Υ.")
                          (account       ,(config/account)                   "Λογαριασμοί")
                          (account-role  ,(config/account-role)              "Ρόλοι Λογαριασμών")
                          (temtx         ,(config/temtx)                     "Πρότυπα Συναλλαγών")
                          (cheque-stran  ,(config/cheque-stran "receivable") "Επιταγές"))
                        :css-class "hnavbar"
                        :active active)))))
