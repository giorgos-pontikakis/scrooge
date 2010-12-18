(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; ------------------------------------------------------------
;;; Rows
;;; ------------------------------------------------------------

(defgeneric selected-p (row)
  (:documentation "Returns T if the row is selected."))

(defgeneric readonly-p (row)
  (:documentation "Returns T if the row is readonly."))

(defgeneric controls-p (row)
  (:documentation "Returns T if the row has active controls."))

(defgeneric cells (row)
  (:documentation "Returns a property list which contains the names of
  the various cells as keys and lists of cell objects as the
  corresponding values."))

(defgeneric get-id (row)
  (:documentation "Returns the id of the row, which should be
  comparable with the selected-id slot of the row's table."))

(defclass crud-row ()
  ((table :accessor table :initarg :table)
   (data  :accessor data  :initarg :data)))

(defmethod selected-p ((row crud-row))
  (eql (get-id row)
       (selected-id (table row))))

(defmethod readonly-p ((row crud-row))
  (or (not (selected-p row))
      (member (op (table row)) '(view delete))))

(defmethod controls-p ((row crud-row))
  (and (selected-p row)
       (member (op (table row)) '(create update delete))))



;;; ------------------------------------------------------------
;;; Tables
;;; ------------------------------------------------------------

(defclass crud-table (widget)
  ((id            :accessor id            :initarg :id)
   (op            :accessor op            :initarg :op)
   (selected-id   :accessor selected-id   :initarg :selected-id)
   (filter        :accessor filter        :initarg :filter)
   (header-labels :accessor header-labels)
   (db-table-fn   :accessor db-table-fn)
   (row-class     :accessor row-class))
  (:default-initargs :id "crud-table"))

(defmethod display ((table crud-table) &key start)
  (let* ((db-data (funcall (db-table-fn table) (filter table)))
         (len (length db-data))
         (rows (mapcar (lambda (db-row)
                         (make-instance (row-class table)
                                        :table table
                                        :data db-row))
                       (if (not start)
                           db-data
                           (subseq db-data (min (1- start) len) (min (+ start 10) len))))))
    (when (eq (op table) 'create)
      (push (make-instance (row-class table) :table table :data ())
            rows))
    (with-html
      (:table :id (id table) :class "crud-table"
              (:thead (:tr (mapc (lambda (i)
                                   (htm (:th (str i))))
                                 (header-labels table))))
              (:tbody
               (iter (for r in rows)
                     (display r)))))))



;;; ------------------------------------------------------------
;;; Cells
;;; ------------------------------------------------------------

(defclass textbox-cell (widget)
  ((name  :accessor name  :initarg :name)
   (value :accessor value :initarg :value)))

(defmethod display ((cell textbox-cell) &key readonlyp)
  (if readonlyp
      (with-html
        (str (value cell)))
      (textbox (name cell)
               :readonlyp readonlyp
               :value (value cell))))



(defclass selector-cell (widget)
  ((states :accessor states :initarg :states)))

(defmethod display ((cell selector-cell) &key state)
  (with-html
    (:a :href (getf (states cell) state)
        (img (if (eq state :on)
                 "bullet_red.png"
                 "bullet_blue.png")))))



(defclass ok-cell (widget)
  ())

(defmethod display ((cell ok-cell) &key activep)
  (if activep
      (with-html
        (submit (html ()
                  (img "tick.png"))))
      (with-html
        "")))



(defclass cancel-cell (widget)
  ((href :accessor href :initarg :href)))

(defmethod display ((cell cancel-cell) &key activep)
  (if activep
      (with-html
        (:a :href (href cell)
            (img "cancel.png")))
      (with-html
        "")))



;;; ----------------------------------------------------------------------
;;; NAVBARS
;;;
;;; A navbar is a unordered list of anchors. One of them may be
;;; active, i.e. its class is active-style instead of
;;; inactive-style. The idea is to represent the link to the page we
;;; are currently viewing with a separate style.
;;; ----------------------------------------------------------------------

(defclass navbar (widget)
  ((id             :accessor id             :initarg :id)
   (spec           :accessor spec           :initarg :spec)
   (style          :accessor style          :initarg :style)))

(defmethod display ((navbar navbar) &key active-page-name)
  (with-html
    (:div :id (id navbar) :class (style navbar)
          (:ul
           (iter (for (page-name label) in (spec navbar))
                 (htm (:li (if (eql page-name active-page-name)
                               (htm (:p (str label)))
                               (htm (:a :href (if (fboundp page-name)
                                                  (funcall page-name)
                                                  (error-page))
                                        (str label)))))))))))

(defclass horizontal-navbar (navbar)
  ((active-style   :initform "active")
   (inactive-style :initform nil)
   (style          :initform "hnavbar")))

(defclass vertical-navbar (navbar)
  ((active-style   :initform "active")
   (inactive-style :initform nil)
   (style          :initform "vnavbar")))



;;; ----------------------------------------------------------------------
;;; MENUS
;;;
;;; A menu is an unordered list of anchors. Some may be disabled.
;;; ----------------------------------------------------------------------

(defclass menu (widget)
  ((id        :accessor id        :initarg :id)
   (style     :accessor style     :initarg :style)
   (spec      :accessor spec      :initarg :spec)))

(defmethod display ((menu menu) &key disabled-items)
  (with-html
    (:div :id (id menu)
          :class (style menu)
          (:ul
           (iter (for (action-name href label) in (spec menu))
                 (unless (or (member action-name disabled-items)
                             (null href))
                   (htm (:li (:a :href href
                                 :class (string-downcase action-name)
                                 (str label))))))))))



(defclass actions-menu (menu)
  ((style :initform "hnavbar actions")))



;;; ------------------------------------------------------------
;;; MESSENGER
;;; ------------------------------------------------------------

(defclass messenger (widget)
  ((style    :accessor style    :initarg :style)
   (messages :accessor messages :initarg :messages)))

(defmethod display ((messenger messenger) &key params)
  (flet ((get-message (param messages)
           (or (second (assoc (error-type param)
                              (getf messages (name param))))
               "Internal error: Unknown message in messenger widget.")))
    (unless (every #'validp params)
      (with-html
        (:ul (iter (for p in params)
                   (unless (validp p)
                     (htm (:li :class (style messenger)
                               (str (get-message p (messages messenger))))))))))))

(defun messenger (message-spec &optional style)
  (make-instance 'messenger
                 :messages message-spec
                 :style style))



;; ;;; ------------------------------------------------------------
;; ;;; Forms
;; ;;; ------------------------------------------------------------

;; (defun form (submit-page hidden body)
;;   (let ((page (find-page submit-page '*webapp*)))
;;     (with-html
;;       (:form :method (request-type page)
;;              :action (concatenate 'string (webroot (webapp page)) (base-url page))
;;              (iter (for key in hidden by #'cddr)
;;                    (for val in (rest hidden) by #'cddr)
;;                    (htm
;;                      (:input :type "hidden"
;;                              :id (string-downcase key)
;;                              :style "display: none;"
;;                              :name (string-downcase key)
;;                              :value (lisp->html val))))
;;              (render body)))))

;; (defmacro with-form (url &body body)
;;   (let ((page-name (first url))
;;         (hidden (rest url)))
;;     `(form ',page-name (list ,@hidden)
;;            ,@body)))


;; (defun textbox (name &key id style readonlyp disabledp passwordp value)
;;   (with-html
;;     (:input :id id
;;             :class style
;;             :type (if passwordp "password" "text")
;;             :name (string-downcase name)
;;             :value (lisp->html (or value :null))
;;             :readonly readonlyp
;;             :disabled disabledp)))

;; (defun radio (name label-value-alist &key id style readonlyp disabledp checked)
;;   (with-html
;;     (:ul :id (or id (string-downcase name))
;;          :class style
;;          (iter (for (label value) in label-value-alist)
;;                (htm (:li (:input :type "radio"
;;                                  :name (string-downcase name)
;;                                  :value (lisp->html value)
;;                                  :checked (equal value checked)
;;                                  :readonly readonlyp
;;                                  :disabled disabledp)
;;                          (render label)))))))

;; (defun dropdown (name label-value-alist &key style readonlyp disabledp selected)
;;   (with-html
;;     (:select :id (string-downcase name)
;;              :class style
;;              :name (string-downcase name)
;;              :disabled disabledp
;;              (iter (for (label value) in label-value-alist)
;;                    (htm (:option :value (lisp->html value)
;;                                  :selected (equal value selected)
;;                                  :readonly readonlyp
;;                                  (render label)))))))

;; (defun label (name text &key style)
;;   (with-html
;;     (:label :class style
;;             :for (string-downcase name)
;;             (render text))))

;; (defun submit (label &key name value style disabledp)
;;   (with-html
;;     (:button :class style
;;              :type "submit"
;;              :name (if name (string-downcase name) nil)
;;              :value (if value (lisp->html value) nil)
;;              :disabled disabledp
;;              (render label))))



;; ;;; ------------------------------------------------------------
;; ;;; Tables
;; ;;; ------------------------------------------------------------

;; (defun thead (&rest args)
;;   (with-html
;;     (:thead
;;      (:tr (mapc (lambda (item)
;;                   (htm (:th (str item))))
;;                 args)))))



;; ;;; ------------------------------------------------------------
;; ;;; Links
;; ;;; ------------------------------------------------------------

;; (defun selector-link (states)
;;   (html (state)
;;     (:a :href (second (assoc state states))
;;         (img (if (true state)
;;                  "bullet_red.png"
;;                  "bullet_blue.png")))))

;; (defun arrow ()
;;   (html ()
;;     (:a :href )))

;; (defun ok-link (visiblep)
;;   (if visiblep
;;       (with-html
;;         (submit (html ()
;;                   (img "tick.png"))))
;;       (with-html
;;         "")))

;; (defun cancel-link (href visiblep)
;;   (if visiblep
;;       (with-html
;;         (:a :href href
;;             (img "cancel.png")))
;;       (with-html
;;         "")))



;; ;;; ------------------------------------------------------------
;; ;;; Table with inline form
;; ;;; ------------------------------------------------------------

;; (defun mkfn-row-selected-p (id-keys)
;;   (lambda (id)
;;     (let ((result (mapcar (lambda (key)
;;                             (eql (val* (find-parameter key))
;;                                  (getf id key) #|(find-datum id key)|#))
;;                           id-keys)))
;;       (every #'true result))))

;; (defun mkfn-row-controls-p (op form-ops)
;;   (lambda (selected-p)
;;     (and selected-p
;;          (member op form-ops))))

;; (defun mkfn-row-readonly-p (op ro-ops rw-ops)
;;   (cond ((member op ro-ops)
;;          (constantly t))
;;         ((member op rw-ops)
;;          (lambda (selected-p)
;;            (not selected-p)))
;;         (t (error "Unknown operation: ~A" op))))

;; (defun mkfn-row-id (id-keys)
;;   (lambda (row-data)
;;     (plist-collect id-keys row-data)))

;; (defun mkfn-row-payload (payload-keys)
;;   (lambda (row-data readonly-p)
;;     (if readonly-p
;;         (plist-collect payload-keys row-data)
;;         (plist-union (plist-collect payload-keys (params->plist (parameters *page*)))
;;                      (plist-collect payload-keys row-data)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defgeneric renderer (widget)
;;   (:documentation "Funcall it to render the widget."))



;; ;;; ------------------------------------------------------------
;; ;;; Tables interface
;; ;;; ------------------------------------------------------------

;; (defclass crud-table ()
;;   ((header          :accessor header          :initarg :header)
;;    (operation       :accessor operation       :initarg :operation)
;;    (cancel-href     :accessor cancel-href     :initarg :cancel-href)
;;    (id-fn           :accessor id-fn           :initarg :id-fn)
;;    (payload-fn      :accessor payload-fn      :initarg :payload-fn)
;;    (data-fn         :accessor data-fn         :initarg :data-fn)
;;    (selector-states :accessor selector-states :initarg :selector-states)))

;; (defmethod renderer ((table crud-table))
;;   ())




;; ;;; ------------------------------------------------------------
;; ;;; Rows interface
;; ;;; ------------------------------------------------------------

;; (defclass crud-row ()
;;   ((id      :accessor id      :initarg :id)
;;    (payload :accessor payload :initarg :payload)))

;; (defgeneric activep-fn (row &key)
;;   (:documentation
;;    "Returns T if it is active, NIL otherwise. This should depend on
;;    the data of the table and other parameters given via keyword
;;    arguments. It should be defined separately for each table."))


;; (defgeneric readonlyp (row)
;;   (:documentation "This method return the read-only status of the row."))

;; (defmethod readonlyp ((row crud-row))
;;   (if (funcall (activep-fn row))
;;       (member (operation (table row)) '(view delete))
;;       nil))

;; (defmethod renderer ((row crud-row))
;;   (html ()
;;     (:tr :class (if (readonlyp row) "active" nil)
;;          (mapc #'render (cells row)))))
