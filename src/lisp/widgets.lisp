(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))


(defclass widget ()
  ((id    :accessor id    :initarg :id)
   (style :accessor style :initarg :style)))



;;; ------------------------------------------------------------
;;; CRUD Collections
;;; ------------------------------------------------------------

(defclass crud-collection (widget)
  ((op      :accessor op      :initarg :op)
   (filter  :accessor filter  :initarg :filter)
   (db-data :reader   db-data)
   (rows    :reader   rows)))

(defmethod initialize-instance :after ((collection crud-collection) &key)
  ;; First store db-data
  (setf (slot-value collection 'db-data)
        (read-data collection))
  ;; Then store row objects
  (setf (slot-value collection 'rows)
        (mapcar (lambda (row-data)
                  (make-row collection row-data))
                (slot-value collection 'db-data))))


(defgeneric read-data (crud-collection))
(defgeneric make-row (crud-collection data))



;;; ------------------------------------------------------------
;;; CRUD Collection Items
;;; ------------------------------------------------------------

(defgeneric selected-p (row selected-id)
  (:documentation "Returns T if the row is selected."))

(defgeneric readonly-p (row selected-id)
  (:documentation "Returns T if the row is readonly."))

(defgeneric controls-p (row selected-id)
  (:documentation "Returns T if the row has active controls."))

(defgeneric cells (row)
  (:documentation "Returns a property list which contains the ids of
  the various cells as keys and lists of cell objects as the
  corresponding values."))

(defgeneric get-id (row)
  (:documentation "Returns the id of the row, which should be
  comparable with the selected-id slot of the row's collection."))

(defgeneric get-payload (row))


(defclass crud-row ()
  ((collection :accessor collection :initarg :collection)
   (data       :accessor data       :initarg :data)))

(defmethod selected-p ((row crud-row) selected-id)
  (eql (get-id row) selected-id))

(defmethod readonly-p ((row crud-row) selected-id)
  (or (not (selected-p row selected-id))
      (member (op (collection row)) '(view delete))))

(defmethod controls-p ((row crud-row) selected-id)
  (and (selected-p row selected-id)
       (member (op (collection row)) '(create update delete))))




;;; ------------------------------------------------------------
;;; Trees
;;; ------------------------------------------------------------

(defclass crud-tree (crud-collection)
  ((parent   :accessor parent   :initarg :parent)
   (children :accessor children :initarg :children)
   (content  :accessor content  :initarg :content)))

(defgeneric db-data->tree-data (tree))



(defmethod display ((tree crud-tree) &key selected-id)
  (with-html
    (:ul :style (style tree)
         (mapc (lambda (x)
                 (htm (:li (display x))))
               (content tree)))))



;;; ------------------------------------------------------------
;;; CRUD Tables
;;; ------------------------------------------------------------

(defclass crud-table (crud-collection)
  ((header-labels :accessor header-labels :initarg :header-labels))
  (:default-initargs :id "crud-table" :style "crud-table"))

(defgeneric paginator (table))

(defmethod initialize-instance :after ((table crud-table) &key)
  ;; First store db-data
  (setf (slot-value table 'db-data)
        (read-data table))
  ;; Then store row objects
  (setf (slot-value table 'rows)
        (mapcar (lambda (row-data)
                  (make-row table row-data))
                (slot-value table 'db-data))))

(defmethod display ((table crud-table) &key selected-id selected-data start)
  (let* ((pg (paginator table))
         (start (if (null selected-id)
                    (if (or (null start)
                            (< start 0)
                            (> start (len pg)))
                        0
                        start)
                    (pg-start table pg selected-id)))
         (pg-rows (pg-rows table pg start)))
    (when (eq (op table) 'create)
      (push (make-row table selected-data) pg-rows))
    (when (eq (op table) 'update)
      (let ((row (find selected-id (rows table) :key #'get-id)))
        (setf (data row)
              (plist-union selected-data (data row)))))
    (with-html
      (display (paginator table) :start start)
      (:table :id (id table) :class (style table)
              (:thead (:tr (mapc (lambda (i)
                                   (htm (:th (str i))))
                                 (header-labels table))))
              (:tbody
               (iter (for r in pg-rows)
                     (display r :selected-id selected-id)))))))



;;; ------------------------------------------------------------
;;; Table Paginator
;;; ------------------------------------------------------------

(defclass paginator (widget)
  ((delta :accessor delta :initarg :delta)
   (urlfn :accessor urlfn :initarg :urlfn)
   (len   :accessor len   :initarg :len)))

(defmethod display ((pg paginator) &key (start 0))
  (let* ((delta (delta pg))
         (prev (if (>= (- start delta) 0)
                   (- start delta)
                   (if (> start 0)
                       0
                       nil)))
         (next (if (<= (+ start delta) (1- (len pg)))
                   (+ start delta)
                   nil)))
    (with-html
      (:div :id (id pg) :class (style pg)
            (fmt "Εγγραφές ~A–~A από ~A"
                 (1+ start)
                 (min (+ start delta) (len pg))
                 (len pg))
            (if prev
                (htm (:a :href (funcall (urlfn pg) prev)
                         (img "resultset_previous.png" )))
                (img "resultset_first.png"))
            (if next
                (htm (:a :href (funcall (urlfn pg) next)
                         (img "resultset_next.png" )))
                (img "resultset_last.png"))))))



;;; ------------------------------------------------------------
;;; Table - Paginator methods
;;; ------------------------------------------------------------

(defgeneric pg-rows (table paginator start))

(defmethod pg-rows ((table crud-table) (pg paginator) start)
  (subseq (rows table)
          (max start 0)
          (min (+ start (delta pg)) (length (db-data table)))))


(defgeneric pg-start (crud-table paginator selected-id))

(defmethod pg-start ((table crud-table) (pg paginator) selected-id)
  (let ((pos (or (position selected-id (db-data table) :key (lambda (db-row)
                                                              (getf db-row :id)))
                 0))
        (delta (delta pg)))
    (* (floor (/ pos delta))
       delta)))



;;; ------------------------------------------------------------
;;; Cells
;;; ------------------------------------------------------------

(defclass textbox-cell (widget)
  ((name  :accessor name  :initarg :name)
   (value :accessor value :initarg :value)))

(defmethod display ((cell textbox-cell) &key readonlyp)
  (if readonlyp
      (with-html
        (:div :class (style cell)
             (str (value cell))))
      (with-html
        (:div :class (style cell)
             (textbox (name cell)
                      :readonlyp readonlyp
                      :value (value cell))))))



(defclass selector-cell (widget)
  ((states :accessor states :initarg :states)))

(defmethod display ((cell selector-cell) &key state)
  (with-html
    (:div :class (style cell)
         (:a :href (getf (states cell) state)
             (img (if (eq state :on)
                      "bullet_red.png"
                      "bullet_blue.png"))))))



(defclass ok-cell (widget)
  ())

(defmethod display ((cell ok-cell) &key activep)
  (if activep
      (with-html
        (:div :class (style cell)
             (submit (html ()
                       (img "tick.png")))))
      (with-html
        (:div :class (style cell)
             ""))))



(defclass cancel-cell (widget)
  ((href  :accessor href  :initarg :href)))

(defmethod display ((cell cancel-cell) &key activep)
  (if activep
      (with-html
        (:div :class (style cell)
              (:a :href (href cell)
                  (img "cancel.png"))))
      (with-html
        (:div :class (style cell)
              ""))))



;;; ----------------------------------------------------------------------
;;; NAVBARS
;;;
;;; A navbar is a unordered list of anchors. One of them may be
;;; active, i.e. its class is active-style instead of
;;; inactive-style. The idea is to represent the link to the page we
;;; are currently viewing with a separate style.
;;; ----------------------------------------------------------------------

(defclass navbar (widget)
  ((spec  :accessor spec  :initarg :spec)))

(defmethod display ((navbar navbar) &key active-page-name)
  (with-html
    (:div :id (id navbar) :class (style navbar)
          (:ul
           (iter (for (page-id label) in (spec navbar))
                 (htm (:li (if (eql page-id active-page-name)
                               (htm (:p (str label)))
                               (htm (:a :href (if (fboundp page-id)
                                                  (funcall page-id)
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
  ((spec  :accessor spec  :initarg :spec)))

(defmethod display ((menu menu) &key disabled-items)
  (with-html
    (:div :id (id menu)
          :class (style menu)
          (:ul
           (iter (for (action-id href label) in (spec menu))
                 (unless (or (member action-id disabled-items)
                             (null href))
                   (htm (:li (:a :href href
                                 :class (string-downcase action-id)
                                 (str label))))))))))

(defclass actions-menu (menu)
  ((style :initform "hnavbar actions")))



;;; ------------------------------------------------------------
;;; MESSENGER
;;; ------------------------------------------------------------

(defclass messenger (widget)
  ((messages :accessor messages :initarg :messages))
  (:default-initargs :id nil))

(defmethod display ((messenger messenger) &key params)
  (flet ((get-message (param messages)
           (or (second (assoc (error-type param)
                              (getf messages (name param))))
               "Internal error: Unknown message in messenger widget.")))
    (unless (every #'validp params)
      (with-html
        (:ul :id (id messenger)
             (iter (for p in params)
                   (unless (validp p)
                     (htm (:li :class (style messenger)
                               (str (get-message p (messages messenger))))))))))))
