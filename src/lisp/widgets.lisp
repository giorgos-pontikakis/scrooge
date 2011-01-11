(in-package :scrooge)




(defclass widget ()
  ((id    :accessor id    :initarg :id)
   (style :accessor style :initarg :style))
  (:default-initargs :id nil :style nil))



;;; ------------------------------------------------------------
;;; CRUD Collections
;;; ------------------------------------------------------------

(defclass collection (widget)
  ((op     :accessor op     :initarg :op)
   (filter :accessor filter :initarg :filter)))

(defgeneric read-items (collection))

(defgeneric insert-item (collection &key)
  (:documentation "Insert an new item to the collection"))

(defgeneric update-item (collection &key)
  (:documentation "Update an item of the collection"))



;;; ------------------------------------------------------------
;;; CRUD Collection Items
;;; ------------------------------------------------------------

(defclass item ()
  ((collection :accessor collection :initarg :collection)
   (key        :accessor key        :initarg :key)
   (record     :accessor record     :initarg :record)))

(defclass node (item)
  ((parent-key :accessor parent-key :initarg :parent-key)
   (children   :accessor children   :initarg :children)))

(defgeneric find-node (node key))

(defmethod find-node ((root node) key)
  (find-node-rec key (list root)))

(defun find-node-rec (target-key fringe)
  (let ((node (first fringe)))
    (cond
      ;; fringe exhausted, target not found
      ((null node)
       nil)
      ;; target found
      ((equal (key node) target-key)
       node)
      ;; expand fringe and continue (depth-first)
      (t
       (find-node-rec target-key
                      (append (children node) (rest fringe)))))))

(defclass row (item)
  ((index :accessor index :initarg :index)))

(defgeneric selected-p (item selected-id)
  (:documentation "Returns T if the item is selected."))

(defgeneric readonly-p (item selected-id)
  (:documentation "Returns T if the item is readonly."))

(defgeneric controls-p (item selected-id)
  (:documentation "Returns T if the item has active controls."))

(defgeneric cells (item &key)
  (:documentation "Returns a property list which contains the ids of
  the various cells as keys and lists of cell objects as the
  corresponding values."))

(defmethod selected-p ((item item) selected-id)
  (equal (key item) selected-id))

(defmethod readonly-p ((item item) selected-id)
  (or (not (controls-p item selected-id))
      (member (op (collection item)) '(view delete))))



;;; ------------------------------------------------------------
;;; Trees
;;; ------------------------------------------------------------

(defclass crud-tree (collection)
  ((root :accessor root :initarg :root))
  (:default-initargs :id "crud-table" :style "crud-table"))

(defmethod initialize-instance :after ((tree crud-tree) &key)
  (setf (slot-value tree 'root)
        (read-items tree)))

(defmethod update-item ((tree crud-tree) &key record key)
  (let ((node (find-node (root tree) key)))
    (setf (record node)
          (plist-union record (record node)))))


(defmethod display ((tree crud-tree) &key selected-id selected-data)
  (when (and (eq (op tree) 'create)
             (null selected-id))
    (insert-item tree
                 :record selected-data
                 :parent-key nil))
  (with-html
    (:ul :class "crud-tree"
         (mapc (lambda (node)
                 (display node
                          :selected-id selected-id
                          :selected-data selected-data))
               (children (root tree))))))



;;; ------------------------------------------------------------
;;; CRUD NODE
;;; ------------------------------------------------------------

(defclass crud-node (node)
  ())

(defmethod controls-p ((item crud-node) selected-id)
  (let ((parent-item (find-node (root (collection item)) (parent-key item))))
    (or
     ;; update or delete
     (and (member (op (collection item)) '(update delete))
          (selected-p item selected-id))
     ;; create
     (and (eql (key item) (parent-key item)) ;; this implies create
          (selected-p parent-item selected-id)))))

(defmethod display ((node crud-node) &key selected-id selected-data)
  (let ((selected-p (selected-p node selected-id))
        (tree (collection node)))
    (with-html
      (:li :class (if selected-p
                      (if (eq (op tree) 'delete)
                          "attention"
                          "selected")
                      nil)
           (:span :class "selector"
                  (display (getf (cells node) :selector)
                           :state (if selected-p :on :off)))
           (:span :class "payload"
                  (display (getf (cells node) :payload)
                           :readonlyp (readonly-p node selected-id)))
           (mapc (lambda (cell)
                   (htm (:span :class "pushbutton"
                               (display cell :activep (controls-p node selected-id)))))
                 (getf (cells node) :controls))
           ;; Create
           (when (and selected-p
                      (eql (op tree) 'create))
             (insert-item tree
                          :record selected-data
                          :parent-key selected-id))
           ;; Update
           (when (and selected-p
                      (eql (op tree) 'update))
             (update-item tree
                          :record selected-data
                          :key selected-id))
           ;; Continue with children
           (when (children node)
             (htm (:ul :class "indent"
                       (mapc (lambda (node)
                               (display node
                                        :selected-id selected-id
                                        :selected-data selected-data))
                             (children node)))))))))



;;; ------------------------------------------------------------
;;; TABLES
;;; ------------------------------------------------------------

(defclass crud-table (collection)
  ((header-labels :accessor header-labels :initarg :header-labels)
   (paginator     :reader   paginator)
   (rows          :accessor rows))
  (:default-initargs :id "crud-table" :style "crud-table"))

(defmethod initialize-instance :after ((table crud-table) &key)
  (setf (slot-value table 'rows)
        (read-items table))
  (when-let (pg (paginator table))
    (setf (slot-value pg 'table)
          table)))

(defmethod update-item ((table crud-table) &key record index)
  (let ((row (nth index (rows table))))
    (setf (record row)
          (plist-union record (record row)))))

(defmethod display ((table crud-table) &key selected-id selected-data start)
  (let ((selected-row (find selected-id (rows table) :key #'key :test #'equal)))
    (let* ((index (if selected-row (index selected-row) nil))
           (pg (paginator table))
           (page-start (page-start pg index start)))
      ;; Create
      (when (eq (op table) 'create)
        (insert-item table
                     :record selected-data
                     :index 0))
      ;; Update
      (when (eq (op table) 'update)
        (update-item table
                     :record selected-data
                     :index index))
      ;; Finally display paginator and table
      (with-html
        (display pg :start page-start)
        (:table :id (id table) :class (style table)
                (:thead (:tr (mapc (lambda (i)
                                     (htm (:th (str i))))
                                   (header-labels table))))
                (:tbody
                 (iter (for row in (subseq (rows table)
                                           page-start
                                           (min (+ page-start (delta pg))
                                                (length (rows table)))))
                       (display row
                                :selected-id selected-id
                                :start start))))))))



;;; ------------------------------------------------------------
;;; CRUD ROW
;;; ------------------------------------------------------------

(defclass crud-row (row)
  ())

(defmethod controls-p ((item row) selected-id)
  (and (selected-p item selected-id)
       (member (op (collection item)) '(create update delete))))

(defmethod display ((row crud-row) &key selected-id start)
  (let ((selected-p (selected-p row selected-id))
        (cells (cells row :start start)))
    (with-html
      (:tr :class (if selected-p
                      (if (eq (op (collection row)) 'delete)
                          "attention"
                          "selected")
                      nil)
           (:td :class "selector"
                (display (getf cells :selector)
                         :state (if (selected-p row selected-id) :on :off)))
           (mapc (lambda (cell)
                   (htm (:td :class "payload"
                             (display cell
                                      :readonlyp (readonly-p row selected-id)))))
                 (ensure-list (getf cells :payload)))
           (mapc (lambda (cell)
                   (htm (:td :class "pushbutton"
                             (display cell :activep (controls-p row selected-id)))))
                 (getf cells :controls))))))



;;; ------------------------------------------------------------
;;; TABLE PAGINATOR
;;; ------------------------------------------------------------

(defclass paginator (widget)
  ((table :accessor table :initarg :table)
   (delta :accessor delta :initarg :delta)
   (urlfn :accessor urlfn :initarg :urlfn)))

(defgeneric page-start (paginator index start))

(defmethod page-start ((pg paginator) index start)
  (if (null index)
      (if (or (null start)
              (< start 0)
              (> start (length (rows (table pg)))))
          0
          start)
      (let ((delta (delta pg)))
        (* (floor (/ index delta))
           delta))))

(defmethod display ((pg paginator) &key (start 0))
  (let* ((delta (delta pg))
         (len (length (rows (table pg))))
         (prev (if (>= (- start delta) 0)
                   (- start delta)
                   (if (> start 0)
                       0
                       nil)))
         (next (if (<= (+ start delta) (1- len))
                   (+ start delta)
                   nil)))
    (with-html
      (:div :id (id pg) :class (style pg)
            (fmt "Εγγραφές ~A–~A από ~A"
                 (1+ start)
                 (min (+ start delta) len)
                 len)
            (if prev
                (htm (:a :href (funcall (urlfn pg) (filter (table pg)) prev)
                         (img "resultset_previous.png" )))
                (img "resultset_first.png"))
            (if next
                (htm (:a :href (funcall (urlfn pg) (filter (table pg)) next)
                         (img "resultset_next.png" )))
                (img "resultset_last.png"))))))




;;; ------------------------------------------------------------
;;; CELLS
;;; ------------------------------------------------------------

(defclass textbox-cell (widget)
  ((name  :accessor name  :initarg :name)
   (value :accessor value :initarg :value)))

(defmethod display ((cell textbox-cell) &key readonlyp)
  (if readonlyp
      (with-html
        (:div :class (style cell)
              (str (lisp->html (value cell)))))
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
             (img (if (eql state :on)
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
               (string-downcase (error-type param)))))
    (unless (every #'validp params)
      (with-html
        (:ul :id (id messenger)
             (iter (for p in params)
                   (unless (validp p)
                     (htm (:li :class (style messenger)
                               (str (get-message p (messages messenger))))))))))))
