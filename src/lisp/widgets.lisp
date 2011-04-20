(in-package :scrooge)



;;; collections

(defclass scrooge-crud-tree (crud-tree)
  ()
  (:default-initargs :style "crud-tree"))

(defclass scrooge-crud-node (crud-node)
  ()
  (:default-initargs :css-delete "attention"
                     :css-selected "selected"
                     :css-selector "selector"
                     :css-payload "payload"
                     :css-controls "controls"
                     :css-indent "indent"))


(defclass scrooge-crud-table (crud-table)
  ()
  (:default-initargs :style "crud-table"))

(defclass scrooge-crud-row (crud-row)
  ()
  (:default-initargs :css-delete "attention"
                     :css-selected "selected"
                     :css-selector "selector"
                     :css-payload "payload"
                     :css-controls "controls"))

(defclass scrooge-paginator (paginator)
  ()
  (:default-initargs
    :delta (get-option "crud-table-num-rows")
    :html-prev (html () (img "resultset_previous.png"))
    :html-next (html () (img "resultset_next.png"))
    :html-prev-inactive (html () (img "resultset_first.png"))
    :html-next-inactive (html () (img "resultset_last.png"))))



;;; buttons

(defclass ok-button (submit)
  ()
  (:default-initargs :content (html ()
                                (img "tick.png"))))

(defun ok-button (&key id style name value disabled content)
  (display (make-instance 'ok-button
                          :id id
                          :style style
                          :name name
                          :value value
                          :disabled disabled
                          :content content)))

(defclass cancel-button (widget)
  ((href    :accessor href    :initarg :href)
   (content :accessor content :initarg :content))
  (:default-initargs :content (html ()
                                (img "cancel.png"))))

(defun cancel-button (href &key id style content)
  (display (make-instance 'cancel-button
                          :id id
                          :style style
                          :href href
                          :content content)))



;;; menus

(defclass actions-menu (menu)
  ()
  (:default-initargs :style "hnavbar actions"))



;;; selector and controls for crud collections

(defmacro define-selector (row-class url)
  `(defmethod selector ((row ,row-class) enabled-p)
     (let* ((id (key row))
            (table (collection row))
            (filter (filter table))
            (start (page-start (paginator table) (index row) (start-index table))))
       (html ()
         (:a :href (if enabled-p
                       (apply #',url :start start filter)
                       (apply #',url :id id filter))
             (selector-img enabled-p))))))

(defmacro define-controls (row-class url)
  `(defmethod controls ((row ,row-class) enabled-p)
     (let ((id (key row))
           (table (collection row)))
       (if enabled-p
           (list (make-instance 'ok-button)
                 (make-instance 'cancel-button
                                :href (apply #',url :id id (filter table))))
           (list nil nil)))))
