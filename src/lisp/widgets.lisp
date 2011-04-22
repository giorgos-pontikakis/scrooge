(in-package :scrooge)



;;; collections

(defclass scrooge-crud-tree (crud-tree)
  ()
  (:default-initargs :css-class "crud-tree"))

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
  (:default-initargs :css-class "crud-table"))

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
  (:default-initargs :body (html ()
                             (img "tick.png"))))

(defun ok-button (&rest instance-initargs)
  (display (apply #'make-instance 'ok-button instance-initargs )))

(defclass cancel-button (widget)
  ((href :accessor href :initarg :href)
   (body :accessor body :initarg :body))
  (:default-initargs :body (html ()
                             (img "cancel.png"))))

(defmethod display ((cancel-button cancel-button) &key)
  (with-html
    (:a :id (id cancel-button)
        :class (css-class cancel-button)
        :href (href cancel-button)
        (display (body cancel-button)))))

(defun cancel-button (href &rest instance-initargs)
  (display (apply #'make-instance 'cancel-button
                  :href href
                  instance-initargs)))



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
