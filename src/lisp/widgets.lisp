(in-package :scrooge)


;;; ----------------------------------------------------------------------
;;; Widget prototol
;;; ----------------------------------------------------------------------

(defgeneric actions (widget &key)
  (:documentation "Prints the html for the actions of the widget"))

(defgeneric disabled-actions (widget &key)
  (:documentation "Returns a list of the disabled actions of the widget"))

(defgeneric get-record (widget)
  (:documentation "Returs a record of the widget"))

(defgeneric filters (collection)
  (:documentation "Returns the filter linke of a collection widget"))


;;; ----------------------------------------------------------------------
;;; Tables
;;; ----------------------------------------------------------------------

(defclass scrooge-table (crud-table)
  ()
  (:default-initargs :css-class "crud-table crud-table-full"))

(defmethod disabled-actions ((tbl scrooge-table) &key)
  (ecase (op tbl)
    (:catalogue '())
    ((:create :update :delete) '(:details :create :update :delete))))


;;; crud rows with records being daos

(defclass scrooge-row/obj (crud-row/obj)
  ()
  (:default-initargs :css-selected "selected"
                     :css-selector "selector"
                     :css-payload "payload"
                     :css-controls "controls"))

(defmethod key ((item scrooge-row/obj))
  (handler-case (slot-value (record item) 'id)
    (unbound-slot () nil)))


;;; rows with records being plists

(defclass scrooge-row/plist (crud-row/plist)
  ()
  (:default-initargs :css-selected "selected"
                     :css-selector "selector"
                     :css-payload "payload"
                     :css-controls "controls"))

(defmethod key ((item scrooge-row/plist))
  (getf (record item) :id))


;;; paginator

(defclass scrooge-paginator (paginator)
  ()
  (:default-initargs :delta 500
                     :id "paginator"
                     :css-class "paginator"))

(defmethod display ((pg paginator) &key (start 0))
  (let* ((delta (delta pg))
         (len (length (rows (table pg))))
         (prev (previous-page-start pg start))
         (next (next-page-start pg start)))
    (when (rows (table pg))
      (with-html
        (:div :class (css-class pg)
          (fmt "Εγγραφές ~A–~A από ~A"
               (1+ start)
               (min (+ start delta) len)
               len)
          (if prev
              (htm (:a :href (target-url pg prev)
                     (img "resultset_previous.png")))
              (img "resultset_first.png"))
          (if next
              (htm (:a :href (target-url pg next)
                     (img "resultset_next.png")))
              (img "resultset_last.png")))))))

;;; table mixins

(defclass ranked-table-mixin ()
  ())

(defmethod disabled-actions ((tbl ranked-table-mixin) &key key)
  (ecase (op tbl)
    (:catalogue (if key
                    nil
                    '(:update :delete :rank-up :rank-down)))
    ((:create :update :delete) '(:create :update :delete :rank-up :rank-down))))


;;; ----------------------------------------------------------------------
;;; Trees
;;; ----------------------------------------------------------------------

(defclass scrooge-tree (crud-tree)
  ((root-parent-key :allocation :class :initform :null))
  (:default-initargs :css-class "crud-tree" :root-key nil))


;;; crud tree with records being daos

(defclass scrooge-node/obj (crud-node/obj)
  ()
  (:default-initargs :css-selected "selected"
                     :css-selector "selector"
                     :css-payload "payload"
                     :css-controls "controls"
                     :css-indent "indent"))

(defmethod key ((item scrooge-row/obj))
  (handler-case (slot-value (record item) 'id)
    (unbound-slot () nil)))

(defmethod parent-key ((item scrooge-row/obj))
  (handler-case (slot-value (record item) 'parent-id)
    (unbound-slot () nil)))


;;; crud tree with records being plists

(defclass scrooge-node/plist (crud-node/plist)
  ()
  (:default-initargs :css-selected "selected"
                     :css-selector "selector"
                     :css-payload "payload"
                     :css-controls "controls"
                     :css-indent "indent"))

(defmethod key ((item scrooge-node/plist))
  (getf (record item) :id))

(defmethod parent-key ((item scrooge-node/plist))
  (getf (record item) :parent-id))



;;; ----------------------------------------------------------------------
;;; row buttons (image without text)
;;; ----------------------------------------------------------------------

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



;;; ------------------------------------------------------------
;;; Data Forms
;;; ------------------------------------------------------------

(defclass crud-form (widget)
  ((op         :accessor op         :initarg :op)
   (key        :accessor key        :initarg :key)
   (filter     :accessor filter     :initarg :filter)
   (cancel-url :accessor cancel-url :initarg :cancel-url)
   (record     :accessor record     :initarg :record))
  (:default-initargs :key nil))

(defclass crud-form/plist (crud-form record/plist-mixin)
  ())

(defclass crud-form/obj (crud-form record/obj-mixin)
  ())

(defmethod initialize-instance :after ((form crud-form) &key)
  (when (and (eql (op form) :create)
             (key form))
    (error "Contradiction in crud-form initialization. Slot OP is :create and slot KEY is not null"))
  (unless (slot-boundp form 'record)
    (setf (slot-value form 'record) (get-record form))))

(defmethod display :before ((form crud-form) &key payload)
  (when (eql (op form) :create)
    (setf (record form) (create-record form payload)))
  (when (eql (op form) :update)
    (update-record form payload)))

(defmethod actions ((form crud-form) &key)
  (declare (ignore form))
  (actions-menu nil))

(defmethod disabled-actions ((form crud-form) &key)
  (ecase (op form)
    (:details '())
    ((:create :update :delete) '(:update :delete))))



;;; ------------------------------------------------------------
;;; selector and controls for crud collections
;;; ------------------------------------------------------------

(defun simple-selector (row selected-p url-fn id-key)
  (let* ((id (key row))
         (table (collection row))
         (filter (filter table))
         (start (page-start (paginator table) (index row) (start-index table))))
    (html ()
      (:a :id id
        :href (if selected-p
                  (apply url-fn :start start filter)
                  (apply url-fn
                         id-key
                         id
                         filter))
        (selector-img selected-p)))))

(defun simple-controls (row enabled-p url-fn id-key)
  (let ((id (key row))
        (table (collection row)))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (apply url-fn
                                          id-key
                                          id
                                          (filter table))))
        (list nil nil))))



;;; ------------------------------------------------------------
;;; menu widgets
;;; ------------------------------------------------------------

(defclass scrooge-menu (menu)
  ()
  (:default-initargs :css-disabled "invisible" :css-class "actions hmenu"))

(defun scrooge-menu (spec &key id css-class disabled css-disabled)
  (let ((initargs (plist-collect-if #'identity
                                    (list :id id
                                          :css-class css-class
                                          :disabled disabled
                                          :css-disabled css-disabled)
                                    :on-values-p t)))
    (display (apply #'make-instance 'scrooge-menu :spec spec initargs))))



;; ;;; ----------------------------------------------------------------------
;; ;;; Account-RO (read only) tree
;; ;;; ----------------------------------------------------------------------

;; (defclass account-ro-tree (account-crud-tree)
;;   ((op :initform :read))
;;   (:default-initargs :item-class 'account-ro-node))

;; (defclass account-ro-node (account-crud-node)
;;   ())

;; (defmethod selector ((node account-ro-node) enabled-p)
;;   (let ((id (key node)))
;;     (html ()
;;       (:a :href
;;           (if enabled-p
;;               (account/overview)
;;               (account/overview :id id))
;;           (selector-img enabled-p)))))

;; (defmethod payload ((node account-ro-node) enabled-p)
;;   (make-instance 'textbox
;;                  :name 'title
;;                  :value (getf (record node) :title)
;;                  :disabled (not enabled-p)))

;; (defmethod controls ((node account-ro-node) enabled-p)
;;   (declare (ignore node enabled-p))
;;   (list nil nil))
