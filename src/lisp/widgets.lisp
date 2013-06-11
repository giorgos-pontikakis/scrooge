(in-package :scrooge)


;;; ----------------------------------------------------------------------
;;; Widget prototol
;;; ----------------------------------------------------------------------

(defgeneric actions (widget &key &allow-other-keys)
  (:documentation "Prints the html for the actions of the widget"))

(defgeneric disabled-actions (widget &key &allow-other-keys)
  (:documentation "Returns a list of the disabled actions of the widget"))

(defgeneric filters (collection)
  (:documentation "Returns the filter links of a collection widget"))

(defgeneric extra-info (widget)
  (:documentation "Display extra info for the widget, e.g. sums for a table"))



;;; ----------------------------------------------------------------------
;;; Records
;;; ----------------------------------------------------------------------

(defmethod get-key ((record standard-object))
  (if (slot-exists-p record 'id)
      (if (slot-boundp record 'id)
          (slot-value record 'id)
          nil)
      (error "Slot ID does not exist for record ~A" record)))

(defmethod get-key ((record list))
  (getf record :id))

(defmethod get-key ((record null))
  nil)


(defmethod get-parent-key ((record standard-object))
  (if (slot-exists-p record 'parent-id)
      (if (slot-boundp record 'parent-id)
          (slot-value record 'parent-id)
          nil)
      (error "Slot ID does not exist for record ~A" record)))

(defmethod get-parent-key ((record list))
  (getf record :parent-id))

(defmethod get-parent-key ((record null))
  nil)



;;; ----------------------------------------------------------------------
;;; Tables
;;; ----------------------------------------------------------------------

(defclass scrooge-crud-table (crud-table)
  ((filter :accessor filter :initarg :filter))
  (:default-initargs :filter nil :css-class "crud-table crud-table-full"))

(defmethod disabled-actions ((tbl scrooge-crud-table) &key)
  (ecase (op tbl)
    (:catalogue '())
    ((:create :update :delete) '(:details :create :update :delete :journal))))


;;; Ranked table mixin

(defclass ranked-table-mixin ()
  ())

(defmethod disabled-actions ((tbl ranked-table-mixin) &key key)
  (ecase (op tbl)
    (:catalogue (if key
                    nil
                    '(:update :delete :rank-up :rank-down)))
    ((:create :update :delete) '(:create :update :delete :rank-up :rank-down))))


;;; Rows

(defclass scrooge-row (crud-row)
  ()
  (:default-initargs :css-selected "selected"
                     :css-selector "selector"
                     :css-payload "payload"
                     :css-controls "controls"))


;;; paginator

(defclass scrooge-paginator (paginator)
  ()
  (:default-initargs :delta 15
                     :css-class "paginator"))

(defmethod display ((pg paginator) &key (start 0))
  (let* ((delta (delta pg))
         (len (length (records (table pg))))
         (prev (previous-page-start pg start))
         (next (next-page-start pg start)))
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
                (img "resultset_last.png"))))))



;;; ----------------------------------------------------------------------
;;; Trees
;;; ----------------------------------------------------------------------

(defclass scrooge-crud-tree (crud-tree)
  ((filter :accessor filter :initarg :filter)
   (root-parent-key :allocation :class :initform :null))
  (:default-initargs :filter nil
                     :css-class "crud-tree"
                     :root-key nil))

(defclass scrooge-node (crud-node)
  ()
  (:default-initargs :css-selected "selected"
                     :css-selector "selector"
                     :css-payload "payload"
                     :css-controls "controls"
                     :css-indent "indent"))



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

(defclass scrooge-crud-form (crud-form)
  ((filter     :accessor filter     :initarg :filter)
   (cancel-url :accessor cancel-url :initarg :cancel-url)))

(defmethod actions ((form scrooge-crud-form) &key)
  (declare (ignore form))
  (actions-menu nil))

(defmethod disabled-actions ((form scrooge-crud-form) &key)
  (ecase (op form)
    (:details '())
    ((:create :update :delete) '(:update :delete :journal))))



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
