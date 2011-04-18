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



;;; selector

(defun selector-img (enabled-p)
  (with-html
    (if enabled-p
        (img "bullet_red.png")
        (img "bullet_blue.png"))))


;;; buttons

(defclass ok-button (submit)
  ()
  (:default-initargs :content (html ()
                                (img "tick.png"))))

(defun lazy-ok-button (&key id style name value disabled)
  (make-instance 'ok-button
                 :id id
                 :style style
                 :name name
                 :value value
                 :disabled disabled))

(defclass cancel-button (widget)
  ((href    :accessor href    :initarg :href)
   (content :accessor content :initarg :content))
  (:default-initargs :content (html ()
                                (img "cancel.png"))))

(defun lazy-cancel-button (href &key id style)
  (make-instance 'cancel-button
                 :id id
                 :style style
                 :href href))



;;; menus

(defclass actions-menu (menu)
  ((style :initform "hnavbar actions")))



;;; ------------------------------------------------------------
;;; COLLAPSIBLE CRUD NODE
;;; ------------------------------------------------------------

;; (defclass collapsible-crud-node (node)
;;   ())

;; (defmethod display ((node crud-node) &key selected-id selected-data)
;;   (let ((selected-p (selected-p node selected-id))
;;         (tree (collection node)))
;;     (with-html
;;       (:li :class (if selected-p
;;                       (if (eq (op tree) :delete)
;;                           "attention"
;;                           "selected")
;;                       nil)
;;            (:span :class "selector"
;;                   (display (getf (cells node) :selector)
;;                            :state (if selected-p :on :off)))
;;            (:span :class "payload"
;;                   (display (getf (cells node) :payload)
;;                            :readonly (readonly-p node selected-id)))
;;            (mapc (lambda (cell)
;;                    (htm (:span :class "pushbutton"
;;                                (display cell :activep (controls-p node selected-id)))))
;;                  (getf (cells node) :controls))
;;            ;; Create
;;            (when (and selected-p
;;                       (eql (op tree) 'create))
;;              (insert-item tree
;;                           :record selected-data
;;                           :parent-key selected-id))
;;            ;; Update
;;            (when (and selected-p
;;                       (eql (op tree) 'update))
;;              (update-item tree
;;                           :record selected-data
;;                           :key selected-id))
;;            ;; Continue with children
;;            (when (children node)
;;              (htm (:ul :id (format nil "node_~D" (key node))
;;                        :class "indent"
;;                        (mapc (lambda (node)
;;                                (display node
;;                                         :selected-id selected-id
;;                                         :selected-data selected-data))
;;                              (children node)))))))))
