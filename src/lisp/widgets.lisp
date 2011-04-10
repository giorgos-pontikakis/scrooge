(in-package :scrooge)


(defclass default-paginator (paginator)
  ()
  (:default-initargs :delta (get-option "crud-table-num-rows")))



;;; ------------------------------------------------------------
;;; COLLAPSIBLE CRUD NODE
;;; ------------------------------------------------------------

(defclass collapsible-crud-node (node)
  ())

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
             (htm (:ul :id (format nil "node_~D" (key node))
                       :class "indent"
                       (mapc (lambda (node)
                               (display node
                                        :selected-id selected-id
                                        :selected-data selected-data))
                             (children node)))))))))
