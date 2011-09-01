(in-package :scrooge)



;;; ----------------------------------------------------------------------
;;; scrooge collections
;;; ----------------------------------------------------------------------


(defclass scrooge-table (crud-table)
  ()
  (:default-initargs :css-class "crud-table"))

;;; crud rows with records being daos

(defclass scrooge-row/obj (crud-row/obj)
  ()
  (:default-initargs :css-delete "attention"
                     :css-selected "selected"
                     :css-selector "selector"
                     :css-payload "payload"
                     :css-controls "controls"))

(defmethod key ((item scrooge-row/obj))
  (let ((rec (record item)))
    (handler-case (first (dao-keys rec))
      (unbound-slot () nil))))


;;; rows with records being plists

(defclass scrooge-row/plist (crud-row)
  ()
  (:default-initargs :css-delete "attention"
                     :css-selected "selected"
                     :css-selector "selector"
                     :css-payload "payload"
                     :css-controls "controls"))

(defmethod key ((item scrooge-row/plist))
  (getf (record item) :id))


;;; paginators

(defclass scrooge-paginator (paginator)
  ()
  (:default-initargs :delta 15
                     :id "paginator"
                     :css-class "paginator"))

(defmethod display ((pg paginator) &key (start 0))
  (let* ((delta (delta pg))
         (len (length (rows (table pg))))
         (prev (previous-page-start pg start))
         (next (next-page-start pg start)))
    (with-html
        (:div :class (css-class pg)
              (fmt "Record ~A–~A from ~A"
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


;;; crud-trees

(defclass scrooge-tree (crud-tree)
  ()
  (:default-initargs :css-class "crud-tree" :root-parent-key :null))

(defclass scrooge-node/plist (crud-node/plist)
  ()
  (:default-initargs :css-delete "attention"
    :css-selected "selected"
    :css-selector "selector"
    :css-payload "payload"
    :css-controls "controls"
    :css-indent "indent"))


(defmethod key ((item scrooge-node/plist))
  (getf (record item) :id))

(defmethod parent-key ((item scrooge-node/plist))
  (getf (record item) :parent-id))




;; ;;; ----------------------------------------------------------------------
;; ;;; top scrooge collections
;; ;;; ----------------------------------------------------------------------

;; ;;; crud-tables

;; (defclass scrooge-crud-table (crud-table)
;;   ()
;;   (:default-initargs :css-class "crud-table"))

;; (defclass scrooge-crud-row (crud-row)
;;   ()
;;   (:default-initargs :css-delete "attention"
;;                      :css-selected "selected"
;;                      :css-selector "selector"
;;                      :css-payload "payload"
;;                      :css-controls "controls"))

;; (defmethod key ((item scrooge-crud-row))
;;   (getf (record item) :id))

;; (defmethod update-item ((table crud-table) &key data index)
;;   (let ((row (nth index (rows table))))
;;     (setf (record row)
;;           (plist-union data (record row)))))

;; ;;; crud-trees

;; (defclass scrooge-crud-tree (crud-tree)
;;   ()
;;   (:default-initargs :css-class "crud-tree" :root-parent-key :null))

;; (defclass scrooge-crud-node (crud-node)
;;   ()
;;   (:default-initargs :css-delete "attention"
;;     :css-selected "selected"
;;     :css-selector "selector"
;;     :css-payload "payload"
;;     :css-controls "controls"
;;     :css-indent "indent"))

;; (defmethod update-item ((tree scrooge-crud-tree) &key data key)
;;   (let ((node (find-node (root tree) key)))
;;     (setf (record node)
;;           (plist-union data (record node)))))

;; (defmethod key ((item scrooge-crud-node))
;;   (getf (record item) :id))

;; (defmethod parent-key ((item scrooge-crud-node))
;;   (getf (record item) :parent-id))




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



;; ;;; ----------------------------------------------------------------------
;; ;;; account-radio tree
;; ;;; ----------------------------------------------------------------------

;; (defclass account-radio-tree (account-crud-tree)
;;   ()
;;   (:default-initargs :item-class 'account-radio-node))

;; (defclass account-radio-node (account-crud-node)
;;   ())

;; (defmethod selector ((node account-radio-node) enabled-p)
;;   (let* ((id (key node)))
;;     (make-instance 'input-radio
;;                    :name 'account-id
;;                    :value id
;;                    :body nil)))

;; (defun make-account-radio-tree (revenues-p)
;;   (make-instance 'account-radio-tree
;;                  :op :read
;;                  :root-parent-key (if revenues-p
;;                                       *revenues-root-account*
;;                                       *expenses-root-account*)
;;                  :filter (list :debit-p (not revenues-p))))



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



;;; selector and controls for crud collections

(defun simple-selector (row enabled-p url-fn)
  (let* ((id (key row))
         (table (collection row))
         (filter (filter table))
         (start (page-start (paginator table) (index row) (start-index table))))
    (html ()
      (:a :href (if enabled-p
                    (apply url-fn :start start filter)
                    (apply url-fn :id id filter))
          (selector-img enabled-p)))))

(defun simple-controls (row enabled-p url-fn)
  (let ((id (key row))
        (table (collection row)))
    (if enabled-p
        (list (make-instance 'ok-button)
              (make-instance 'cancel-button
                             :href (apply url-fn :id id (filter table))))
        (list nil nil))))
