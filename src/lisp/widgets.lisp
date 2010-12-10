(in-package :scrooge)

(declaim (optimize (speed 0) (debug 3)))



;;; ------------------------------------------------------------
;;; Navigation bars
;;; ------------------------------------------------------------
(defun generic-navbar (&key ul-style active-page-style inactive-page-style)
  (html (&key id page-specs active-page-name)
    (:div :id id
          (:ul :class ul-style
               (iter (for (page-name label) in page-specs)
                     (htm (:li (:a :class  (if (eql page-name active-page-name)
                                               active-page-style
                                               inactive-page-style)
                                   :href (full-url page-name)
                                   (render label)))) )))))

(defun hnavbar ()
  (generic-navbar :active-page-style "active"
                  :inactive-page-style nil
                  :ul-style "hmenu"))

(defun primary-navbar (active)
  (render (hnavbar)
          :id "navbar"
          :page-specs '((home "Αρχική")
                        (config "Ρυθμίσεις"))
          :active-page-name active))


(defun generic-menu (&key id div-style ul-style)
  (html (&key item-specs enabled-items)
    (:div :id id
          :class div-style
          (:ul :class ul-style
               (iter (for (action-name href label img-url) in item-specs)
                     (when (and (member action-name enabled-items)
                                (not (null href)))
                       (htm (:li (:a :href href
                                     (when img-url
                                       (img img-url))
                                     (str label))))))))))

(defun generic-errorbar ()
  (flet ((get-message (param messages)
           (or (second (assoc (error-type param)
                              (getf messages (name param))))
               "Unknown error in generic-errorbar.")))
    (html (params messages)
      (unless (every #'validp params)
        (htm
         (:div (:ul :class "error"
                    (iter (for p in params)
                          (unless (validp p)
                            (htm (:li (str (get-message p messages)))))))))))))



;;; ------------------------------------------------------------
;;; Forms
;;; ------------------------------------------------------------

(defun form (submit-page hidden body)
  (let ((page (find-page submit-page '*webapp*)))
    (with-html
      (:form :method (request-type page)
             :action (concatenate 'string (webroot (webapp page)) (base-url page))
             (iter (for key in hidden by #'cddr)
                   (for val in (rest hidden) by #'cddr)
                   (htm
                     (:input :type "hidden"
                             :id (string-downcase key)
                             :style "display: none;"
                             :name (string-downcase key)
                             :value (lisp->html val))))
             (render body)))))

(defmacro with-form (url &body body)
  (let ((page-name (first url))
        (hidden (rest url)))
    `(form ',page-name (list ,@hidden)
           ,@body)))


(defun textbox (name &key id style readonlyp disabledp passwordp value)
  (with-html
    (:input :id id
            :class style
            :type (if passwordp "password" "text")
            :name (string-downcase name)
            :value (lisp->html (or value :null))
            :readonly readonlyp
            :disabled disabledp)))

(defun radio (name label-value-alist &key id style readonlyp disabledp checked)
  (with-html
    (:ul :id (or id (string-downcase name))
         :class style
         (iter (for (label value) in label-value-alist)
               (htm (:li (:input :type "radio"
                                 :name (string-downcase name)
                                 :value (lisp->html value)
                                 :checked (equal value checked)
                                 :readonly readonlyp
                                 :disabled disabledp)
                         (render label)))))))

(defun dropdown (name label-value-alist &key style readonlyp disabledp selected)
  (with-html
    (:select :id (string-downcase name)
             :class style
             :name (string-downcase name)
             :disabled disabledp
             (iter (for (label value) in label-value-alist)
                   (htm (:option :value (lisp->html value)
                                 :selected (equal value selected)
                                 :readonly readonlyp
                                 (render label)))))))

(defun label (name text &key style)
  (with-html
    (:label :class style
            :for (string-downcase name)
            (render text))))

(defun submit (label &key name value style disabledp)
  (with-html
    (:button :class style
             :type "submit"
             :name (if name (string-downcase name) nil)
             :value (if value (lisp->html value) nil)
             :disabled disabledp
             (render label))))



;;; ------------------------------------------------------------
;;; Tables
;;; ------------------------------------------------------------

(defun thead (&rest args)
  (with-html
    (:thead
     (:tr (mapc (lambda (item)
                  (htm (:th (str item))))
                args)))))



;;; ------------------------------------------------------------
;;; Links
;;; ------------------------------------------------------------

(defun selector-link (states)
  (html (state)
    (:a :href (second (assoc state states))
        (img (if (true state)
                 "bullet_red.png"
                 "bullet_blue.png")))))

(defun arrow ()
  (html ()
    (:a :href )))

(defun ok-link (visiblep)
  (if visiblep
      (with-html
        (submit (html ()
                  (img "tick.png"))))
      (with-html
        "")))

(defun cancel-link (href visiblep)
  (if visiblep
      (with-html
        (:a :href href
            (img "cancel.png")))
      (with-html
        "")))



;;; ------------------------------------------------------------
;;; Table with inline form
;;; ------------------------------------------------------------

(defun mkfn-row-selected-p (id-keys)
  (lambda (id)
    (let ((result (mapcar (lambda (key)
                            (eql (val* (find-parameter key))
                                 (getf id key) #|(find-datum id key)|#))
                          id-keys)))
      (every #'true result))))

(defun mkfn-row-controls-p (op form-ops)
  (lambda (selected-p)
    (and selected-p
         (member op form-ops))))

(defun mkfn-row-readonly-p (op ro-ops rw-ops)
  (cond ((member op ro-ops)
         (constantly t))
        ((member op rw-ops)
         (lambda (selected-p)
           (not selected-p)))
        (t (error "Unknown operation: ~A" op))))

(defun mkfn-row-id (id-keys)
  (lambda (row-data)
    (plist-collect id-keys row-data)))

(defun mkfn-row-payload (payload-keys)
  (lambda (row-data readonly-p)
    (if readonly-p
        (plist-collect payload-keys row-data)
        (plist-union (plist-collect payload-keys (params->plist (parameters *page*)))
                     (plist-collect payload-keys row-data)))))
