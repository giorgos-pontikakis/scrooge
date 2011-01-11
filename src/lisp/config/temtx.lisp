(in-package :scrooge)



;;; ------------------------------------------------------------
;;; Transaction templates - Actions
;;; ------------------------------------------------------------

(define-dynamic-page actions/temtx/create ((description string #'not-db-null-p)
                                           (debit-acc string #'acc-exists-p)
                                           (credit-acc string #'acc-exists-p))
    ("actions/temtx/create" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
        (with-parameter-rebinding #'val
          (let ((debit-acc-id (account-id debit-acc))
                (credit-acc-id (account-id credit-acc)))
            (with-db ()
              (insert-dao (make-instance 'temtx
                                         :description description
                                         :debit-acc-id debit-acc-id
                                         :credit-acc-id credit-acc-id))
              (see-other (temtx)))))
        (with-parameter-rebinding #'raw
          (see-other (temtx/create :description description
                                   :debit-acc debit-acc
                                   :credit-acc credit-acc))))))

(define-dynamic-page actions/temtx/delete ((temtx-id integer #'valid-temtx-id-p))
    ("actions/temtx/delete" :request-type :post)
  (no-cache)
  (if (validp temtx-id)
      (with-db ()
        (delete-dao (get-dao 'temtx (val temtx-id)))
        (see-other (temtx)))
      (see-other (notfound))))

(define-dynamic-page actions/temtx/update ((temtx-id integer #'valid-temtx-id-p)
                                           (description string #'not-db-null-p)
                                           (debit-acc string #'acc-exists-p)
                                           (credit-acc string #'acc-exists-p))
    ("actions/temtx/update" :request-type :post)
  (no-cache)
  (with-parameter-list params
    (if (every #'validp params)
        (with-parameter-rebinding #'val
          (let ((debit-acc-id (account-id debit-acc))
                (credit-acc-id (account-id credit-acc)))
            (with-db ()
              (execute (:update 'temtx :set
                                'description description
                                'debit-acc-id debit-acc-id
                                'credit-acc-id credit-acc-id
                                :where (:= 'id temtx-id)))
              (see-other (temtx)))))
        (with-parameter-rebinding #'raw
          (see-other (temtx/update :temtx-id temtx-id
                                   :description description
                                   :debit-acc debit-acc
                                   :credit-acc credit-acc))))))



;;; ------------------------------------------------------------
;;; Transaction templates - Snippets
;;; ------------------------------------------------------------

(define-menu temtx-menu (temtx-id) (:div-style "actions" :ul-style "hmenu")
  (:create (with-html
             (:li (:a :href (temtx/create)
                      (:img :src (url "img/add.png")) "Δημιουργία"))))
  (:view (with-html
           (:li (:a :href (temtx :temtx-id temtx-id)
                    (:img :src (url "img/magnifier.png")) "Προβολή"))))
  (:update (if temtx-id
               (with-html
                 (:li (:a :href (temtx/update :temtx-id temtx-id)
                          (:img :src (url "img/pencil.png")) "Επεξεργασία")))
               nil))
  (:delete (if temtx-id
               (with-html
                 (:li (:a :href (temtx/delete :temtx-id temtx-id)
                          (:img :src (url "img/delete.png")) "Διαγραφή")))
               nil)))

(define-errorbar temtx-errorbar (:ul-style "error")
  (description "Η περιγραφή δεν πρέπει να είναι κενή")
  (debit-acc "Ο χρεωστικός λογαριασμός που έχετε εισάγει δεν υπάρχει.")
  (credit-acc "Ο πιστωτικός λογαριασμός που έχετε εισάγει δεν υπάρχει."))



;;; ------------------------------------------------------------
;;; Transaction templates - Pages
;;; ------------------------------------------------------------

(define-dynamic-page temtx ((temtx-id integer #'valid-temtx-id-p))
    ("config/temtx")
  (no-cache)
  (if (validp temtx-id)
      (with-parameter-rebinding #'val
        (with-page ()
          (:head
           (:title "Πρότυπες συναλλαγές")
           (head-css-std))
          (:body
           (:div :id "header"
                 (logo)
                 (primary-navbar 'config)
                 (config-navbar 'temtx))
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Κατάλογος πρότυπων συναλλαγών"))
                 (:div :id "temtx" :class "window"
                       (temtx-menu temtx-id :create :update :delete)
                       (display-temtx temtx-id :view))))))
      (see-other (notfound))))

(define-dynamic-page temtx/create ((description string #'not-db-null-p)
                                   (debit-acc string #'acc-exists-p)
                                   (credit-acc string #'acc-exists-p))
    ("config/temtx/create")
  (no-cache)
  (with-parameter-list params
    (with-page ()
      (:head
       (:title "Πρότυπες συναλλαγές: Δημιουργία")
       (head-css-std)
       (head-js-std))
      (:body
       (:div :id "header"
             (logo)
             (primary-navbar 'config)
             (config-navbar 'temtx))
       (:div :id "body"
             (:div :class "message"
                   (:h2 :class "info" "Δημιουργία πρότυπης συναλλαγής")
                   (temtx-errorbar description debit-acc credit-acc))
             (:div :id "temtx" :class "window"
                   (temtx-menu nil :view)
                   (display-temtx nil :create params)))))))

(define-dynamic-page temtx/update ((temtx-id integer #'valid-temtx-id-p t)
                                   (description string #'not-db-null-p)
                                   (debit-acc string #'acc-exists-p)
                                   (credit-acc string #'acc-exists-p))
    ("config/temtx/update")
  (no-cache)
  (if (validp temtx-id)
      (with-parameter-list params
        (with-page ()
          (:head
           (:title "Πρότυπες συναλλαγές: Επεξεργασία")
           (head-css-std)
           (head-js-std))
          (:body
           (:div :id "header"
                 (logo)
                 (primary-navbar 'config)
                 (config-navbar 'temtx))
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Επεξεργασία πρότυπης συναλλαγής")
                       (temtx-errorbar description debit-acc credit-acc))
                 (:div :id "temtx" :class "window"
                       (temtx-menu (val temtx-id) :view :delete)
                       (display-temtx (val temtx-id) :update (rest params)))))))
      (see-other (notfound))))

(define-dynamic-page temtx/delete ((temtx-id integer #'valid-temtx-id-p t))
    ("config/temtx/delete")
  (no-cache)
  (if (validp temtx-id)
      (with-parameter-rebinding #'val
        (with-page ()
          (:head
           (:title "Πρότυπες συναλλαγές: Διαγραφή")
           (head-css-std))
          (:body
           (:div :id "header"
                 (logo)
                 (primary-navbar 'config)
                 (config-navbar 'temtx))
           (:div :id "body"
                 (:div :class "message"
                       (:h2 :class "info" "Διαγραφή πρότυπης συναλλαγής"))
                 (:div :id "temtx" :class "window"
                       (temtx-menu temtx-id :view :update)
                       (display-temtx temtx-id :delete))))))
      (see-other (notfound))))


(defun display-temtx (active-id intent &optional params)
  (flet ((normal-row (id row activep)
           (bind (((description debit-acc credit-acc)  row))
             (with-html
               (:tr :class (if activep "active" nil)
                    (:td :class "select"
                         (if activep
                             (htm (:a :href (temtx)
                                      (:img :src (url "img/bullet_red.png"))))
                             (htm (:a :href (temtx :temtx-id id)
                                      (:img :src (url "img/bullet_blue.png"))))))
                    (:td :class "data" (str (lisp-to-html description)))
                    (:td :class "data" (str (lisp-to-html debit-acc)))
                    (:td :class "data" (str (lisp-to-html credit-acc)))
                    (:td :class "button" "")
                    (:td :class "button" "")))))
         (form-row-create (row styles)
           (bind (((description debit-acc credit-acc) row)
                  ((description% debit-acc% credit-acc%) styles))
             (with-form (actions/temtx/create)
               (:tr :class "active"
                    (:td :class "select"
                         (:a :href (temtx)
                             (:img :src (url "img/bullet_red.png"))))
                    (:td :class "data" (textbox 'description :value description :style description%))
                    (:td :class "data" (textbox 'debit-acc :value debit-acc :style debit-acc%))
                    (:td :class "data" (textbox 'credit-acc :value credit-acc :style credit-acc%))
                    (:td :class "button" (ok-button))
                    (:td :class "button" (cancel-button (temtx)))))))
         (form-row-update (id row styles)
           (bind (((description debit-acc credit-acc)  row)
                  ((description% debit-acc% credit-acc%) styles))
             (with-form (actions/temtx/update :temtx-id id)
               (:tr :class "active"
                    (:td :class "select"
                         (:a :href (temtx)
                             (:img :src (url "img/bullet_red.png"))))
                    (:td :class "data" (textbox 'description :value description :style description%))
                    (:td :class "data" (textbox 'debit-acc :value debit-acc :style debit-acc%))
                    (:td :class "data" (textbox 'credit-acc :value credit-acc :style credit-acc%))
                    (:td :class "button" (ok-button))
                    (:td :class "button" (cancel-button (temtx :temtx-id id)))))))
         (form-row-delete (id row)
           (destructuring-bind (description debit-acc credit-acc) row
             (with-form (actions/temtx/delete :temtx-id id)
               (:tr :class "attention"
                    (:td :class "select"
                         (:a :href (temtx)
                             (:img :src (url "img/bullet_red.png"))))
                    (:td :class "data" (str (lisp-to-html description)))
                    (:td :class "data" (str (lisp-to-html debit-acc)))
                    (:td :class "data" (str (lisp-to-html credit-acc)))
                    (:td :class "button" (ok-button))
                    (:td :class "button" (cancel-button (temtx :temtx-id id))))))))
    (with-db ()
      (let ((auto-txs (query (:select 'temtx.id 'description
                                      'debit-account.title 'credit-account.title
                                      :from 'temtx
                                      :inner-join (:as 'account 'debit-account)
                                      :on (:= 'debit-account.id 'temtx.debit-acc-id)
                                      :inner-join (:as 'account 'credit-account)
                                      :on (:= 'credit-account.id 'temtx.credit-acc-id))))
            (header '("" "Περιγραφή" "Λογαριασμός Χρέωσης" "Λογαριασμός Πίστωσης" "" ""))
            (inputs (if params
                        (mapcar #'val* params)
                        (make-list 3)))
            (styles (if params
                        (mapcar (lambda (p) (if (validp p) nil "attention")) params)
                        (make-list 3))))
        (with-html
          (:table :id "temtx-table" :class "forms-in-row"
                  (:thead
                   (:tr (iter (for label in header) (htm (:th (str label))))))
                  (:tbody
                   (when (eql intent :create)
                     (form-row-create inputs styles))
                   (iter (for (id . defaults) in auto-txs)
                         (for activep = (and active-id (= active-id id)))
                         (if activep
                             (let ((row (mapcar (lambda (p d) (or p d)) inputs defaults)))
                               (case intent
                                 (:view (normal-row id row activep))
                                 (:update (form-row-update id row styles))
                                 (:delete (form-row-delete id row))))
                             (normal-row id defaults activep))))))))))
