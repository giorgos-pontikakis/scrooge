(in-package :scrooge)


(define-existence-predicate cheque-stran-exists-p cheque-stran id)

(defun chk-cheque-stran-id (id)
  (if (cheque-stran-exists id)
      nil
      :cheque-stran-id-unknown))

;;; ------------------------------------------------------------
;;; Bank - Actions
;;; ------------------------------------------------------------

(define-regex-page actions/config/cheque-stran/create
    (("actions/config/cheque-stran" cheque-kind)
     :registers (cheque-kind "(receivable|payable)")
     :request-type :post)
    ((title          string)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title)
     (from-status    string chk-cheque-status)
     (to-status      string chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
    (if (every #'validp (parameters *page*))
        (with-db ()
          (let* ((debit-acc-id (account-id (val debit-account)))
                 (credit-acc-id (account-id (val credit-account)))
                 (new-cheque-stran (make-instance 'cheque-stran
                                                  :title (val title)
                                                  :debit-acc-id debit-acc-id
                                                  :credit-acc-id credit-acc-id
                                                  :payable-p (string= cheque-kind "payable")
                                                  :from-status (val from-status)
                                                  :to-status (val to-status))))))
        (see-other (config/cheque-stran/create cheque-kind
                                               :title (raw title)
                                               :debit-account debit-account
                                               :credit-account credit-account
                                               :from-status from-status
                                               :to-status to-status)))))

(define-regex-page config/cheque-stran (("config/cheque-stran/" cheque-kind)
                                        :registers (cheque-kind "(receivable|payable)"))
    (id integer chk-cheque-stran-id))

(define-regex-page config/cheque-stran/create (("config/cheque-stran/" cheque-kind "/create")
                                               :registers (cheque-kind "(receivable|payable)"))

    ((title          string)
     (debit-account  string chk-acc-title)
     (credit-account string chk-acc-title)
     (from-status    string chk-cheque-status)
     (to-status      string chk-cheque-status))
  (with-auth ("configuration")
    (no-cache)
    (with-document ()
      (:head
       (:title "Καταστατικές Μεταβολές Επιταγών > Δημιουργία")
       (config-headers))
      (:body
       (:div :id "container" :class "container_12"
             (header 'config)
             (config-navbar 'cheque)
             (:div :id "sidebar" :class "sidebar grid_3"
                   (:p :class "title" "Φίλτρα")
                   (searchbox (cheque cheque-kind) (val search))
                   (cheque-notifications))
             (:div :class "window grid_9"
                   (:div :class "title" "Καταστατικές Μεταβολές Επιταγών > Δημιουργία")
                   (cheque-stran-menu cheque-kind
                                      nil
                                      (val search)
                                      '(create update delete))
                   (with-form (actions/config/cheque-stran/create cheque-kind)
                     (cheque-stran-data-form cheque-kind
                                             'create
                                             :data (parameters->plist bank
                                                                      company
                                                                      due-date
                                                                      amount
                                                                      status)
                                             :styles (parameters->styles bank
                                                                         company
                                                                         due-date
                                                                         amount
                                                                         status)))))))))
