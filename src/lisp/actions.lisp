(in-package :scrooge)


(define-dynamic-page verify-login ((username string) (scrooge-password string))
    ("verify-login" :request-type :post)
  (with-db
    (let ((user (select-dao-unique 'webuser (:= 'username username))))
      (if (or (null user)
	      (string/= scrooge-password (password user)))
	  (redirect (login))
	  (progn
	    (start-session)
	    (setf (session-value 'user) user)
	    (redirect (home)))))))


;;; --- Banks --------------------

(define-dynamic-page insert-bank ((title string))
    ("actions/bank/insert" :request-type :post)
  (with-auth "root"
    (create-bank title)
    (redirect (config))))

(define-dynamic-page edit-bank ((id integer) (title string))
    ("actions/bank/edit" :request-type :post)
  (with-auth "root"
    (update-bank id :title title)
    (redirect (config))))

(define-dynamic-page remove-bank ((id integer))
    ("actions/bank/remove" :request-type :post)
  (with-auth "root"
    (delete-bank id)
    (redirect (config))))


;;; --- Cities --------------------

(define-dynamic-page insert-city ((title string))
    ("actions/city/insert" :request-type :post)
  (with-auth "root"
    (create-city title)
    (redirect (config))))

(define-dynamic-page edit-city ((id integer) (title string))
    ("actions/city/edit" :request-type :post)
  (with-auth "root"
    (update-city id :title title)
    (redirect (config))))

(define-dynamic-page remove-city ((id integer))
    ("actions/city/remove" :request-type :post)
  (with-auth "root"
    (delete-city id)
    (redirect (config))))


;;; --- Taxation Offices --------------------

(define-dynamic-page insert-tof ((title string))
    ("actions/tof/insert" :request-type :post)
  (with-auth "root"
    (create-tof title)
    (redirect (config))))

(define-dynamic-page edit-tof ((id integer) (title string))
    ("actions/tof/edit" :request-type :post)
  (with-auth "root"
    (update-tof id :title title)
    (redirect (config))))

(define-dynamic-page remove-tof ((id integer))
    ("actions/tof/remove" :request-type :post)
  (with-auth "root"
    (delete-tof id)
    (redirect (config))))


;;; --- Companies --------------------

(define-dynamic-page insert-company ((title string) (occupation string)
				     (tof-id integer) (tin integer) (address string)
				     (city-id integer) (pobox integer) (zipcode integer))
    ("actions/company/insert" :request-type :post)
  (with-auth "root"
    (create-company title occupation tof-id tin address city-id pobox zipcode)
    (redirect (companies))))

(define-dynamic-page edit-company ((company-id integer) (title string) (occupation string)
				   (tof-id integer) (tin integer) (address string)
				   (city-id integer) (pobox integer) (zipcode integer))
    ("actions/company/edit" :request-type :post)
  (with-auth "root"
    (update-company company-id
		    :title title
		    :occupation occupation
		    :tof-id tof-id
		    :tin tin
		    :address address
		    :city-id city-id
		    :pobox pobox
		    :zipcode zipcode)
    (redirect (company/view :company-id company-id))))

(define-dynamic-page remove-company ((company-id integer))
    ("actions/company/remove" :request-type :post)
  (with-auth "root"
    (delete-company company-id)
    (redirect (companies))))


;;; --- Accounts --------------------

(define-dynamic-page insert-account ((parent-id integer) (basename string)
				     (title string) (debit-p boolean))
    ("actions/account/insert" :request-type :post)
  (with-auth "root"
    (create-account parent-id basename title debit-p )
    (redirect (accounts))))

(define-dynamic-page edit-account ((account-id integer) (parent-id integer) (basename string)
				   (title string) (debit-p boolean))
    ("actions/account/edit" :request-type :post) 
  (with-auth "root" 
    (update-account account-id
		    :parent-id parent-id
		    :basename basename
		    :title title
		    :debit-p debit-p)
    (redirect (account/view :account-id account-id))))

(define-dynamic-page remove-account ((account-id integer))
    ("actions/accounts/remove" :request-type :post)
  (with-auth "root"
    (delete-account account-id)
    (redirect (accounts))))


;;; --- Transaction types --------------------

(define-dynamic-page insert-tx-type (title (debit-acc-id integer) (credit-acc-id integer))
    ("actions/transaction-type/insert" :request-type :post)
  (with-auth "root"
    (create-tx-type title debit-acc-id credit-acc-id)
    (redirect (transaction-types))))

(define-dynamic-page edit-tx-type ((tx-type-id integer) title
				   (debit-acc-id integer) (credit-acc-id integer))
    ("actions/transaction-type/edit" :request-type :post)
  (with-auth "root"
    (update-tx-type tx-type-id
		    :title title
		    :debit-acc-id debit-acc-id
		    :credit-acc-id credit-acc-id)
    (redirect (transaction-types))))

(define-dynamic-page remove-tx-type ((tx-type-id integer))
    ("actions/transaction-type/remove" :request-type :post)
  (with-auth "root"
    (delete-tx-type tx-type-id)
    (redirect (transaction-types))))