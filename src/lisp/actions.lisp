(in-package :scrooge)


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


