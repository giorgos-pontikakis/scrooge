(in-package :scrooge)


(define-dynamic-page insert-bank ((title string)) ("actions/bank/insert" :request-type :post)
  (create-bank title)
  (redirect (config)))

(define-dynamic-page edit-bank ((id integer) (title string)) ("actions/bank/edit" :request-type :post)
  (update-bank id :title title)
  (redirect (config)))

(define-dynamic-page remove-bank ((id integer)) ("actions/bank/remove" :request-type :post)
  (delete-bank id)
  (redirect (config)))




(define-dynamic-page insert-city ((title string)) ("actions/city/insert" :request-type :post)
  (create-city title)
  (redirect (config)))

(define-dynamic-page edit-city ((id integer) (title string)) ("actions/city/edit" :request-type :post)
  (update-city id :title title)
  (redirect (config)))

(define-dynamic-page remove-city ((id integer)) ("actions/city/remove" :request-type :post)
  (delete-city id)
  (redirect (config)))




(define-dynamic-page insert-tof ((title string)) ("actions/tof/insert" :request-type :post)
  (create-tof title)
  (redirect (config)))

(define-dynamic-page edit-tof ((id integer) (title string)) ("actions/tof/edit" :request-type :post)
  (update-tof id :title title)
  (redirect (config)))

(define-dynamic-page remove-tof ((id integer)) ("actions/tof/remove" :request-type :post)
  (delete-tof id)
  (redirect (config)))




(define-dynamic-page insert-company ((title string) (occupation string)
				     (tof-id integer) (tin integer) (address string)
				     (city-id integer) (pobox integer) (zipcode integer))
    ("actions/company/insert" :request-type :post)
  (create-company title occupation tof-id tin address city-id pobox zipcode)
  (redirect (companies)))

(define-dynamic-page edit-company ((company-id integer) (title string) (occupation string)
				   (tof-id integer) (tin integer) (address string)
				   (city-id integer) (pobox integer) (zipcode integer))
    ("actions/company/edit" :request-type :post)
  (update-company company-id
		  :title title
		  :occupation occupation
		  :tof-id tof-id
		  :tin tin
		  :address address
		  :city-id city-id
		  :pobox pobox
		  :zipcode zipcode)
  (redirect (company/view :company-id company-id)))

(define-dynamic-page remove-company ((company-id integer)) ("actions/company/remove" :request-type :post)
  (delete-company company-id)
  (redirect (companies)))