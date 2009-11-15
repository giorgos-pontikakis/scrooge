(in-package :scrooge)

(define-dynamic-page company/notfound () ("company/notfound")
  (with-page ()
    (:head
     (:title "Σκρούτζ: Άγνωστη εταιρία")
     (css "reset.css" "scrooge.css"))
    (:body
     (:div :id "header"
	   (logo)
	   (navbar "Εταιρίες"))
     (:div :id "body"
	   (:div :id "content" :class "summary"
		 (:p "Η εταιρία που προσπαθείτε να προσπελάσετε δεν υπάρχει πια.")
		 (:p "Επιστρέψτε στο μενού των εταιριών και προσπαθήστε ξανά."))))))