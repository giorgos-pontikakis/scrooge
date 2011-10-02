(in-package :scrooge)

(defun make-events-for-project-dao (pdao)
  (when (quote-date pdao)
    (insert-dao
     (make-instance 'project-event
                    :project-id (id pdao)
                    :tx-id :null
                    :project-stran-id (id (select-dao-unique 'project-stran
                                                             (:and (:is-null 'from-state)
                                                                   (:= 'to-state "quoted"))))
                    :event-date (quote-date pdao))))
  (when (start-date pdao)
    (insert-dao
     (make-instance 'project-event
                    :project-id (id pdao)
                    :tx-id :null
                    :project-stran-id (id (select-dao-unique 'project-stran
                                                             (:and (:= 'from-state "quoted")
                                                                   (:= 'to-state "ongoing"))))
                    :event-date (start-date pdao))))
  (when (end-date pdao)
    (insert-dao
     (make-instance 'project-event
                    :project-id (id pdao)
                    :tx-id :null
                    :project-stran-id (id (select-dao-unique 'project-stran
                                                             (:and (:= 'from-state "ongoing")
                                                                   (:= 'to-state "finished"))))
                    :event-date (end-date pdao))))
  (when (string-equal (state pdao) "archived")
    (insert-dao
     (make-instance 'project-event
                    :project-id (id pdao)
                    :tx-id :null
                    :project-stran-id (id (select-dao-unique 'project-stran
                                                             (:and (:= 'from-state "finished")
                                                                   (:= 'to-state "archived"))))
                    :event-date (end-date pdao)))))

(defun make-all-project-events ()
  (mapc #'make-events-for-project-dao (select-dao 'project)))


(defun possible-next-project-states (project-state)
  (with-db ()
    (query (:select 'project-stran
                    :where (:= 'project-stran.from-state project-state))
           :list)))