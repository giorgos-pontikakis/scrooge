-- -- First execute
-- -- createlang -U gnp plpgsql scrooge

-- alter table temtx add column propagated_p boolean;
-- alter table temtx add column sign integer;

-- alter table temtx alter column customer_p set not null;
-- alter table temtx alter column propagated_p set default 'f';
-- update temtx set propagated_p = 'f';
-- alter table temtx alter column propagated_p set not null;

-- update temtx set sign = +1 where balance = 'debit';
-- update temtx set sign = -1 where balance = 'credit';
-- update temtx set sign = 0 where balance = 'both';
-- alter table temtx alter column sign set not null;
-- alter table temtx drop column balance;
-- drop function company_tx_sum(integer, text);
-- drop function descendants(setof integer);

-- -- Manipulation of accounts and temtx
-- update account set parent_id = 12 where id = 13;
-- update cheque_stran set temtx_id = 5 where id = 23;
-- delete from temtx where title = 'Επιστροφή επιταγής';
-- update temtx set title = 'Σφράγισμα/Επιστροφή επιταγής' where id = 5;

-- update temtx set propagated_p = 'f';
-- update temtx set propagated_p = 't' where title like 'Χρέωση%';
-- update temtx set propagated_p = 't' where title like 'Πίστωση%';
-- update temtx set propagated_p = 't' where title like '%άμεσα%';
-- update temtx set propagated_p = 't' where title like 'Μεταφορά%';

-- alter table tx add column temtx_id integer not null default -1;

-- \i '/home/gnp/www/scrooge/src/sql/functions.sql'

-- update tx set temtx_id = get_temtx(debit_acc_id, credit_acc_id);
-- alter table tx alter column temtx_id drop default;


-- alter table tx rename column debit_acc_id to debit_account_id;
-- alter table tx rename column credit_acc_id to credit_account_id;
-- alter table temtx rename column debit_acc_id to debit_account_id;
-- alter table temtx rename column credit_acc_id to credit_account_id;

\d cheque_stran


alter table cheque_stran add column customer_p boolean;
update cheque_stran set customer_p = (select customer_p from temtx
where temtx.id = cheque_stran.temtx_id);

alter table cheque_stran alter column customer_p set not null;

alter table cheque_stran add constraint from_to_customer_p_key
unique(from_state_id, to_state_id, customer_p);

alter table temtx add constraint temtx_id_customer_p_fkey
unique(id, customer_p);

alter table cheque_stran add constraint cheque_stran_temtx_id_customer_p_fkey
foreign key (temtx_id, customer_p)
references temtx (id, customer_p);



alter table cheque_event add column cheque_stran_id integer;

update cheque_event set cheque_stran_id = (
select cheque_stran.id from cheque_event as ce
inner join cheque
on cheque.id = ce.cheque_id
inner join cheque_stran
on (cheque_stran.from_state_id = ce.from_state_id
   and
   cheque_stran.to_state_id = ce.to_state_id
   and
   cheque_stran.customer_p = cheque.customer_p)
where cheque_event.id = ce.id);

select * from cheque_event as ce
inner join cheque
on cheque.id = ce.cheque_id
inner join cheque_stran as cs
on cs.id = ce.cheque_stran_id
where (cs.from_state_id <> ce.from_state_id
   or
   cs.to_state_id <> ce.to_state_id
   or
   cs.customer_p <> cheque.customer_p);

alter table cheque_event alter column cheque_stran_id set not null;

alter table cheque_event drop column from_state_id;
alter table cheque_event drop column to_state_id;

alter table cheque_event add constraint cheque_event_cheque_stran_id_fkey
foreign key (cheque_stran_id)
references cheque_stran(id);


insert into account_role (id, account_id, description, rank)
       values ('project-account', 8, 'Λογαριασμός Έργων', 8);
