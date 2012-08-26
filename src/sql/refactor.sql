-- First execute
-- createlang -U gnp plpgsql scrooge

alter table temtx add column propagated_p boolean;
alter table temtx add column sign integer;

alter table temtx alter column customer_p set not null;
alter table temtx alter column propagated_p set default 'f';
update temtx set propagated_p = 'f';
alter table temtx alter column propagated_p set not null;

update temtx set sign = +1 where balance = 'debit';
update temtx set sign = -1 where balance = 'credit';
update temtx set sign = 0 where balance = 'both';
alter table temtx alter column sign set not null;
alter table temtx drop column balance;
drop function company_tx_sum(integer, text);
drop function descendants(setof integer);

-- Manipulation of accounts and temtx
update account set parent_id = 12 where id = 13;
update cheque_stran set temtx_id = 5 where id = 23;
delete from temtx where title = 'Επιστροφή επιταγής';
update temtx set title = 'Σφράγισμα/Επιστροφή επιταγής' where id = 5;

update temtx set propagated_p = 'f';
update temtx set propagated_p = 't' where title like 'Χρέωση%';
update temtx set propagated_p = 't' where title like 'Πίστωση%';
update temtx set propagated_p = 't' where title like '%άμεσα%';
update temtx set propagated_p = 't' where title like 'Μεταφορά%';

alter table tx add column temtx_id integer not null default -1;

\i '/home/gnp/www/scrooge/src/sql/functions.sql'

update tx set temtx_id = get_temtx(debit_acc_id, credit_acc_id);
alter table tx alter column temtx_id drop default;


alter table tx rename column debit_acc_id to debit_account_id;
alter table tx rename column credit_acc_id to credit_account_id;
alter table temtx rename column debit_acc_id to debit_account_id;
alter table temtx rename column credit_acc_id to credit_account_id;
