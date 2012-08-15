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

create or replace
function account_lineage (in id integer, out parent_ids integer)
returns setof integer as
$$ with recursive node (id, parent_id) as (
select id, parent_id from account where account.id = $1
union
select account.id, account.parent_id
from account, node
where node.parent_id = account.id
)
select id from node where id <> $1
$$ language sql;

create or replace
function account_descendants (in id integer, out children_ids integer)
returns setof integer as
$$ with recursive node (id, parent_id) as (
select id, parent_id from account where account.id = $1
union
select account.id, account.parent_id
from account, node
where account.parent_id = node.id
)
select id from node where id <> $1;
$$ language sql;

create or replace
function account_level (in id integer, out level bigint)
returns bigint as
$$ with recursive node (id, parent_id) as (
select id, parent_id from account where account.id = $1
union
select account.id, account.parent_id
from account, node
where node.parent_id = account.id
)
select (count(id)-1) from node;
$$ language sql;

alter table account add column lineage integer[];
alter table account add column descendants integer[];
alter table account add column level bigint;

update account set lineage = array(select account_lineage(id));
update account set descendants = array(select account_descendants(id));
update account set level = (select account_level(id));

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


----------------------------------------------------------------------
-- Find the corresponding temtx of a given tx
----------------------------------------------------------------------
create or replace
function find_temtx (in tx_id integer, out temtx_id integer)
returns integer as
$$
with temtx_level as (
select temtx.id, --temtx.title, temtx.debit_acc_id, temtx.credit_acc_id,
(temtx_debit_account.level + temtx_credit_account.level) as combined_level
from tx
-- accounts referenced by tx
inner join account as tx_debit_account
on tx.debit_acc_id = tx_debit_account.id
inner join account as tx_credit_account
on tx.credit_acc_id = tx_credit_account.id
-- set of relevant temtx
inner join temtx
on (temtx.debit_acc_id = tx_debit_account.id and
    temtx.credit_acc_id = tx_credit_account.id)
or (temtx.propagated_p = 't' and
    ((temtx.debit_acc_id = any (tx_debit_account.lineage) or
      temtx.debit_acc_id = tx_debit_account.id) and
     (temtx.credit_acc_id = any (tx_credit_account.lineage) or
      temtx.credit_acc_id = tx_credit_account.id)))
-- accounts referenced by each temtx
inner join account as temtx_debit_account
on temtx.debit_acc_id = temtx_debit_account.id
inner join account as temtx_credit_account
on temtx.credit_acc_id = temtx_credit_account.id
where tx.id = $1   -- 2033
order by combined_level desc
)
select temtx_level.id
from temtx_level
where combined_level = (select max(combined_level) from temtx_level);
$$ language sql;


----------------------------------------------------------------------
-- Implement Temtx Basic Constraint
----------------------------------------------------------------------
create or replace
function temtx_conflicts (in temtx_id integer, out conflicts integer)
returns setof integer as
$$
select temtx.id
from temtx
inner join temtx as input_temtx
on $1 = input_temtx.id
inner join account as debit_account
on temtx.debit_acc_id = debit_account.id
inner join account as credit_account
on temtx.credit_acc_id = credit_account.id
where
temtx.debit_acc_id = input_temtx.debit_acc_id and
temtx.credit_acc_id = input_temtx.credit_acc_id and
temtx.id <> input_temtx.id
or
(temtx.propagated_p = 't'
 and
 ((input_temtx.debit_acc_id in (select account_lineage(temtx.debit_acc_id)) and
   input_temtx.credit_acc_id in (select account_descendants(temtx.credit_acc_id)))
    or
  (input_temtx.debit_acc_id in (select account_descendants(temtx.debit_acc_id)) and
   input_temtx.credit_acc_id in (select account_lineage(temtx.credit_acc_id)))))
$$ language sql;
