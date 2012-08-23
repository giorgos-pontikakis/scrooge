----------------------------------------------------------------------
-- Account lineage, descendants and level
----------------------------------------------------------------------
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



----------------------------------------------------------------------
-- Get the temtx for a given pair of accounts (debit/credit)
----------------------------------------------------------------------
create or replace
function get_temtx (in debit_account_id integer, in credit_account_id integer, out temtx_id integer)
returns integer as
$$
with temtx_level as (
select id, ((select account_level(debit_acc_id)) +
            (select account_level(credit_acc_id))) as combined_level
from temtx
where (debit_acc_id = $1 and
       credit_acc_id = $2)
or (propagated_p = 't'
   and
   ((debit_acc_id = any (select account_lineage($1)) or
   debit_acc_id = $1)
   and
   (credit_acc_id = any (select account_lineage($2)) or
   credit_acc_id = $2)))
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
function temtx_conflicts (in debit_account_id integer, in credit_account_id integer, out conflicts integer)
returns setof integer as
$$
select temtx.id
from temtx
inner join account as debit_account
on temtx.debit_acc_id = debit_account.id
inner join account as credit_account
on temtx.credit_acc_id = credit_account.id
where
-- precise matches
temtx.debit_acc_id = $1 and
temtx.credit_acc_id = $2
or
-- propagated matches
(temtx.propagated_p = 't'
 and
 (($1 in (select account_lineage(temtx.debit_acc_id)) and
   $2 in (select account_descendants(temtx.credit_acc_id)))
    or
  ($1 in (select account_descendants(temtx.debit_acc_id)) and
   $2 in (select account_lineage(temtx.credit_acc_id)))))
$$ language sql;

-- validation for existing temtxs
select * from (select temtx.id, temtx.title, temtx_conflicts(debit_acc_id, credit_acc_id) from temtx) as foo
where foo.id <> foo.temtx_conflicts;


----------------------------------------------------------------------
-- Company Balance
----------------------------------------------------------------------
create or replace function company_balance (in id integer, out company_balance numeric)
returns numeric as
$$ select coalesce(sum(tx.amount*temtx.sign))
from tx
left join cheque_event
on (cheque_event.tx_id = tx.id)
left join cheque
on (cheque.id = cheque_event.cheque_id)
inner join temtx
on temtx.id = tx.temtx_id
where tx.company_id = $1 and
      ((cheque_event.to_state_id = cheque.state_id) or (cheque_event.to_state_id is null))
$$ language sql;



----------------------------------------------------------------------
-- temtx trigger
----------------------------------------------------------------------
create or replace function generate_temtx_id () returns trigger as $$
begin
select get_temtx(new.debit_acc_id, new.credit_acc_id) into new.temtx_id;
return new;
end
$$ language plpgsql;

create trigger generate_temtx_id_trigger
before insert or update
on tx for each row
execute procedure generate_temtx_id();
