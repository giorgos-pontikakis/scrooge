
--- extract an array of the parents of an account
create or replace function account_lineage (in id integer, out parent_ids integer)
returns setof integer as
$$ with recursive node (id, parent_id) as (
select id, parent_id from account where account.id = $1
union
select account.id, account.parent_id
from account, node
where node.parent_id = account.id
)
select id from node;
$$ language sql;

--- update lineage field in the accounts table to avoid using
--- the account_lineage recursive function in queries
create or replace function update_account_lineage () returns void as
$$ update account set lineage = array(select account_lineage(id));
$$ language sql;

--- compute company balance (total debit - credit tx)
create or replace function company_balance (in id integer, out company_balance numeric)
returns numeric as
$$ select coalesce(sum(tx.amount*temtx.sign))
from tx
left join cheque_event
on (cheque_event.tx_id = tx.id)
left join cheque
on (cheque.id = cheque_event.cheque_id)
inner join account as debit_account
on (debit_account.id = tx.debit_acc_id)
inner join account as credit_account
on (credit_account.id = tx.credit_acc_id)
inner join temtx
on ((temtx.debit_acc_id = any (debit_account.lineage)) and
    (temtx.credit_acc_id = any (credit_account.lineage)))
where tx.company_id = $1 and
      ((cheque_event.to_state_id = cheque.state_id) or (cheque_event.to_state_id is null))
$$ language sql;
