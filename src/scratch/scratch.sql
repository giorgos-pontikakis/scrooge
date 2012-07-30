
CREATE FUNCTION descendants (IN id integer, OUT children_ids integer)
RETURNS setof integer AS
$$ WITH RECURSIVE parent (id, parent_id) AS (
SELECT id, parent_id FROM account WHERE account.id = $1
UNION
SELECT account.id, account.parent_id
FROM account, parent
WHERE account.parent_id = parent.id
)
SELECT id FROM parent;
$$ LANGUAGE SQL;


CREATE FUNCTION company_tx_sum (IN id integer, IN balance text, OUT company_tx_sum numeric)
RETURNS numeric AS
$$ SELECT coalesce(sum(tx.amount), 0)
FROM tx
LEFT JOIN cheque_event
ON (cheque_event.tx_id = tx.id)
LEFT JOIN cheque
ON (cheque.id = cheque_event.cheque_id)
INNER JOIN temtx
ON ((tx.debit_acc_id IN (SELECT * FROM descendants(temtx.debit_acc_id))) AND
    (tx.credit_acc_id IN (SELECT * FROM descendants(temtx.credit_acc_id))))
WHERE ((tx.company_id = $1) AND
       (temtx.balance = $2) AND
       ((cheque_event.to_state_id = cheque.state_id) or (cheque_event.to_state_id IS NULL)))
$$ LANGUAGE SQL;



-- COMPANY DEBITS
select tx_date, tx.id, tx.description, temtx.debit_p, tx.amount
       cheque.due_date, cheque.state_id,
       debit_account.title as debit_account, credit_account.title as credit_account
       from tx
       left join cheque_event
       on cheque_event.tx_id = tx.id
       left join cheque
       on cheque.id = cheque_event.cheque_id
       left join account as debit_account
       on debit_account.id = tx.debit_acc_id
       left join account as credit_account
       on credit_account.id = tx.credit_acc_id
       inner join temtx
       on ((tx.debit_acc_id in (select * from descendants(temtx.debit_acc_id))) and
           (tx.credit_acc_id in (select * from descendants(temtx.credit_acc_id))))
       where (tx.company_id = 304 and temtx.debit_p = 't' and temtx.customer_p = 'f' and
              (cheque_event.to_state_id = cheque.state_id or (cheque_event.to_state_id is null)))
       order by tx_date desc, tx.description desc;

-- COMPANY CREDITS
select tx_date, tx.id, tx.description, temtx.debit_p, tx.amount,
       cheque.due_date, cheque.state_id,
       debit_account.title as debit_account, credit_account.title as credit_account
       from tx
       left join cheque_event
       on cheque_event.tx_id = tx.id
       left join cheque
       on cheque.id = cheque_event.cheque_id
       left join account as debit_account
       on debit_account.id = tx.debit_acc_id
       left join account as credit_account
       on credit_account.id = tx.credit_acc_id
       inner join temtx
       on ((tx.debit_acc_id in (select * from descendants(temtx.debit_acc_id))) and
           (tx.credit_acc_id in (select * from descendants(temtx.credit_acc_id))))
       where (tx.company_id = 304 and temtx.debit_p = 'f' and temtx.customer_p = 'f' and
              (cheque_event.to_state_id = cheque.state_id or (cheque_event.to_state_id is null)))
       order by tx_date desc, tx.description desc;

-- ALL TX
select tx_date, tx.id, tx.description, temtx.debit_p, tx.amount,
       cheque.due_date, cheque.state_id,
       debit_account.title as debit_account, credit_account.title as credit_account
       from tx
       left join cheque_event
       on cheque_event.tx_id = tx.id
       left join cheque
       on cheque.id = cheque_event.cheque_id
       left join account as debit_account
       on debit_account.id = tx.debit_acc_id
       left join account as credit_account
       on credit_account.id = tx.credit_acc_id
       inner join temtx
       on ((tx.debit_acc_id in (select * from descendants(temtx.debit_acc_id))) and
           (tx.credit_acc_id in (select * from descendants(temtx.credit_acc_id))))
       where (tx.company_id = 45 and (temtx.customer_p = 'f' or temtx.customer_p = 't') and
              (cheque_event.to_state_id = cheque.state_id or (cheque_event.to_state_id is null)))
       order by tx_date desc, tx.description desc;


-- DEBTORS/CREDITORS

select * from tx
       left join cheque_event
       on cheque_event.tx_id = tx.id
       left join cheque
       on cheque.id = cheque_event.cheque_id
       left join account as debit_account
       on debit_account.id = tx.debit_acc_id
       left join account as credit_account
       on credit_account.id = tx.credit_acc_id
       inner join temtx
       on ((tx.debit_acc_id in (select * from descendants(temtx.debit_acc_id))) and
           (tx.credit_acc_id in (select * from descendants(temtx.credit_acc_id))))
           where

select * from temtx where temtx.credit_acc_id in (select * from lineage (7));

select * from tx
inner join temtx on ((tx.debit_acc_id in (select * from descendants (temtx.debit_acc_id)))
                     and
                     (tx.credit_acc_id in (select * from descendants (temtx.credit_acc_id))))
where (tx.company_id = 45);



select * from tx where tx.amount = 15000;

select * from cheque.event where
