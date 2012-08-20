alter table account_role alter column description set not null;
alter table account_role add constraint account_role_description_key unique(description);
alter table contact add check (rank > 0);
alter table bill add check (rank > 0);
alter table project drop column location;
alter table cheque_stran alter column from_state_id set not null;
alter table cheque_stran alter column temtx_id set not null;
alter table cheque_stran alter column receivable_p set not null;

alter table project_stran rename column from_state to from_state_id;
alter table project_stran rename column to_state to to_state_id;
alter table project_stran alter column from_state_id set not null;
delete from project_stran;
alter table project_stran alter column temtx_id set not null;
