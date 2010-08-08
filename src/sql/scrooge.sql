drop table contact;
drop table project;
drop table project_fsm;
drop table project_status;
drop table tx;
drop table temtx;
drop table cheque_fsm;
drop table cheque;
drop table cheque_status;

drop table company;
-- drop table debit_account;
-- drop table credit_account;
drop table account;
-- drop table account_type;

drop table bank;
drop table tof;


--- Basic config ------------------------------

create table tof ( -- taxation office
       id serial primary key
       ,title varchar(64) unique not null
);

create table bank (
       id serial primary key
       ,title varchar(64) unique not null
);

--- Companies and contacts ------------------------------

create table company (
       id		serial primary key
       ,title		varchar(256) unique not null
       ,occupation	varchar(64)
       ,tof_id       	integer references tof(id)
       ,tin       	char(9) unique
       ,address		varchar(256)
       ,city		varchar(64)
       ,pobox		integer check (pobox > 10000 and pobox < 89999)
       ,zipcode		integer check (zipcode > 0)
);


create table contact (
       id serial primary key
       ,company_id integer not null references company(id)
       ,tag varchar(32)
       ,phone varchar(32)
);


--- Accounts -----------------------------

create table account (
       id serial primary key
       ,title varchar(128) unique not null
       ,parent_id integer references account(id)
       ,debit_p boolean not null
);


--- Cheques ------------------------------

create table cheque_status (
       status varchar(32) primary key
       ,description varchar(32) 
);
insert into cheque_status (status, description) values('pending', 'Εκκρεμεί');
insert into cheque_status (status, description) values('paid', 'Πληρωμένη');
insert into cheque_status (status, description) values('bounced', 'Ακάλυπτη');
insert into cheque_status (status, description) values('returned', 'Επιστράφηκε');

create table cheque_fsm (
       id serial primary key
       ,description varchar(256)
       ,debit_acc_id integer references account(id)
       ,credit_acc_id integer references account(id)
       ,old_status varchar(16) references cheque_status(status)
       ,new_status varchar(16) references cheque_status(status)
);

create table cheque (
       id serial primary key
       ,bank_id integer not null references bank(id)
       ,company_id integer not null references company(id)
       ,due_date date not null
       ,amount integer not null check (amount > 0) 
       ,status varchar(8) not null references cheque_status(status) default 'pending'
       ,payable_p boolean default 'f'
);



--- Transactions ------------------------------

create table tx (
       id serial primary key
       ,tx_date date
       ,description varchar(256)
       ,debit_acc_id integer not null references account(id)
       ,credit_acc_id integer not null references account(id)
       ,company_id integer not null references company(id)
       ,amount integer check (amount > 0)
       ,src_id integer
       ,src_tbl varchar(16)
);

create table temtx (
       id serial primary key 
       ,description varchar(256) not null
       ,debit_acc_id integer references account(id)
       ,credit_acc_id integer references account(id)
);



--- Projects ------------------------------

create table project_status (
       status varchar(32) primary key
       ,description varchar(32) 
);
insert into project_status (status, description) values('quoted', 'Δόθηκε προσφορά');
insert into project_status (status, description) values('ongoing', 'Σε εξέλιξη');
insert into project_status (status, description) values('finished', 'Ολοκληρώθηκε');

create table project_fsm (
       id serial primary key
       ,description varchar(256) not null
       ,debit_acc_id integer not null references account(id)
       ,credit_acc_id integer not null references account(id)
       ,old_status varchar(16) not null references project_status(status)
       ,new_status varchar(16) not null references project_status(status)
);

create table project (
       id serial primary key
       ,company_id integer not null references company(id)
       ,description varchar(64) not null
       ,location varchar(64)
       ,price integer check (price > 0)
       ,start_date date 
       ,end_date date
       ,status varchar(8) not null references project_status(status) default 'quoted'
       ,vat integer check (vat > 0)
);



