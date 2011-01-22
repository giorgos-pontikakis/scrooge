drop table project;
drop table project_status;

drop table cheque;
drop table cheque_status;

drop table temtx;
drop table tx;

drop table contact;
drop table company;
drop table account;

drop table city;
drop table bank;
drop table tof;
drop table usr;


--- Users ------------------------------

create table usr (
       username varchar(128) unique not null primary key,
       password varchar(128) not null,
       authgroup varchar(128) not null
);


--- Basic config ------------------------------

create table tof ( -- Tax Office
       id serial primary key
       ,title varchar(64) unique not null
);

create table bank (
       id serial primary key
       ,title varchar(64) unique not null
);

create table city (
       id serial primary key
       ,title varchar(64) unique not null
);



--- Companies and contacts ------------------------------

create table company (
       id serial primary key,
       title varchar(256) unique not null,
       occupation varchar(64),
       tof_id integer references tof(id),
       tin char(9) unique,
       address varchar(256),
       city_id integer references city(id),
       pobox integer check (pobox > 10000 and pobox < 99999),
       zipcode integer check (zipcode > 0 and zipcode < 99999)
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



--- Transactions ------------------------------

create table tx (
       id serial primary key
       ,tx_date date
       ,description varchar(256)
       ,debit_acc_id integer not null references account(id)
       ,credit_acc_id integer not null references account(id)
       ,company_id integer not null references company(id)
       ,amount integer check (amount > 0)

);

create table temtx (
       id serial primary key
       ,description varchar(256) not null
       ,debit_acc_id integer references account(id)
       ,credit_acc_id integer references account(id)
);



--- Cheques ------------------------------

create table cheque_status (
       id varchar(32) primary key
       ,description varchar(32)
);
insert into cheque_status (id, description) values('pending', 'Εκκρεμεί');
insert into cheque_status (id, description) values('paid', 'Πληρωμένη');
insert into cheque_status (id, description) values('bounced', 'Ακάλυπτη');
insert into cheque_status (id, description) values('returned', 'Επιστράφηκε');

create table cheque (
       id serial primary key
       ,bank_id integer not null references bank(id)
       ,company_id integer not null references company(id)
       ,due_date date not null
       ,amount integer not null check (amount > 0)
       ,status varchar(32) not null references cheque_status(id)
       ,payable_p boolean default 'f'
);



--- Projects ------------------------------

create table project_status (
       id varchar(32) primary key
       ,description varchar(32)
);
insert into project_status (id, description) values('quoted', 'Δόθηκε προσφορά');
insert into project_status (id, description) values('ongoing', 'Σε εξέλιξη');
insert into project_status (id, description) values('finished', 'Ολοκληρώθηκε');
insert into project_status (id, description) values('cancelled', 'Ακυρώθηκε');


-- project status transitions

create table project (
       id serial primary key
       ,company_id integer not null references company(id)
       ,description varchar(64) not null
       ,location varchar(64)
       ,price integer check (price > 0)
       ,start_date date
       ,end_date date
       ,status varchar(32) not null references project_status(id) default 'quoted'
       ,vat integer check (vat > 0)
);
