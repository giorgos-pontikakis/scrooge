drop table contact;
drop table project;
drop table tx;
drop table cheque;
-- drop table tx_type;

drop table company;
-- drop table debit_account;
-- drop table credit_account;
drop table account;
-- drop table account_type;

drop table bank;
drop table tof;



create table tof ( -- taxation office
       id varchar(8) primary key
       ,title varchar(64) unique
);

create table bank (
       id varchar(8) primary key
       ,title varchar(64) unique not null
);


create table company (
       id		serial primary key
       ,title		varchar(256)
       ,occupation	varchar(64)
       ,tof_id       	varchar(8) references tof(id)
       ,tin       	char(9) unique
       ,address		varchar(256)
       ,city		varchar(6400)
       ,pobox		integer check (pobox > 10000 and pobox < 89999)
       ,zipcode		integer check (zipcode > 0)
);


create table cheque (
       id serial primary key
       ,bank_id varchar(8) not null references bank(id)
       ,company_id integer not null references company(id)
       ,due_date date not null
       ,amount integer not null check (amount > 0) 
       ,status varchar(8) default 'pending'
       ,payable boolean default "f"
-- status should be one of pending, paid, bounced, returned, transfered
);





-- create table account_type (
--        id  varchar (8) primary key
-- );

-- insert into account_type (id) values('debit');
-- insert into account_type (id) values('credit');

-- create table account (
--        id varchar(8) primary key
--        ,acc_type varchar(8) references account_type(id)
--        ,title varchar(128)
--        ,unique (id, acc_type)
-- );

-- create table debit_account (
--        id varchar(8) primary key references account(id)
--        ,acc_type varchar(8) default 'debit'
--        ,parent_id varchar(8) references debit_account(id)
--        ,check (acc_type = 'debit')
--        ,foreign key(id, acc_type) references account(id, acc_type)
-- );

-- create table credit_account (
--        id varchar(8) primary key references account(id)
--        ,acc_type varchar(8) default 'credit'
--        ,parent_id varchar(8) references credit_account(id)
--        ,check (acc_type = 'credit')
--        ,foreign key(id, acc_type) references account(id, acc_type)
-- );


create table account (
       id serial primary key
       ,title varchar(128)
       ,parent_id integer references account(id)
       ,debit_p boolean not null
);

-- create table debit_account (
--        id varchar(8) primary key references account(id)
--        ,parent_id varchar(8) references debit_account(id)
--        ,title varchar(128)
-- );

-- create table credit_account (
--        id varchar(8) primary key references account(id)
--        ,title varchar(128)
--        ,parent_id varchar(8) references credit_account(id)
-- );


create table tx (
       id		serial primary key
       ,tx_date		date
       ,description	varchar(256)
       ,debit_acc_id    integer not null references account(id)
       ,credit_acc_id   integer not null references account(id)
       ,company_id	integer not null references company(id)
       ,amount          integer check (amount > 0)
       ,cheque_id       integer references cheque(id)
       ,check ((cheque_id is null and amount is not null) 
            or (cheque_id is not null and amount is null))
);

create table autotx (
       id serial primary key
       ,description varchar(256)
       ,debit_acc_id integer not null references account(id)
       ,credit_acc_id integer not null references account(id)
);

create table project (
       id serial primary key
       ,company_id integer not null references company(id)
       ,title varchar(64)
       ,location varchar(64)
       ,price integer check (price > 0)
);


create table contact (
       id serial primary key
       ,company_id integer not null references company(id)
       ,tag varchar(32)
       ,phone varchar(32)
);

-- create table tx_type (
--        id               serial
--        title		varchar(256)
--        debit_acc_id     integer not null
--        credit_acc_id    integer not null
--        constraint tx_type_pk primary key(id)
--        constraint debit_acc_fk foreign key(debit_acc_id) references account(id)
--        constraint credit_acc_fk foreign key(credit_acc_id) references account(id)
-- );




