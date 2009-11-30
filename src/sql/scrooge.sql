DROP TABLE project;
DROP TABLE cheque;

DROP TABLE tx;
DROP TABLE tx_type;

DROP TABLE company;
DROP TABLE account;

DROP TABLE bank;
DROP TABLE tof;
DROP TABLE city;
DROP TABLE webuser;

CREATE TABLE webuser (
       id		serial,
       username		varchar(64),
       password		varchar(64),
       webrole		varchar(64),
       CONSTRAINT webuser_pk PRIMARY KEY(id)
);

CREATE TABLE city (
       id		serial,
       title		varchar(64),
       CONSTRAINT city_pk PRIMARY KEY(id)
);

CREATE TABLE tof ( -- taxation office
       id		serial,
       title		varchar(64),
       CONSTRAINT tof_pk PRIMARY KEY(id)
);

CREATE TABLE bank (
       id		serial,
       title		varchar(64),
       CONSTRAINT bank_fk PRIMARY KEY(id)
);



-- create table account_group (
--        id               serial,
--        basename         varchar(32),
--        title		varchar(128),
--        debit_p  boolean,
--        CONSTRAINT account_group_pk PRIMARY KEY(id)
-- );

CREATE TABLE account (
       id		    serial,
       parent_id	    integer,
       title	   	    varchar(128),
       -- account_group_id	    integer NOT NULL,
       debit_p	    boolean,
       CONSTRAINT account_pk PRIMARY KEY(id),
       CONSTRAINT parent_fk FOREIGN KEY(parent_id) REFERENCES account(id)
       -- CONSTRAINT account_group_fk FOREIGN KEY(account_group_id) REFERENCES account_group(id)
);

CREATE TABLE company (
       id		serial,
       title		varchar(256),
       occupation	varchar(64),
       tof_id       	integer,
       tin       	char(9),
       address		varchar(256),
       city_id		integer,
       pobox		integer,
       zipcode		integer,
       CONSTRAINT company_fk PRIMARY KEY(id),
       CONSTRAINT city_fk FOREIGN KEY(city_id) REFERENCES city(id),
       CONSTRAINT tof_fk FOREIGN KEY(tof_id) REFERENCES tof(id)
);


CREATE TABLE tx_type (
       id               serial,
       title		varchar(256),
       debit_acc_id     integer NOT NULL,
       credit_acc_id    integer NOT NULL,
       CONSTRAINT tx_type_pk PRIMARY KEY(id),
       CONSTRAINT debit_acc_fk FOREIGN KEY(debit_acc_id) REFERENCES account(id),
       CONSTRAINT credit_acc_fk FOREIGN KEY(credit_acc_id) REFERENCES account(id)
);

CREATE TABLE tx (
       id		serial,
       tx_date		date,
       title		varchar(256),
       tx_type_id	integer NOT NULL,
       company_id	integer NOT NULL,
       amount		integer,
       cheque_id	integer,
       CONSTRAINT tx_pk PRIMARY KEY(id), 
       CONSTRAINT tx_type_fk FOREIGN KEY(tx_type_id) REFERENCES tx_type(id),
       CONSTRAINT company_fk FOREIGN KEY(company_id) REFERENCES company(id)
);


CREATE TABLE cheque (
       id		serial,
       bank_id          integer NOT NULL,
       issuer		varchar(256),
       due_date		date,
       amount		integer,
       CONSTRAINT cheque_pk PRIMARY KEY(id), 
       CONSTRAINT bank_fk FOREIGN KEY(bank_id) REFERENCES bank(id)
);


CREATE TABLE project (
       id		serial,
       company_id	integer NOT NULL,
       title		varchar(64),
       location		varchar(64),
       price		integer,
       CONSTRAINT project_pk PRIMARY KEY(id),
       CONSTRAINT company_fk FOREIGN KEY(company_id) REFERENCES company(id)
);

