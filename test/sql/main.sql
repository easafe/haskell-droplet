create or replace function utc_now()
  returns timestamptz as
$body$
begin
    return now() at time zone 'utc';
end;
  $body$
  language plpgsql;

create table table_a (
    id integer generated always as identity primary key,
    a_column1 text not null,
    a_column2 varchar(255),
    a_column3 timestamp without time zone,
    a_column4 date default (utc_now())
);

create table t_able_b (
    id bigint generated always as identity primary key,
    column1 smallint,
    column2 boolean not null,
    column3 timestamp without time zone default (utc_now())
);

create table tablec (
    id integer generated always as identity primary key,
    tableccolumn1 decimal not null,
    tableccolumn2 timestamp without time zone not null
);

create or replace function truncate_tables()
  returns void as
$body$
begin
    truncate table table_a restart identity cascade;
    truncate table t_able_b restart identity cascade;
    truncate table tags restart identity cascade;
end;
  $body$
  language plpgsql;
