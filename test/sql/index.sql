create or replace function utc_now()
  returns timestamptz as
$body$
begin
    return now() at time zone 'utc';
end;
  $body$
  language plpgsql;

drop table if exists "tableD";
drop table if exists tablec;
drop table if exists t_able_b;
drop table if exists table_a;

create table table_a (
    id integer generated always as identity primary key,
    a_column1 text not null,
    a_column2 varchar(255),
    a_column3 timestamp without time zone,
    a_column4 date unique default (utc_now())
);

create table t_able_b (
    id bigint primary key generated always as identity,
    column1 smallint,
    column2 boolean not null,
    column3 timestamp without time zone constraint "unique_column" unique constraint "default_column" default (utc_now())
);

create table tablec (
    id integer generated always as identity,
    id2 integer,
    tableccolumn1 decimal not null,
    tableccolumn2 timestamp without time zone not null,
    constraint "pk_column" primary key(id, id2)
);

create table "tableD" (
    "iD" integer,
    "fkColumn" bigint,
    fk_column integer references table_a(id),
    foreign key("fkColumn") references t_able_b(id)
);

create or replace function truncate_tables()
  returns void as
$body$
begin
    truncate table table_a restart identity cascade;
    truncate table t_able_b restart identity cascade;
    truncate table tablec restart identity cascade;
    truncate table "tableD" restart identity cascade;
end;
  $body$
  language plpgsql;
