%%%-------------------------------------------------------------------
%%% File     : erlvolt/etc/test/environment_SUITE.erl
%%% Descr    : Suite #1 - testing the test setup, db and pathes =
%%%            availability of crypto app, erlvolt app and test db.
%%% Author   : H. Diedrich
%%% Requires : Erlang 14B (prior may not have ct_run)
%%%-------------------------------------------------------------------
%%%
%%% THIS SUITE DOES NO ACTUAL TESTS BUT CHECKS THE TEST DATABASE ETC.
%%% Test Cases are in this high granularity for clear failure reports.
%%%
%%% Run from erlvolt/:
%%%     make test
%%%
%%% Results see:
%%%     etc/test/index.html
%%%
%%%-------------------------------------------------------------------

-module(environment_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

% List of test cases.
% Test cases have self-explanatory names.
%%--------------------------------------------------------------------
all() ->
    [initializing_crypto_app,
     initializing_erlvolt_app,
     accessing_erlvolt_module,
     connecting_to_db_and_creating_a_pool,
     insert,
     select
     ].

% nothing, but forces call of clean up
%%--------------------------------------------------------------------
init_per_suite(Config) ->
    Config.

% clean up
%%--------------------------------------------------------------------
end_per_suite(_) ->
    erlvolt:close_pool(environment_test_pool),
    ok.

% Test Case: Test if the crypt app is available. This detects a path error.
%%--------------------------------------------------------------------
initializing_crypto_app(_) ->
    crypto:start(),
    ok.

% Test Case: Test if the erlvolt app is available. This detects a path error.
%%--------------------------------------------------------------------
initializing_erlvolt_app(_) ->
    application:start(erlvolt),
    ok.

% Test Case: Test if the erlvolt module is available. This detects a path error.
%%--------------------------------------------------------------------
accessing_erlvolt_module(_) ->
    erlvolt:modules(),
    ok.

% Test Case: Test if we can connect to the test db and create a connection pool.
%%--------------------------------------------------------------------
connecting_to_db_and_creating_a_pool(_) ->
    erlvolt:add_pool(environment_test_pool, [{"localhost", 21212}]),
    ok.


% Test Case: Test if we can call a procedure, insert a record.
%%--------------------------------------------------------------------
insert(_) ->
    erlvolt:call_procedure(environment_test_pool, "Initialize", [1,"Karl Egon"]),
    ok.

% Test Case: Test if we can select, adhoc.
%%--------------------------------------------------------------------
select(_) ->

    delete_all(),

    erlvolt:call_procedure(environment_test_pool, "Initialize", [2,"Karl, Egon"]),
    Result = erlvolt:call_procedure(environment_test_pool, "@AdHoc", ["select COUNT(*) as CNT from contestants"]),
    {result, { voltresponse, _, [ Table | _ ] }} = Result,
    Row = erlvolt:get_row(Table, 1),
    Count = erlvolt:get_integer(Row, Table, "CNT"),
    Count = 2,
    ok.

delete_all() ->

    erlvolt:call_procedure(environment_test_pool, "@AdHoc", ["DELETE FROM contestants"]),
    erlvolt:call_procedure(environment_test_pool, "@AdHoc", ["DELETE FROM area_code_state"]),
    erlvolt:call_procedure(environment_test_pool, "@AdHoc", ["DELETE FROM votes"]).
