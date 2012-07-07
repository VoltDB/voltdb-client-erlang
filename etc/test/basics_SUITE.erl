%%%-------------------------------------------------------------------
%%% File     : erlvolt/test/basics_SUITE.erl
%%% Descr    : Suite #2: Tests of basic SQL
%%% Author   : H. Diedrich
%%% Requires : Erlang 14B (prior may not have ct_run)
%%%-------------------------------------------------------------------
%%%
%%% Run from erlvolt/:
%%%     make test
%%%
%%% Results see:
%%%     etc/test/index.html
%%%
%%%-------------------------------------------------------------------

-module(basics_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

%% Optional suite settings
%%--------------------------------------------------------------------
%% Function: suite() -> Info
%% Info = [tuple()]
%%--------------------------------------------------------------------

suite() ->
    [{timetrap,{seconds,30}}].

%% Mandatory list of test cases and test groups, and skip orders.
%%--------------------------------------------------------------------
%% Function: all() -> GroupsAndTestCases | {skip,Reason}
%% GroupsAndTestCases = [{group,GroupName} | TestCase]
%% GroupName = atom()
%% TestCase = atom()
%% Reason = term()
%%--------------------------------------------------------------------

all() ->
    [delete_all,
     insert_only,
     insert_and_read_back].

%% Optional suite pre test initialization
%%--------------------------------------------------------------------
%% Function: init_per_suite(Config0) ->
%%               Config1 | {skip,Reason} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%%--------------------------------------------------------------------

init_per_suite(Config) ->

    % if this fails, focus on environment_SUITE to fix test setup.
    crypto:start(),
    application:start(erlvolt),
    erlvolt:add_pool(test_pool, [{"localhost", 21212}]),
    Config.

%% Optional suite post test wind down
%%--------------------------------------------------------------------
%% Function: end_per_suite(Config0) -> void() | {save_config,Config1}
%% Config0 = Config1 = [tuple()]
%%--------------------------------------------------------------------

end_per_suite(_Config) ->
    erlvolt:close_pool(test_pool),
    ok.

%% A test case. The ok is irrelevant. What matters is, if it returns.
%%--------------------------------------------------------------------
%% Function: TestCase(Config0) ->
%%               ok | exit() | {skip,Reason} | {comment,Comment} |
%%               {save_config,Config1} | {skip_and_save,Reason,Config1}
%% Config0 = Config1 = [tuple()]
%% Reason = term()
%% Comment = term()
%%--------------------------------------------------------------------

%% Test Case: Delete all records in the test database
%%--------------------------------------------------------------------
delete_all(_) ->

    erlvolt:call_procedure(test_pool, "@AdHoc", ["DELETE FROM votes"]),
    ok.


%% Test Case: Make an Insert
%%--------------------------------------------------------------------
insert_only(_) ->

    erlvolt:call_procedure(test_pool, "Initialize", [3, "Hugo, Bert, Rupert"]),
    ok.

%% Test Case: Make an Insert and Select it back
%%--------------------------------------------------------------------
insert_and_read_back(_) ->

    delete_all(),

    erlvolt:call_procedure(test_pool, "Initialize", [3, "Hugo, Bert, Rupert"]),

    Result = erlvolt:call_procedure(test_pool, "@AdHoc", ["select COUNT(*) as C from contestants"]),
    {result, { voltresponse, _, [ Table | _ ] }} = Result,
    Row = erlvolt:get_row(Table, 1),
    Count = erlvolt:get_integer(Row, Table, "C"),

    % find this output by clicking on the test name, then case name in test/index.html
    io:format("Result: ~n~p ~n", [Result]),
    io:format("Should be: ~n~p ~n", ["{result,{voltresponse,{0,<<0,0,_,0,0,0,0,_>>,1,<<>>,128,<<>>,<<>>,_},[{volttable,[<<\"C\">>],[6],[{voltrow,[3]}]}]}}"]),
    io:format("Row: ~n~p ~n", [Row]),
    io:format("Should be: ~n~p ~n", ["{voltrow,[3]}"]),
    io:format("~p (should be 3)~n", [Count]),
    {result,{voltresponse,{0,<<0,0,_,0,0,0,0,_>>,1,<<>>,128,<<>>,<<>>,_}, [{volttable,[<<"C">>],[6],[{voltrow,[3]}]}]}} = Result,
    Row = {voltrow,[3]},
    Count = 3,
    ok.

delete_all() ->

    erlvolt:call_procedure(test_pool, "@AdHoc", ["DELETE FROM contestants"]),
    erlvolt:call_procedure(test_pool, "@AdHoc", ["DELETE FROM area_code_state"]),
    erlvolt:call_procedure(test_pool, "@AdHoc", ["DELETE FROM votes"]).
