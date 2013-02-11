%%%-------------------------------------------------------------------------%%%
%%% File        : erlunit.hrl                                               %%% 
%%% Description : Unit test convenience macros                              %%%      
%%% Version     : 0.2.8.2/alpha                                             %%%
%%% Copyright   : (c) 2010 Eonblast Corporation http://www.eonblast.com     %%%
%%% License     : MIT - see LICENSE                                         %%%
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%
%%% Created     : 01 May 2010                                               %%%
%%% Changed     : 10 May 2010 - see CHANGES                                 %%%
%%% Tested on   : Erlang R13B01                                             %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%% Include this file in your test program to use macros. if you don't use  %%%
%%% Macros, you don't need to include this file.                            %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%


%%%-------------------------------------------------------------------------%%%
%%% Imperative Macros                                                       %%%
%%%-------------------------------------------------------------------------%%%


-define(ERLUNIT_SEQ_TRUE_MSG(F, M), erlunit:true(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_SEQ_TRUE(F), erlunit:true(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_SEQ_FALSE_MSG(F, M), erlunit:false(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_SEQ_FALSE(F), erlunit:false(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_SEQ_NOT_TRUE_MSG(F, M), erlunit:not_true(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_SEQ_NOT_TRUE(F), erlunit:not_true(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_SEQ_NOT_FALSE_MSG(F, M), erlunit:not_false(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_SEQ_NOT_FALSE(F), erlunit:not_false(fun() -> F end, ??F, ?MODULE, ?LINE )).

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

-define(ERLUNIT_SEQ_PASS_MSG(F, M), erlunit:pass(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_SEQ_PASS(F), erlunit:pass(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_SEQ_FAIL_MSG(F, M), erlunit:fail(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_SEQ_FAIL(F), erlunit:fail(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_SEQ_EXITS_MSG(F, M), erlunit:exits(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_SEQ_EXITS(F), erlunit:exits(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_SEQ_ERROR_MSG(F, M), erlunit:error(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_SEQ_ERROR(F), erlunit:error(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_SEQ_THROWS_MSG(F, M), erlunit:throws(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_SEQ_THROWS(F), erlunit:throws(fun() -> F end, ??F, ?MODULE, ?LINE )).


%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

-define(ERLUNIT_SEQ_EQUAL_MSG(F, R, M), erlunit:equal(fun() -> F end, R, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_SEQ_EQUAL(F, R), erlunit:equal(fun() -> F end, R, "Expect " ++ ??F ++ " == " ++ ??R, ?MODULE, ?LINE)).

-define(ERLUNIT_SEQ_NOT_EQUAL_MSG(F, R, M), erlunit:not_equal(fun() -> F end, R, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_SEQ_NOT_EQUAL(F, R), erlunit:not_equal(fun() -> F end, R, "Expect " ++ ??F ++ " == " ++ ??R, ?MODULE, ?LINE)).

-define(ERLUNIT_SEQ_EXACT_MSG(F, R, M), erlunit:exact(fun() -> F end, R, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_SEQ_EXACT(F, R), erlunit:exact(fun() -> F end, R, "Expect " ++ ??F ++ " == " ++ ??R, ?MODULE, ?LINE)).

-define(ERLUNIT_SEQ_BIGGER_MSG(F, R, M), erlunit:bigger(fun() -> F end, R, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_SEQ_BIGGER(F, R), erlunit:bigger(fun() -> F end, R, "Expect " ++ ??F ++ " == " ++ ??R, ?MODULE, ?LINE)).

-define(ERLUNIT_SEQ_LESSER_MSG(F, R, M), erlunit:lesser(fun() -> F end, R, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_SEQ_LESSER(F, R), erlunit:lesser(fun() -> F end, R, "Expect " ++ ??F ++ " == " ++ ??R, ?MODULE, ?LINE)).

%%%-------------------------------------------------------------------------%%%
%%% Message Passing Macros                                                  %%%
%%%-------------------------------------------------------------------------%%%

-define(ERLUNIT_TRUE_MSG(F, M), erlunit:true(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_TRUE(F), erlunit:true(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_FALSE_MSG(F, M), erlunit:false(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_FALSE(F), erlunit:false(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_NOT_TRUE_MSG(F, M), erlunit:not_true(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_NOT_TRUE(F), erlunit:not_true(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_NOT_FALSE_MSG(F, M), erlunit:not_false(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_NOT_FALSE(F), erlunit:not_false(fun() -> F end, ??F, ?MODULE, ?LINE )).

%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

-define(ERLUNIT_PASS_MSG(F, M), erlunit:pass(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_PASS(F), erlunit:pass(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_FAIL_MSG(F, M), erlunit:fail(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_FAIL(F), erlunit:fail(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_EXITS_MSG(F, M), erlunit:exits(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_EXITS(F), erlunit:exits(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_ERROR_MSG(F, M), erlunit:error(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_ERROR(F), erlunit:error(fun() -> F end, ??F, ?MODULE, ?LINE )).

-define(ERLUNIT_THROWS_MSG(F, M), erlunit:throws(fun() -> F end, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_THROWS(F), erlunit:throws(fun() -> F end, ??F, ?MODULE, ?LINE )).


%%% - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

-define(ERLUNIT_EQUAL_MSG(F, R, M), erlunit:equal(fun() -> F end, R, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_EQUAL(F, R), erlunit:equal(fun() -> F end, R, "Expect " ++ ??F ++ " == " ++ ??R, ?MODULE, ?LINE)).

-define(ERLUNIT_NOT_EQUAL_MSG(F, R, M), erlunit:not_equal(fun() -> F end, R, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_NOT_EQUAL(F, R), erlunit:not_equal(fun() -> F end, R, "Expect " ++ ??F ++ " == " ++ ??R, ?MODULE, ?LINE)).

-define(ERLUNIT_EXACT_MSG(F, R, M), erlunit:exact(fun() -> F end, R, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_EXACT(F, R), erlunit:exact(fun() -> F end, R, "Expect " ++ ??F ++ " == " ++ ??R, ?MODULE, ?LINE)).

-define(ERLUNIT_BIGGER_MSG(F, R, M), erlunit:bigger(fun() -> F end, R, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_BIGGER(F, R), erlunit:bigger(fun() -> F end, R, "Expect " ++ ??F ++ " == " ++ ??R, ?MODULE, ?LINE)).

-define(ERLUNIT_LESSER_MSG(F, R, M), erlunit:lesser(fun() -> F end, R, M ++ " | " ++ ??F, ?MODULE, ?LINE )).
-define(ERLUNIT_LESSER(F, R), erlunit:lesser(fun() -> F end, R, "Expect " ++ ??F ++ " == " ++ ??R, ?MODULE, ?LINE)).
