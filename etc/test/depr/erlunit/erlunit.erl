%%%----------------------------------------------------------------------------
%%% File        : erlunit.erl
%%% Description : Test functions - import this file and use its functions.
%%% Version     : 0.2.8.2/alpha
%%% Status      : alpha
%%% Copyright   : (c) 2010 Eonblast Corporation http://www.eonblast.com
%%% License     : MIT - see below 
%%% Author      : H. Diedrich <hd2010@eonblast.com>
%%% Created     : 18 Apr 2010
%%% Changed     : 15 May 2010 - see CHANGES
%%% Tested on   : Erlang R13B01
%%%----------------------------------------------------------------------------
%%%
%%% This Module contains test functions to test other modules.
%%%
%%% Usage, see sample.erl. Basically -
%%%
%%%    either:                  or:                         or:
%%%	   erlunit:start()      |   Test = erlunit:start(),  |  erlunit:start()
%%%    erlunit:equal(1, 1), |   Test ! { equal, 1, 1 },  |  ?ERLUNIT_EQUAL(1,1),
%%%    ...  				|   ...                      |  ...
%%%	   erlunit:execute().   |   erlunit:execute().       |  erlunit:execute().
%%%
%%% There are also suites, names for tests and concurrent execution.
%%%
%%%----------------------------------------------------------------------------
%%%
%%% Start source inspection in sample.erl.
%%%
%%% There are Erlang masters on erlang-questions, this here source is
%%% not written by one. Beware of copying mistakes if you are learning.
%%% There may be a slight advantage to that, the code is kind of plain.
%%%
%%% Mail to hd2010@eonblast.com with questions and suggestions, I will
%%% be quite happy to answer. - Henning
%%%
%%% This bit is dedicated to Joe, who had me smile now and then reading
%%% his book Programming Erlang. Thanks, Joe! You are the man.
%%%
%%% Thanks to Stefan Marr, Fred Hebert and Richard O'Keefe.
%%%
%%%----------------------------------------------------------------------------
% 
% Copyright (c) 2010 Eonblast Corporation http://www.eonblast.com
% 
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
% 
% The above copyright notice, including link, and this permission notice shall 
% be included in all copies or substantial portions of the Software.
% 
% THE SOFTWARE IS PROVIDED "AS IS",  WITHOUT WARRANTY  OF ANY KIND, EXPRESS OR
% IMPLIED,  INCLUDING BUT NOT LIMITED TO  THE  WARRANTIES  OF  MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
% AUTHORS  OR  COPYRIGHT  HOLDER  BE LIABLE  FOR  ANY CLAIM,  DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR  THE USE  OR  OTHER DEALINGS IN
% THE SOFTWARE.
% 
%%%----------------------------------------------------------------------------

-module(erlunit).
-vsn("0.2.8.2/alpha").
-author("H. Diedrich <hd2010@eonblast.com>").
-license("MIT - http://www.opensource.org/licenses/mit-license.php").
-copyright("(c) 2010 Eonblast Corporation http://www.eonblast.com").

%%%----------------------------------------------------------------------------

-export([start/0, start/1, execute/0, stats/2]).
-export([suite/1, suite/2]).
-export([true/1, not_true/1, false/1, not_false/1, pass/1, fail/1]).
-export([true/2, not_true/2, false/2, not_false/2, pass/2, fail/2]).
-export([true/3, not_true/3, false/3, not_false/3, pass/3, fail/3]).
-export([true/4, not_true/4, false/4, not_false/4, pass/4, fail/4]).
-export([exits/1, throws/1, error/1]).
-export([exits/2, throws/2, error/2]).
-export([exits/3, throws/3, error/3]).
-export([exits/4, throws/4, error/4]).
-export([exact/2, equal/2, not_equal/2, bigger/2, lesser/2]).
-export([exact/3, equal/3, not_equal/3, bigger/3, lesser/3]).
-export([exact/5, equal/5, not_equal/5, bigger/5, lesser/5]).

-export([echo/1, echo/2]).
-export([banner/1, banner/2, ascii_banner/0, ascii_banner/1, center/2]).
-export([strong_banner/1, strong_banner/2]). % legacy
-export([vecho/2, vecho/3]).
-export([alive/1,delimited/1]).

-export([glist/1, glist_add/3, glist_get/2, glist_get/3, glist_drop/2, glist_loop/0, glist_loop/3]).

-compile([{nowarn_unused_function, [passed/3, failed/3]}]).
-compile({no_auto_import,[error/1, error/2]}).


%%%----------------------------------------------------------------------------

-define(VERSION, "0.2.8.2/alpha").
-define(LIBRARY, "Erlunit").
-define(COPYRIGHT, "(c) 2010 Eonblast Corporation http://www.eonblast.com").

% -define(PROMPT, "\e[1;30merlunit:\e[0m ").
-define(PROMPT, "erlunit: ").
-define(INDENT, "         ").

-define(USRERR, "This is an error in the way you use erlunit, or an error in erlunit itself.").
-define(PRGERR, "This is an error in erlunit itself.").
-define(INVERT, "This is an inverted suite. Fails count for Passes, for testing the tests.").

-define(DEFAULT_SUITE_NAME, "Default Suite").

-define(V1, false).  			% verbosity settings
% not used: -define(V2, false). % higler level enabled
% not used: -define(V3, false). % means more details.

-define(D3, false).  % Debug verbosity:
-define(D4, false).  % Numbers mean different areas, not levels.
-define(D5, false).  % They are called out as hint in error messages.

-define(LINES, true). % governs display of lines in output.


%%%****************************************************************************
%%% INDIVIDUAL CHECKS
%%%****************************************************************************
%%%                                                                      
%%% This is repetive to avoid the hiding of details coming with macro use and
%%% more complicated function calls that anonymous function would entail.
%%%
%%% These function can be called by the main process or a suite process. 
%%%                                                                      
%%%---------------------------------------------------------------------checks-
%%% true - check if an expression returns true
%%%----------------------------------------------------------------------------
%%%
%%% For true/0 see (*). That is used for a different purpose.

true(msg) -> "Check for true";

true(A) -> true(A, true(msg)).

true(A, Message) -> true(whereis(suite), A, Message).

true(Suite, A, Message) when is_pid(Suite) -> true(Suite, A, Message, "", "").

true(A, Message, Module, Line) -> true(whereis(suite), A, Message, Module, Line).

true(Suite, A, Message, Module, Line) when is_pid(Suite) ->

		AA = payload(Suite, Message, A),
	
		if  AA ->
				passed(Suite, Message, "evaluates to ~w", [AA]);
	    	true ->
				failed(Suite, Message, "evaluates to ~w, should be true but is not | ~w ~w", [AA, Module, Line])
		end.

%%%                                                                      checks
%%%----------------------------------------------------------------------------
%%% not_true - in Erlang, this is not necessarily 'false'
%%%----------------------------------------------------------------------------
%%%

not_true(msg) -> "Check for not true";

not_true(A) -> not_true(A, not_true(msg)).

not_true(A, Message) -> not_true(whereis(suite), A, Message).

not_true(Suite, A, Message) when is_pid(Suite) -> not_true(Suite, A, Message, "", "").

not_true(A, Message, Module, Line) -> not_true(whereis(suite), A, Message, Module, Line).

not_true(Suite, A, Message, Module, Line) when is_pid(Suite) ->

	AA = payload(Suite, Message, A),

    if AA =/= true ->
			passed(Suite, Message, "evaluates to ~w, not true, as it should", [AA]);
       true ->
			failed(Suite, Message, "evaluates to ~w, but should not be true | ~w ~w", [AA, Module, Line])
       end.
       
%%%                                                                      checks
%%%----------------------------------------------------------------------------
%%% false
%%%----------------------------------------------------------------------------
%%%

false(msg) -> "Check for false";

false(A) -> false(A, false(msg)).

false(A, Message) -> false(whereis(suite), A, Message).

false(Suite, A, Message) when is_pid(Suite) -> false(Suite, A, Message, "", "").

false(A, Message, Module, Line) -> false(whereis(suite), A, Message, Module, Line).

false(Suite, A, Message, Module, Line) when is_pid(Suite) ->

	AA = payload(Suite, Message, A),

    if AA == false ->
			passed(Suite, Message, "evaluates to ~w", [AA]);
       true ->
			failed(Suite, Message, "evaluates to ~w, should be false but is not | ~w ~w", [AA, Module, Line])
       end.

%%%                                                                      checks
%%%----------------------------------------------------------------------------
%%% not_false - in Erlang, this is not necessarily 'true'
%%%----------------------------------------------------------------------------
%%%


not_false(msg) -> "Check for not false";

not_false(A) -> not_false(A, not_false(msg)).

not_false(A, Message) -> not_false(whereis(suite), A, Message).

not_false(Suite, A, Message) when is_pid(Suite) -> not_false(Suite, A, Message, "", "").

not_false(A, Message, Module, Line) -> not_false(whereis(suite), A, Message, Module, Line).

not_false(Suite, A, Message, Module, Line) when is_pid(Suite) ->

	AA = payload(Suite, Message, A),

    if AA =/= false ->
			passed(Suite, Message, "evaluates to ~w, not false, as it should", [AA]);
       true ->
			failed(Suite, Message, "evaluates to ~w, but should not be false | ~w ~w", [AA, Module, Line])
       end.

%%%                                                                      checks
%%%----------------------------------------------------------------------------
%%% pass - ok means: Fun throws NO exception
%%%----------------------------------------------------------------------------
%%%

pass(msg) -> "Check for passing without exception";

pass(Fun) when is_function(Fun) -> pass(Fun, pass(msg)).

pass(Fun, Message) when is_function(Fun) -> pass(whereis(suite), Fun, Message).

pass(Suite, Fun, Message) when is_function(Fun) -> pass(Suite, Fun, Message, "", "").

pass(Fun, Message, Module, Line) when is_function(Fun) -> pass(whereis(suite), Fun, Message, Module, Line).

pass(Suite, Fun, Message, Module, Line) when is_function(Fun) ->

    try 
    	Fun(),
    	passed(Suite, Message, "passes ok")
	catch
    	throw:Term -> 
    		failed(Suite, Message, "throws exception (~w) but should pass ok | ~w  ~w~n~p", [Term, Module, Line, erlang:get_stacktrace()]);
    	exit:Reason -> 
    		failed(Suite, Message, "make exit (~w) but should pass ok | ~w  ~w~n~p", [Reason, Module, Line, erlang:get_stacktrace()]);
    	error:Reason -> 
    		failed(Suite, Message, "runs into error (~w) but should pass ok | ~w ~w~n~p", [Reason, Module, Line, erlang:get_stacktrace()])
	end.

%%%                                                                      checks
%%%----------------------------------------------------------------------------
%%% fail - ok when Fun throws an exception
%%%----------------------------------------------------------------------------
%%%

fail(msg) -> "Check for exception";

fail(Fun) when is_function(Fun) -> fail(Fun, fail(msg)).

fail(Fun, Message) when is_function(Fun) -> fail(whereis(suite), Fun, Message).

fail(Suite, Fun, Message) when is_function(Fun) -> fail(Suite, Fun, Message, "", "").

fail(Fun, Message, Module, Line) when is_function(Fun) -> fail(whereis(suite), Fun, Message, Module, Line).

fail(Suite, Fun, Message, Module, Line) when is_function(Fun) ->

    try 
    	Result = Fun(),
    	failed(Suite, Message, "passes ok (~w) but should fail | ~w  ~w~n~p", [Result, Module, Line, erlang:get_stacktrace()])
	catch
    	throw:Term -> 
    		passed(Suite, Message, "throws exception (~w), failing as it should", [Term]);
    	exit:Reason -> 
    		passed(Suite, Message, "makes exit (~w), failing as it should", [Reason]);
    	error:Reason -> 
    		passed(Suite, Message, "runs into error (~w), failing as it should", [Reason])
	end.

%%%                                                                      checks
%%%----------------------------------------------------------------------------
%%% throws - ok when Fun throws an exception
%%%----------------------------------------------------------------------------
%%%


throws(msg) -> "Check for throw";

throws(Fun) when is_function(Fun) -> throws(Fun, throws(msg)).

throws(Fun, Message) when is_function(Fun) -> throws(whereis(suite), Fun, Message).

throws(Suite, Fun, Message) when is_function(Fun) -> throws(Suite, Fun, Message, "", "").

throws(Fun, Message, Module, Line) when is_function(Fun) -> throws(whereis(suite), Fun, Message, Module, Line).

throws(Suite, Fun, Message, Module, Line) when is_function(Fun) ->

    try 
    	Fun(),
    	failed(Suite, Message, "passes ok but should throw and fail | ~w ~w~n~p", [Module, Line, erlang:get_stacktrace()])
	catch
    	throw:_Term -> 
    		passed(Suite, Message, "throws exception, as it should");
    	exit:_Reason -> 
    		failed(Suite, Message, "makes exit, but should throw and and fail | ~w ~w~n~p", [Module,Line, erlang:get_stacktrace()]);
    	error:_Reason -> 
    		failed(Suite, Message, "runs into error, but should throw and fail | ~w ~w~n~p", [Module,Line, erlang:get_stacktrace()])
	end.
%%%                                                                      checks
%%%----------------------------------------------------------------------------
%%% exits - ok when Fun calls exit
%%%----------------------------------------------------------------------------
%%%

exits(msg) -> "Check for exit being called";

exits(Fun) when is_function(Fun) -> exits(Fun, exits(msg)).

exits(Fun, Message) when is_function(Fun) -> exits(whereis(suite), Fun, Message).

exits(Suite, Fun, Message) when is_function(Fun) -> exits(Suite, Fun, Message, "", "").

exits(Fun, Message, Module, Line) when is_function(Fun) -> exits(whereis(suite), Fun, Message, Module, Line).

exits(Suite, Fun, Message, Module, Line) when is_function(Fun) ->

    try 
    	Fun(),
    	failed(Suite, Message, "passes ok but should throw and fail | ~w ~w~n~p", [Module,Line, erlang:get_stacktrace()])
	catch
    	throw:_Term -> 
    		failed(Suite, Message, "throws exception, as it should | ~w ~w~n~p", [Module,Line, erlang:get_stacktrace()]);
    	exit:_Reason -> 
    		passed(Suite, Message, "makes exit, but should throw and and fail");
    	error:_Reason -> 
    		failed(Suite, Message, "runs into error, but should throw and fail | ~w ~w~n~p", [Module,Line, erlang:get_stacktrace()])
	end.
%%%                                                                      checks
%%%----------------------------------------------------------------------------
%%% error - ok when Fun runs into error
%%%----------------------------------------------------------------------------
%%%

error(msg) -> "Check for having error";

error(Fun) when is_function(Fun) -> error(Fun, error(msg)).

error(Fun, Message) when is_function(Fun) -> error(whereis(suite), Fun, Message).

error(Suite, Fun, Message) when is_function(Fun) -> error(Suite, Fun, Message, "", "").

error(Fun, Message, Module, Line) when is_function(Fun) -> error(whereis(suite), Fun, Message, Module, Line).

error(Suite, Fun, Message, Module, Line) when is_function(Fun) ->

    try 
    	Fun(),
    	failed(Suite, Message, "passes ok but should run into error | ~w ~w~n~p", [Module, Line, erlang:get_stacktrace()])
	catch
    	throw:_Term -> 
    		failed(Suite, Message, "throws exception, but should run into error | ~w ~w~n~p", [Module,Line, erlang:get_stacktrace()]);
    	exit:_Reason -> 
    		failed(Suite, Message, "makes exit, but should run into error | ~w ~w~n~p", [Module,Line, erlang:get_stacktrace()]);
    	error:_Reason -> 
    		passed(Suite, Message, "runs into error, as it should")
	end.
%%%                                                                      checks
%%%----------------------------------------------------------------------------
%%% equal
%%%----------------------------------------------------------------------------
%%%

equal(msg) -> "Equality check".

equal(A, B) -> equal(A, B, equal(msg)).

equal(A, B, Message) -> equal(whereis(suite), A, B, Message).

equal(Suite, A, B, Message) when is_pid(Suite) -> equal(Suite, A, B, Message, "", "").

equal(A, B, Message, Module, Line) -> equal(whereis(suite), A, B, Message, Module, Line).

equal(Suite, A, B, Message, Module, Line) when is_pid(Suite) -> 

  try
	AA = payload(Suite, Message, A),
	BB = payload(Suite, Message, B),

    if 
         AA == BB ->
			passed(Suite, Message, "~w as it should", [AA]);
         true ->
         	failed(Suite, Message, "~w /= ~w but should be equal | ~w ~w", [AA, BB, Module, Line])
    end
  catch _:_ -> nil  
  end.
  
%%%                                                                      checks
%%%----------------------------------------------------------------------------
%%% exact
%%%----------------------------------------------------------------------------
%%%

exact(msg) -> "Exact Equality check".

exact(A, B) -> exact(A, B, exact(msg)).

exact(A, B, Message) -> exact(whereis(suite), A, B, Message).

exact(Suite, A, B, Message) when is_pid(Suite) -> exact(Suite, A, B, Message, "", "").

exact(A, B, Message, Module, Line) -> exact(whereis(suite), A, B, Message, Module, Line).

exact(Suite, A, B, Message, Module, Line) when is_pid(Suite) -> 

  try
	AA = payload(Suite, Message, A),
	BB = payload(Suite, Message, B),

    if 
         AA =:= BB ->
			passed(Suite, Message, "~w as it should", [AA]);
         true ->
         	failed(Suite, Message, "~w /= ~w but should be exact | ~w ~w", [AA, BB, Module, Line])
    end
  catch _:_ -> nil  
  end.
  
%%%                                                                      checks
%%%----------------------------------------------------------------------------
%%% not_equal
%%%----------------------------------------------------------------------------


not_equal(msg) -> "Non-Equality check".

not_equal(A, B) -> not_equal(A, B, not_equal(msg)).

not_equal(A, B, Message) -> not_equal(whereis(suite), A, B, Message).

not_equal(Suite, A, B, Message) when is_pid(Suite) -> not_equal(Suite, A, B, Message, "", "").

not_equal(A, B, Message, Module, Line) -> not_equal(whereis(suite), A, B, Message, Module, Line).

not_equal(Suite, A, B, Message, Module, Line) when is_pid(Suite) -> 

	AA = payload(Suite, Message, A),
	BB = payload(Suite, Message, B),

    if 
         AA /= BB ->
			passed(Suite, Message, "~w /= ~w as it should", [AA, BB]);
         true ->
         	failed(Suite, Message, "~w == ~w but should NOT be equal | ~w ~w", [AA, BB, Module, Line])
       end.

%%%                                                                      checks
%%%----------------------------------------------------------------------------
%%% bigger
%%%----------------------------------------------------------------------------
%%%

bigger(msg) -> "Check if bigger".

bigger(A, B) -> bigger(A, B, bigger(msg)).

bigger(A, B, Message) -> bigger(whereis(suite), A, B, Message).

bigger(Suite, A, B, Message) when is_pid(Suite) -> bigger(Suite, A, B, Message, "", "").

bigger(A, B, Message, Module, Line) -> bigger(whereis(suite), A, B, Message, Module, Line).

bigger(Suite, A, B, Message, Module, Line) when is_pid(Suite) -> 

	AA = payload(Suite, Message, A),
	BB = payload(Suite, Message, B),

    if 
         AA > BB ->
			passed(Suite, Message, "~w > ~w as it should", [A, B]);
         true ->
         	failed(Suite, Message, "~w <= ~w but should be bigger | ~w ~w", [A, B, Module, Line])
       end.

%%%                                                                      checks
%%%----------------------------------------------------------------------------
%%% lesser
%%%----------------------------------------------------------------------------
%%%

lesser(msg) -> "Non-Equality check".

lesser(A, B) -> lesser(A, B, lesser(msg)).

lesser(A, B, Message) -> lesser(whereis(suite), A, B, Message).

lesser(Suite, A, B, Message) when is_pid(Suite) -> lesser(Suite, A, B, Message, "", "").

lesser(A, B, Message, Module, Line) -> lesser(whereis(suite), A, B, Message, Module, Line).

lesser(Suite, A, B, Message, Module, Line) when is_pid(Suite) -> 

	AA = payload(Suite, Message, A),
	BB = payload(Suite, Message, B),

    if 
         AA < BB ->
			passed(Suite, Message, "~w < ~w as it should", [A, B]);
         true ->
         	failed(Suite, Message, "~w >= ~w but should be lesser | ~w ~w", [A, B, Module, Line])
       end.

%%%****************************************************************************
%%% COUNTER CALLS
%%%****************************************************************************
%%%
%%% These function can be called by the main process or a suite process. 
%%%                                                                      
%%%----------------------------------------------------------------------------
%%% passed - echo positive result and count it
%%%----------------------------------------------------------------------------


passed(Suite, Message, Result) -> passed(Suite, Message, Result, []).

passed(Suite, Message, Result, ResultParameter) ->

	try
		Suite ! passed,

	  	Options = get(options),

		% this works for both main process or suite processes
	  	case lists:member(nopasses,Options) of
	  		true -> nil;
	  		false ->

				SuiteName = glist_get(suitenames, Suite, ""),
%				io:format(?PROMPT ++ "+ ok | ~s~s~s~s | " ++ Result ++ ".~n",
				io:format(?PROMPT ++ "+ ok | ~s~s~s~s~n" ++ ?PROMPT ++ "     | --> " ++ Result ++ ".~n",
					[SuiteName, iff(SuiteName,": ",""), 
					"", % iff(length(SuiteName) > ((width()-15)*2), <<13,10,?INDENT,"     ` ">>,""),
					 delimited(Message) | delimited(ResultParameter)])
		end
	catch
		Type:Reason -> io:format("~n~n~nCrashed while printing success message. "
		                         ++ ?PRGERR ++"~n ~p ~p~n~n ~p ~n~n", [Type, Reason, erlang:get_stacktrace()])
	end.

%%%----------------------------------------------------------------------------
%%% failed - echo negative result and count it
%%%----------------------------------------------------------------------------


failed(Suite, Message, Result) -> failed(Suite, Message, Result, []).

failed(Suite, Message, Result, ResultParameter) ->

	try
		Suite ! failed,

		% this works for both main process or suite processes
	  	Options = get(options),

		FailTag = 
	  	case lists:member(inverted,get(flags)) of
			true -> "FAIL (but inverted for testing the tests = ok)";
			false -> "FAIL"
		end,

	  	case lists:member(nofails,Options) of
	  		true -> nil;
	  		false ->
	  			% colorizing
			  	case lists:member(colors,Options) of
	  				true -> Red = "\e[1;31m", UnRed = "\e[0m"; 
	  				false -> Red = "", UnRed = ""
	  			end,

				SuiteName = glist_get(suitenames, Suite, ""),
				io:format(?PROMPT ++ "~s | ##### ~s~s ~s~s~s | " ++ delimited(Result) ++ ". #####~n",
					[FailTag, SuiteName, iff(SuiteName,":",""), Red, delimited(Message), UnRed | delimited(ResultParameter)])
		end
	catch
		Type:Reason -> io:format("~n~n~nCrashed while printing failure message. " 
		                         ++ ?PRGERR ++"~n ~p ~p~n~n~p~n~n", [Type, Reason, erlang:get_stacktrace()])
	end.

%%%----------------------------------------------------------------------------
%%% crashed - echo negative result and count it
%%%----------------------------------------------------------------------------


crashed(Suite, Reason) ->

%		Suite ! crashed,
%
%	  	Options = get(options),
%
%	  	case lists:member(nocrashes,Options) of
%	  		true -> nil;
%	  		false ->
%	  			% colorizing
%			  	case lists:member(colors,Options) of
%	  				true -> Red = "\e[1;31m", UnRed = "\e[0m"; 
%	  				false -> Red = "", UnRed = ""
%	  			end,

 				Red = "\e[1;31m", UnRed = "\e[0m",

				SuiteName = glist_get(suitenames, Suite, ""),
				io:format(?PROMPT ++ "CRASH | XXXXX ~s~s ~s~w ~w~s XXXXX~n",
					[SuiteName, iff(SuiteName,":",""), Red, Reason, erlang:get_stacktrace(), UnRed]).
%		end.

%%%----------------------------------------------------------------------------
%%% kinda cut messages short, whatever gives
%%%----------------------------------------------------------------------------		

-define(DELIMIT, 100).
-define(DELIMIT_REPLACE, "....").

delimited([]) ->

	[];	

delimited(List) when is_list(List), length(List) > ?DELIMIT ->

	delimited(lists:append(lists:sublist(List, ?DELIMIT - length(?DELIMIT_REPLACE)), ?DELIMIT_REPLACE));

delimited(List) when is_list(List) ->

	[ delimited(X) || X <- List ];

delimited(Tuple) when is_tuple(Tuple) ->

	list_to_tuple([ delimited(X) || X <- tuple_to_list(Tuple) ]);

delimited(Item) when is_binary(Item), size(Item) > ?DELIMIT ->

	<<Limited:?DELIMIT/binary, _/binary>> = Item,
	<<Limited/binary, ".....">>;

delimited(Item) ->

	Item.


%%%****************************************************************************
%%% MAIN TEST
%%%****************************************************************************
%%%
%%% You can have multiple Main Tests, that are collections of suites
%%% but they should probably be executed in sequence, not in parallel.
%%%
%%%----------------------------------------------------------------------------
%%% Start                                                                 main
%%%----------------------------------------------------------------------------

start() -> start([]).

start(Options) when is_list(Options) ->

	ascii_banner(),

	echo("Start of Tests."),
	register(main, self()),

	put(options, Options),

	Stats = spawn(fun() -> stats("Overall", Options) end),
	force_register(stats, Stats),

	glist(suitenames), % mind you, before first suite() call.
	
	DefaultSuite = spawn(fun() -> suite(?DEFAULT_SUITE_NAME, [], self()) end),
	force_register(suite, DefaultSuite),
	self() ! { add, DefaultSuite, ?DEFAULT_SUITE_NAME },
		
	main.

%%%----------------------------------------------------------------------------
%%% execute - work the list of queued checks                              main
%%%----------------------------------------------------------------------------

execute() -> 
	
	execute_loop(start, [], []).
                                                                       
%%% non-exported: --------------------------------------------------------main-

execute_loop(Phase, SuitesActive, SuitesToPrint) ->

	vecho(?D3, "Main process phase: ~w | active suites: ~w | to print: ~w", [Phase, SuitesActive, SuitesToPrint]),

	receive
	Rcv ->
	vecho(?D3, "Main process receives: ~w", [Rcv]),
	
	case Rcv of
	
		%%%-suite-wait----------------------------------------------------main-

		{ add, Suite, Name } -> 
			if 
				Phase == start ->
					Suite ! done,
					execute_loop(Phase,  [ Suite | SuitesActive ],  lists:append(SuitesToPrint, [Suite])  );

				% protection vs misuse
				true ->
					exit("Too late to add ~s. " ++ ?USRERR ++ " Try D3.", [Name])
			end;

		{ done, Suite } -> 
			vecho(?D3, "~s done.", [glist_get(suitenames, Suite, "*")]),
			self() ! check_done,
			execute_loop(Phase,  lists:delete(Suite, SuitesActive), SuitesToPrint );

		check_done -> 
			% print_suites must be sent only once. Problem: check_done can come
			% in multiple times with empty SuitesActive list, depending on Suite message sequences.
			% Therefore, the if, and the phase forced to 'print'.
			if
				(Phase /= print) and (SuitesActive == []) -> 
					self() ! print_suites,
					NewPhase = print; % must be here, not later.
				true ->
					NewPhase = Phase
			end,
			execute_loop(NewPhase, SuitesActive, SuitesToPrint);
		
		%%%-printing------------------------------------------------------main-

		print_suites ->
			% There is always at least one suite, at least the default suite.
			% Print one and wait for ack in the form of { printed, Suite }, below.
			[ Suite | LeftToPrint ] = SuitesToPrint,
			Suite ! print,
			execute_loop(Phase, SuitesActive, LeftToPrint);
			
		{ printed, _Suite } ->
		    if 
				SuitesToPrint == [] -> self() ! print_summary; 
				true -> self() ! print_suites
			end,
			execute_loop(Phase,  SuitesActive, SuitesToPrint );
			
		print_summary ->
			stats ! print,
			self() ! finis,
			execute_loop(finish, SuitesActive, SuitesToPrint);
			
		%%%-wrap-up-------------------------------------------------------main-

		finis ->
			timer:sleep(200), % allow rogue processes to finish
			force_unregister(suitenames),
			force_unregister(suite),
			force_unregister(stats),
			force_unregister(main),
			complete;

		Any -> 
			vecho(?D3, "Handed on from main process to suite: ~w~n", [Any]),
			suite ! Any,
			execute_loop(Phase, SuitesActive, SuitesToPrint)
		end

		after 3000 ->
			exit("Main process stalled. " ++ ?USRERR ++ " Try 3D.")
	end.

	% TODO: return a value from stats: ok, fail or error.

%%%****************************************************************************
%%% SUITE
%%%****************************************************************************
%%%                                                                       
%%%----------------------------------------------------------------------------
%%% Create Suite Process                                                 suite
%%%----------------------------------------------------------------------------
%%% expects to be called by the main process ----------------------------------

suite(Nom) -> suite(Nom, []). 

suite(Nom, Flags) when is_list(Nom), is_list(Flags) -> 

	vecho(?V1, "~s~nStarting Suite ~s.", [line(), Nom]),
	
	put(flags, Flags),
	
	case lists:member(inverted, Flags) of
		true ->	strong_banner(Nom ++ ": " ++ ?INVERT, 0);
		_ -> nil
	end,
	
	Options = get(options),
		
	Suite = spawn(fun() -> suite("Suite " ++ Nom, Flags, Options, self()) end),
	
	glist_add(suitenames, Suite, "Suite " ++ Nom), 
	main ! { add, Suite, Nom },

	% register as default suite	
	force_register(suite, Suite),
	
	Suite.

%%%	                                                                     
%%%----------------------------------------------------------------------------
%%% Create Suite Process                                                 suite
%%%----------------------------------------------------------------------------
%%% non-exported: -------------------------------------------------------------
	
suite(Nom, Flags, Caller) -> suite(Nom, Flags, [], Caller).

suite(Nom, Flags, Options, Caller) ->


	put(options, Options), % replicates main process options

	suite_loop(Nom, Flags, Caller, nil, 0, 0, 0),
	exit(1000).

suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed) ->
	
	
	try 
		receive
	
			%% counters                                                       suite
			%% --------------------------------------------------------------------
			%% Note that these counter calls are processed way later than
			%% the check calls, because the latter are fielded, usually,
			%% in fast succession, thus are queued first in line, before
			%% even the first of them are executed, which then, in turn,
			%% generate the below counter calls
			
			passed -> 
				{ Signal, Pass, Fail } = inversion(Flags, passed),
				stats ! Signal,
				suite_loop(Nom, Flags, Caller, Sub, Passed + Pass, Failed + Fail, Crashed);
			failed -> 
				{ Signal, Pass, Fail } = inversion(Flags, failed),
				stats ! Signal,
				suite_loop(Nom, Flags, Caller, Sub, Passed + Pass, Failed + Fail, Crashed);
			crashed -> 
				stats ! crashed,
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed +1, Crashed +1);
				% doubled, see (***)

			%% check: true                                                    suite
			%% --------------------------------------------------------------------
	
			{ true, A } ->
				true(self(), A, true(msg)),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			{ true, A, Message } ->
				true(self(), A, Message ),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			%% check: not_true                                                suite
			%% --------------------------------------------------------------------
	
			{ not_true, A } ->
				not_true(self(), A, not_true(msg)),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			{ not_true, A, Message } ->
				not_true(self(), A, Message ),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			%% check: false                                                   suite
			%% --------------------------------------------------------------------
	
			{ false, A } ->
				false(self(), A, false(msg)),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			{ false, A, Message } ->
				false(self(), A, Message ),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			%% check: not_false                                               suite
			%% --------------------------------------------------------------------
	
			{ not_false, A } ->
				not_false(self(), A, not_false(msg)),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			{ not_false, A, Message } ->
				not_false(self(), A, Message ),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			%% check: pass                                                    suite
			%% --------------------------------------------------------------------
	
			{ pass, A } ->
				pass(self(), A, pass(msg)),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			{ pass, A, Message } ->
				pass(self(), A, Message ),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			%% check: fail                                                    suite
			%% --------------------------------------------------------------------
	
			{ fail, A } ->
				fail(self(), A, fail(msg)),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			{ fail, A, Message } ->
				fail(self(), A, Message ),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			%% check: throws                                                  suite
			%% --------------------------------------------------------------------
	
			{ throws, A } ->
				throws(self(), A, throws(msg)),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			{ throws, A, Message } ->
				throws(self(), A, Message ),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			%% check: error                                                  suite
			%% --------------------------------------------------------------------
	
			{ error, A } ->
				error(self(), A, error(msg)),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			{ error, A, Message } ->
				error(self(), A, Message ),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			%% check: exits                                                   suite
			%% --------------------------------------------------------------------
	
			{ exits, A } ->
				exits(self(), A, exits(msg)),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			{ exits, A, Message } ->
				exits(self(), A, Message ),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);		
			
			%% check: equal                                                   suite
			%% --------------------------------------------------------------------
	
			{ equal, A, B } ->
				equal(self(), A, B, equal(msg)),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			{ equal, A, B, Message } ->
				equal(self(), A, B, Message),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			%% check: exact                                                   suite
			%% --------------------------------------------------------------------
	
			{ exact, A, B } ->
				exact(self(), A, B, exact(msg)),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			{ exact, A, B, Message } ->
				exact(self(), A, B, Message),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			%% check: not_equal                                               suite
			%% --------------------------------------------------------------------
	
			{ not_equal, A, B } ->
				not_equal(self(), A, B, not_equal(msg)),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			{ not_equal, A, B, Message } ->
				not_equal(self(), A, B, Message),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			%% check: lesser                                                  suite
			%% --------------------------------------------------------------------
	
			{ lesser, A, B } ->
				lesser(self(), A, B, lesser(msg)),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			{ lesser, A, B, Message } ->
				lesser(self(), A, B, Message),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			%% check: bigger                                                  suite
			%% --------------------------------------------------------------------
	
			{ bigger, A, B } ->
				bigger(self(), A, B, bigger(msg)),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			{ bigger, A, B, Message } ->
				bigger(self(), A, B, Message),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
	
			%% screen                                                         suite
			%% --------------------------------------------------------------------
			print -> 
				if
					% suppress printing the default suite when unused
					(Nom == ?DEFAULT_SUITE_NAME) and (Passed+Failed+Crashed == 0) ->
						nil;
	
					true ->
						if 
							(Failed > 0) or (Crashed > 0) -> 
								echo("~s~n~s failed - Tests: ~w, Failed: ~w, Crashes: ~w~n~s",
									 [line(bad), Nom, Passed+Failed, Failed, Crashed, line(bad)]);
							true -> 
								echo("~s~n+ OK | ~s: all ~w tests passed.~n~s",
									 [line(good), Nom, Passed, line(good)])
						end
			
						% echo("~s~n~s ~s - Tests: ~w, Passed: ~w, Failed: ~w, Crashes: ~w~n~s",
						%	 [Line, Nom, Verdict, Passed+Failed, Passed, Failed, Crashed, Line])
				end,
	
				main ! { printed, self() },
	
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
				
			%% control                                                        suite
			%% --------------------------------------------------------------------
			stop -> 
				return;
	
			done -> 
				% self() ! print,
				main ! { done, self() },
				vecho(?V1, "~s~n~s done.", [line(), Nom]),
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed);
				
			Malformed -> 
				echo("###### ~s can't deal with ~w. ######~n###### ~s ######", [Nom, Malformed, ?USRERR]),
				self() ! crashed,
				suite_loop(Nom, Flags, Caller, Sub, Passed, Failed, Crashed)
				
			after 1000 ->
				exit("Suite " ++ Nom ++ " stalled. " ++ ?USRERR)
		end

    catch
    	error:Reason ->
    		stats ! crashed,
    		crashed(self(), Reason),	
			suite_loop(Nom, Flags, Caller, Sub, Passed, Failed +1, Crashed +1)
	end.

    
%%%	                                                                     
%%%----------------------------------------------------------------------------
%%% Inversion mapping                                                    suite
%%%----------------------------------------------------------------------------

inversion(Flags, Signal) when is_list(Flags), is_atom(Signal) ->

	case (Signal == passed) xor lists:member(inverted, Flags) of
		true -> { passed, 1, 0 };
		false -> {failed, 0, 1 }
	end.
	
%%%****************************************************************************
%%% STATS HOLDER
%%%****************************************************************************
%%%
%%%----------------------------------------------------------------------------
%%% Stats Process
%%%----------------------------------------------------------------------------

stats(Nom, Options) ->

	stats_loop(Nom, 0, 0, 0, Options).

stats_loop(Nom, Passed, Failed, Crashed, Options) ->
	
	% io:format("~nEntering stats_loop: ~s ~w ~w ~w ~w~n", [Nom, Passed, Failed, Crashed, Options]),
	
	receive
	
		% X -> io:format("Stats loop received ~w~n", [X]),
		%	stats_loop(Nom, Passed, Failed, Crashed);

		passed -> 
			stats_loop(Nom, Passed +1, Failed, Crashed, Options);

		failed -> 
			stats_loop(Nom, Passed, Failed +1, Crashed, Options);

		crashed -> 
			stats_loop(Nom, Passed, Failed +1, Crashed +1, Options);

		print ->
		  
		  	% colorizing
		  	case lists:member(colors,Options) of
		  		true -> 
		  			Red = "\e[1;31m", UnRed = "\e[0m",
		  			Green = "\e[1;32m", UnGreen = "\e[0m"; 
		  		false -> 
		  			Red = "", UnRed = "",
		  			Green = "", UnGreen = ""
		  	end,
		
			if
				Failed > 0 -> 
					{ Verdict, Line } = { "failed", line(bad) },
					Color = "", UnColor = "", Hi = Red, UnHi = UnRed;
				true -> 
					{ Verdict, Line } = { "passed", line(splendid) },
					Color = Green, UnColor = UnGreen, Hi = "", UnHi = ""
			end,

			echo("~s~n~s~s~s ~s~s~s - Tests: ~w, Passed: ~w, Failed: ~s~w~s, Crashes: ~w~n~s",
				 [Line, Color, Hi, Nom, Verdict, UnColor, UnHi, Passed+Failed, Passed, Hi, Failed, UnHi, Crashed, Line]),

			io:format("--o---------------------------------------------------------------------o--~n"),

			stats_loop(Nom, Passed, Failed, Crashed, Options)

    end.

%%%****************************************************************************
%%% GLOBAL LIST
%%%****************************************************************************
%%%
%%% Ack, ugh *** don't be me so hard! Oh ye Gods of FP, no, please ...
%%%
%%%----------------------------------------------------------------------------
%%% Create Global List
%%%----------------------------------------------------------------------------

glist(AtomName) ->

	GList = spawn(fun() -> glist_loop() end),
	force_register(AtomName, GList),
	GList.
	
%%%
%%%----------------------------------------------------------------------------
%%% Main Loop
%%%----------------------------------------------------------------------------

glist_loop() -> glist_loop([], nil, nil).

glist_loop(List, PrevCaller, Return) ->

	% call back to originally caller, to deliver result.
	if is_pid(PrevCaller) -> 
		vecho(?D4, "call ~w ~w", [PrevCaller, Return]), 
		PrevCaller ! Return; 
		true -> nil
	 end,

	receive
		{ add, Element, Caller } ->	glist_loop([Element | List], Caller, ok);
		{ drop, Key, Caller } -> glist_loop(lists:keydelete(Key,1,List), Caller, ok);
		{ get, Key, Caller } -> glist_loop(List, Caller, lists:keyfind(Key,1,List));
		E -> throw(E)
	end.

%%%
%%%----------------------------------------------------------------------------
%%% Create a Global List
%%%----------------------------------------------------------------------------


glist_add(GList, Key, Contents) ->
	
	vecho(?D4, "glist_add: GList ~w | Key ~w | Contents ~w", [GList, Key, Contents]),

	GList ! { add, { Key, Contents }, self() },
	receive ok -> ok after 3000 -> exit("Urgh. Programmed to death. glist_add() stalled. " ++ ?USRERR ++ " Try D4.") end.
	% this is forced sequentiality

%%%
%%%----------------------------------------------------------------------------
%%% Drop an element from the Global List, by Key
%%%----------------------------------------------------------------------------

	
glist_drop(GList, Key) ->
	
	GList ! { drop, Key, self() },
	receive ok -> ok after 3000 -> exit("Urgh. Programmed to death. glist_drop() stalled. " ++ ?USRERR ++ " Try D4.") end.
	% this is forced sequentiality

%%%
%%%----------------------------------------------------------------------------
%%% Get an element from the Global List, by Key
%%%----------------------------------------------------------------------------

%%%
%%% Key may be a Pid or an atom, to be interpreted as registered name.
	
glist_get(GList, Key) when is_atom(Key) -> glist_get(GList, whereis(Key), nil, 0);

glist_get(GList, Key) when is_pid(Key) -> glist_get(GList, Key, nil, 0).

glist_get(GList, Key, Default) when is_atom(Key) -> glist_get(GList, whereis(Key), Default, 0);

glist_get(GList, Key, Default) when is_pid(Key) -> glist_get(GList, Key, Default, 0).

%%% non-exported: -------------------------------------------------------------

glist_get(GList, Key, Default, Retry) -> 

	vecho(?D4, "glist_get: GList ~w | Key ~w | Retry ~w", [GList, Key, Retry]),

	case lists:member(GList, registered()) of
		true ->
			GList ! { get, Key, self() },
			receive 
				{ Key, Name } -> 
					Name;
				false ->
					% ok, short time out and retry for slow concurrent adds (?)
					case Retry of
						0 ->
							timer:sleep(100),
							glist_get(GList, Key, Default, Retry +1);
						_ ->
							Default 
					end
				after 3000 -> exit("Urgh. Programmed to death. glist_get() stalled. " ++ ?USRERR ++ " Try D4.") 
			end;
			% immediate receive forces sequentiality
		
		% not yet ready or never registered
		_ -> Default
	end.

%%%****************************************************************************
%%% UTILITY
%%%****************************************************************************
%%%
%%% Be especially wary not to take a cue from these.
%%%
%%%----------------------------------------------------------------------------
%%% payload - execute if function, else return identically.
%%%----------------------------------------------------------------------------
%%%
%%% Catch all exceptions and make sure they are condensed and 'thrown upward'.

payload(Suite, Message, Fun) when is_function(Fun) -> 

    try 
    	Fun()
	catch
    	throw:Term -> 
    		failed(Suite, Message, "throws exception (~w) but should pass ok.~n~p", [Term, erlang:get_stacktrace()]),
    		throw(Term);
    	exit:Reason -> 
    		failed(Suite, Message, "makes exit (~w) but should pass ok.~n~p", [Reason, erlang:get_stacktrace()]),
    		throw(Reason);
    	error:{ badarity, _ } -> 
    		failed(Suite, Message, "runs into error (~w) but should pass ok. Maybe you did not want ~p to be executed to evaluate the test? If you want to compare function references, you need to convert them, e.g to strings. Function references will otherwise be executed with arity 0, for your convenience (sic). Note that you might have other irregularities that are not detected, where the function is of arity 0. This here error was only detected because ~p is not of arity 0.", [badarity, Fun, Fun, erlang:get_stacktrace()]),
    		throw(badarity);
    	error:Reason -> 
    		failed(Suite, Message, "runs into error (~w) but should pass ok.~n~p", [Reason, erlang:get_stacktrace()]),
    		throw(Reason)
	end;

payload(_Suite, _Message, A) -> A.

%%%
%%%----------------------------------------------------------------------------
%%% force_unregister - unregister w/o complaint if missing.
%%%----------------------------------------------------------------------------
%%%
%%% For cleanup at closing down of the main test program thread.

force_unregister(Name) ->

	case whereis(Name) of
		undefined -> nil;
		_ -> unregister(Name)
	end.
%%%
%%%----------------------------------------------------------------------------
%%% force_register - register w/o complaint if already existing. 
%%%----------------------------------------------------------------------------
%%%
%%% Avoids argument errors for already registered processes during program
%%% restarts right after crashed runs. TODO: in stats and suits add time out.

force_register(Name, Process) ->

	force_unregister(Name),
	register(Name, Process).
%%%
%%%----------------------------------------------------------------------------
%%% if-function
%%%----------------------------------------------------------------------------

%%%
%%% Returns 2nd parameter if 1st is true, not "" and not 0. Else 3rd.

iff(Cond, A, _) when is_list(Cond) and not (Cond == "") -> A;

iff(Cond, A, _) when Cond == true -> A;

iff(Cond, A, _) when is_integer(Cond) and Cond /= 0 -> A;

iff(_, _, B) -> B.


%%%----------------------------------------------------------------------------
%%% true - vehicle to suppress warnings where definitions are used
%%%----------------------------------------------------------------------------

%%%
%%% To suppress warnings where definitions are used for clauses (**).

true() -> true. 

%%%****************************************************************************
%%% SCREEN UTILITY
%%%****************************************************************************
%%%

-define(WIDTH, 75).

width() -> ?WIDTH.
dashline() -> string:chars($*, width()).

%%%----------------------------------------------------------------------------
%%% Echo
%%%----------------------------------------------------------------------------

%%%
%%% A io:format() pretext that adds 'erlunit: ' left hand side.

echo(Message) -> echo(Message, []).

echo(Format, Para) -> 

	Format2 = re:replace(Format, "~n", "~n" ++ ?PROMPT, [global, {return,list}]),
	Format3 = ?PROMPT ++ Format2 ++ "~n",

	io:format(Format3, Para).

%%%
%%%----------------------------------------------------------------------------
%%% Verbose Echo
%%%----------------------------------------------------------------------------

%%%
%%% Echo only if the switch parameter is true. To be refactored.

vecho(Switch, Message) -> vecho(Switch, Message, []).

vecho(Switch, Format, Para) ->

	if Switch -> echo(Format, Para); true -> nil end.

%%%
%%%----------------------------------------------------------------------------
%%% Line
%%%----------------------------------------------------------------------------


line() -> line(default).

line(Type) ->

	PrintLines = ?LINES and true(), % see (**)
	if PrintLines ->
	
		if
			Type == splendid ->
				"oooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo";
%				"==================================================================";
			Type == good ->
				"------------------------------------------------------------------";
			Type == bad ->
				"##################################################################";
			Type == strong ->
				"******************************************************************";
		    true ->
				"..................................................................."
		end;
	
		true -> ""
	
	end.
%%%
%%%----------------------------------------------------------------------------
%%% ASCII Art Banner
%%%----------------------------------------------------------------------------
%%%
%%% This illegible ASCII art serves the purpose of quick orientation 
%%% when scrolling up and down the terminal window. And I like it, too.
%%%

ascii_banner() -> ascii_banner("").

ascii_banner(Message) ->
    io:format("------------------------------------------------------------------------o--~n"),
    io:format("---ooo-ooo--o--o--o-o--o-o-ooo------------------------------------------o--~n"),
    io:format("---Oo--OoO--O--O--O-Oo-O-O--O-------------------------------------------o--~n"),
    io:format("---Ooo-O--O-Ooo-OO--O-oO-O--O-------------------------------------------o--~n"),
    io:format("------------------------------------------------------------------------o--~n"),
    io:format("~s ~s ~s~n~s~s",[?LIBRARY, ?VERSION, ?COPYRIGHT, Message, iff(Message,"~n","")]),
    io:format("------------------------------------------------------------------------o--~n"),
    ok.
    
%%%----------------------------------------------------------------------------
%%% Strong Banner
%%%----------------------------------------------------------------------------
%%% Centered banner with text only:

% translate legacy name
banner(Text) -> strong_banner(Text).
banner(Text, EmptyRows) -> strong_banner(Text, EmptyRows).

strong_banner(Text) -> strong_banner(Text, 1).
strong_banner(Text, EmptyRows) ->

    io:format("~s~s~n~s~s~n~s~n",[string:chars(13, EmptyRows), dashline(), center_indent(Text, ?WIDTH), Text, dashline()]).

%%%----------------------------------------------------------------------------
%%% Centering a headline
%%%----------------------------------------------------------------------------

center(Text, Width) ->

	center_indent(Text, Width) ++ Text.

center_indent(Text, Width) ->
	string:chars(32, erlang:max(0, trunc((Width - length(Text)) / 2))).
    
%%%****************************************************************************
%%% DEBUG UTILITY
%%%****************************************************************************
%%%
%%%----------------------------------------------------------------------------
%%% alive - dump if a registered process is still registered
%%%----------------------------------------------------------------------------


alive(Name) ->
	Is = lists:member(Name, registered()),
	if Is -> Say = "is alive", Paint = "++++++"; true -> Say = "is gone", Paint = "XXXXX" end,
	io:format("~s~s ~s ~s ~s~n", [?INDENT, Paint, Name, Say, Paint]).
	

%%%                                   %0> 
%%%                                   (^)
%%% Easter egg                        ###                           End of art
