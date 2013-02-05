%%%-------------------------------------------------------------------------%%%
%%% File        : examples/voter.erl                                        %%%
%%% Version     : 0.3/beta                                                  %%%
%%% Description : Erlang VoltDB driver VoltDB staple 'Voter' example        %%%
%%% Copyright   : VoltDB, LLC - http://www.voltdb.com                       %%%
%%% Production  : Eonblast Corporation - http://www.eonblast.com            %%%
%%% Author      : H. Diedrich <hd2012@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 30 Jan 2013                                               %%%
%%% Changed     : 02 Feb 2013                                               %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%   This driver is being contributed to VoltDB by Eonblast Corporation.   %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%    Erlvolt 0.3/beta    - Erlang VoltDB client API.                      %%%
%%%                                                                         %%%
%%%    This file is part of VoltDB.                                         %%%
%%%    Copyright (C) 2008-2013 VoltDB, LLC http://www.voltdb.com            %%%
%%%    Author H. Diedrich <hd2012@eonblast.com> http://www.eonblast.com     %%%
%%%                                                                         %%%
%%% Permission is hereby granted, free of charge,  to any person obtaining  %%%
%%% a copy  of this  software  and  associated  documentation  files  (the  %%%
%%% "Software"),  to deal in the  Software without restriction,  including  %%%
%%% without limitation the rights to use,  copy,  modify,  merge, publish,  %%%
%%% distribute,  sublicense,  and/or sell copies of the  Software,  and to  %%%
%%% permit persons to whom the Software is furnished to do so,  subject to  %%%
%%% the following conditions:                                               %%%
%%%                                                                         %%%
%%% The  above  copyright  notice  and  this  permission  notice  shall be  %%%
%%% included in all copies or substantial portions of the Software.         %%%
%%%                                                                         %%%
%%% THE  SOFTWARE  IS  PROVIDED  "AS IS",  WITHOUT  WARRANTY  OF ANY KIND,  %%%
%%% EXPRESS OR IMPLIED,  INCLUDING  BUT  NOT LIMITED  TO THE WARRANTIES OF  %%%
%%% MERCHANTABILITY, FITNESS  FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. %%%
%%% IN NO EVENT SHALL  THE AUTHORS  BE LIABLE  FOR  ANY CLAIM,  DAMAGES OR  %%%
%%% OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT  OR OTHERWISE,  %%%
%%% ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE  OR THE USE OR  %%%
%%% OTHER DEALINGS IN THE SOFTWARE.                                         %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%% USAGE                                                                   %%%
%%%                                                                         %%%
%%% You can run this sample using the 'Voter' tutorial-server discussed     %%%
%%% that is present in every VoltDB distribution. For an explanation of     %%%
%%% the 'TV voter' scenario, see comments below.                            %%%
%%%                                                                         %%%
%%% To try it, start that server from your voltdb installation with:        %%%
%%%                                                                         %%%
%%%     $ cd voltdb/examples/voter                                          %%%
%%%     $ ./run.sh                                                          %%%
%%%                                                                         %%%
%%% Then, in a different terminal, from the driver root:                    %%%
%%%                                                                         %%%
%%%     $ make voter                                                        %%%
%%% or                                                                      %%%
%%%     $ make                                                              %%%
%%%     $ cd examples                                                       %%%
%%%     $ erlc -I ../include -o ../ebin +debug_info voter.erl               %%%
%%%     $ erl -pa ../ebin -s voter run -s init stop -noshell                %%%
%%%                                                                         %%%
%%% You will see this response, listing parameters and voting results:      %%%
%%%                                                                         %%%
%%%     Voter Sample                                                        %%%
%%%     ----------------------------------------------------------------    %%%
%%%              Hosts: localhost:21212                                     %%%
%%%           username: "user"                                              %%%
%%%           password: "password"                                          %%%
%%%           database: "database"                                          %%%
%%%         queue_size: 100000                                              %%%
%%%              slots: 100                                                 %%%
%%%              nagle: true                                                %%%
%%%        send_buffer: 10000                                               %%%
%%%     receive_buffer: 100000                                              %%%
%%%       send_timeout: 10000                                               %%%
%%%                                                                         %%%
%%%     calls ...                                                           %%%
%%%     cool down ...                                                       %%%
%%%     done ...                                                            %%%
%%%     ----------------------------------------------------------------    %%%
%%%     votes: 10000 (6 contestants)                                        %%%
%%%     .......Jessie Eichman:    1692                                      %%%
%%%     .......Jessie Alloway:    1678                                      %%%
%%%     ........Alana Bregman:    1673                                      %%%
%%%     ........Edwina Burnam:    1671                                      %%%
%%%     .........Kelly Clauss:    1645                                      %%%
%%%     ......Tabatha Gehling:    1641                                      %%%
%%%     ----------------------------------------------------------------    %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%       For a barebones 'hello world' see examples/hello.erl.             %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%% See README.md or .html for instructions,  examples/ for more examples.  %%%
%%% See doc/BENCHMARKS.md or .html for a description of driver benchmarks.  %%%
%%%                                                                         %%%
%%% For getting started with VoltDB,see: voltdb/doc/GettingStarted.pdf or   %%%
%%% online:  http://voltdb.com/downloads/documentation/GettingStarted.pdf.  %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%% VOTER - DESCRIPTION                                                     %%%
%%%                                                                         %%%
%%% The Voter example application simulates a phone based election process. %%%
%%% Voters (based on phone numbers generated randomly by the client         %%%
%%% application) are allowed a limited number of votes.                     %%%
%%%                                                                         %%%
%%% The main goal of the voter application is to demonstrate the            %%%
%%% performance possibilities of VoltDB:                                    %%%
%%%                                                                         %%%
%%% - Stored procedures are invoked asynchronously. - The client            %%%
%%% can record and report thoughput and latency, when compiled with         %%%
%%% the driver-built in profiler (`make clean profile`).                    %%%
%%%                                                                         %%%
%%% Usually you will run this sample on a single node. However, it can      %%%
%%% easily be reconfigured to run any combination of clients and servers.   %%%
%%% The servers support multiple clients at one time - Single node or       %%%
%%% multi-node clusters are supported                                       %%%
%%%                                                                         %%%
%%% To test the server with a different configuration, simply edit          %%%
%%% run.sh, listing one server node as the lead node when building the      %%%
%%% catalog and using a comma-separated list of the server addresses as     %%%
%%% an argument to the client. See the comments in the build script for     %%%
%%% details.                                                                %%%
%%%                                                                         %%%
%%% The client starts "fire-hosing" the VoltDB server using pre-set         %%%
%%% parameters. You can experiment with different loads to better           %%%
%%% understand the best tuning to get the most out of your specific         %%%
%%% VoltDB deployment.                                                      %%%
%%%                                                                         %%%
%%% Rate-limiting your clients (or adding cluster nodes) is essential to    %%%
%%% preventing "fire-hosing" your server (cluster) and will ensure you      %%%
%%% get proper application responsiveness (latency) while maximizing        %%%
%%% througput (TPS) for your hardware configuration.                        %%%
%%%                                                                         %%%
%%% The "Voter" application is specifically designed for benchmarking to    %%%
%%% give you a good feel for the type of performance VoltDB is capable      %%%
%%% of on your hardware.                                                    %%%
%%%                                                                         %%%
%%% For more on benchmarking and tips on application tuning, visit the      %%%
%%% VoltDB blog: http://voltdb.com/search/node/benchmark and                %%%
%%% - http://voltdb.com/search/node/tuning                                  %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%

-module(voter).
-export([run/0, run/1,
	 prep_voter/1, vote/1, voter_return_text/1]).
-import(erlvolt).
-include("erlvolt.hrl").

%% Voter return values
-define(VOTE_SUCCESSFUL, 0).
-define(ERR_INVALID_CONTESTANT, 1).
-define(ERR_VOTER_OVER_VOTE_LIMIT, 2).

-define(SEP, "-----------------------------------------------------------------------~n").
%%%----------------------------------------------------------------------------
%%% Voter Sample Start Function
%%%----------------------------------------------------------------------------
%% @spec run() -> any().

run() ->

	run(['10000']).

%% @spec run([ACallCount]) -> ok.
%%	 ACallCount = atom()

run([ACallCount]) ->

	%% Cast from command line atoms and verify

	io:format("~n~nVoter Sample~n"),
	io:format(?SEP),

    crypto:start(),
    application:start(erlvolt),

	CallCount = list_to_integer(atom_to_list(ACallCount)),
    Hosts = [{"localhost", 21212}],

	%% Note: these are pretty much the default values, shown
	%% here explicitly to give an idea of the set of options.
    Opts = [{username, "user"},
			{password, "password"},
			{database, "database"},
			{queue_size, 100000},
			{slots, 100},
			{nagle, true},
			{send_buffer, 10000},
			{receive_buffer, 100000},
			{send_timeout, 10000}],

	POpts = [ io_lib:format("~14.s: ~p~n", [Key, Value]) || {Key, Value} <- Opts ],

	io:format("         Hosts: ~s~n", [[D++":"++integer_to_list(P)++" "||{D,P}<-Hosts]]),
	io:format("~s~n", [POpts]),

    try
        %%%
        %%% Connect to the database
        %%%

        erlvolt:add_pool(mypool, Hosts, Opts),

        %%%
        %%% Load sample data into the database
        %%%

		prep_voter(mypool),

        %%%
        %%% Measure
        %%%

        io:format("calls ... ~n"),
		[ spawn(?MODULE, vote, [mypool]) || _ <- lists:seq(1,CallCount) ],

        %%%
        %%% Wait for last responses
        %%%

        io:format("cool down ... ~n"),
		erlvolt:drain_pool(mypool),
        io:format("done ... ~n"),

        %%%
        %%% Show results
        %%%

	  	C = query_contestants(mypool),
  		V = query_votes(mypool),
  		io:format(?SEP),
  		io:format("votes: ~p (~p contestants)~n", [V, C]),

	  	Result = erlvolt:call_procedure(mypool, "Results", []),
	  	dump(Result),
  		io:format(?SEP),
	    ?ERLVOLT_PROFILER_COUNT_SUCCESS(),

        %%%
        %%% Shut Down
        %%%

		erlvolt:close_pool(mypool)

    catch
		throw:{ connection_failed, _}=_Why ->
			io:format("~n------------------------------------------------------------------------------------~n"),
            io:format("Failed to open server connection.~nIs the VoltDB server running and accessible?"),
			io:format("~n------------------------------------------------------------------------------------~n");

        What:Why ->
            io:format("~n ~w ~w ~n ~p", [What, Why, erlang:get_stacktrace()]),
            exit({What, Why})
    end.

%% @spec prep_voter(mypool) -> ok
prep_voter(mypool) ->

	%% Seed pseduo random generator
	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),

	%% Initialize the database
	Result = erlvolt:call_procedure(mypool, "Initialize", [6,voter_candidates()]),
	try
		{result, {voltresponse, {0,<<0,0,_,0,0,0,0,0>>,1,<<>>,128,<<>>,<<>>,_}, [{volttable,[<<>>],[6],[{voltrow,[6]}]}]}} = Result
    catch
    	error:{badmatch,_} ->
    		case erlvolt:get_statusstring(Result) of
    			<<"Procedure Initialize was not found">> ->
    				io:format("### Calling stored procedure 'Initialize' failed. Right database running? ###~n"),
    				exit(cant_initialize);
    			Status ->
    				io:format("### Calling stored procedure 'Initialize' failed: ~p~n",[Status])
    		end
    end,
    ?ERLVOLT_PROFILER_COUNT_SUCCESS(0),
	ok.

%% Names of candidates.
%% @spec voter_candidates() -> string()
voter_candidates() ->

	"Edwina Burnam,Tabatha Gehling,Kelly Clauss," ++
	"Jessie Alloway,Alana Bregman,Jessie Eichman,Allie Rogalski,Nita Coster," ++
	"Kurt Walser,Ericka Dieter,Loraine NygrenTania Mattioli".

%% Get a pseudo random candidate number
%% @spec voter_get_candidate() -> 1..6
voter_get_candidate() ->

	{A1,A2,A3} = now(),
	random:seed(A1, A2, A3),
	random:uniform(6).

%% List of Area Codes.
%% @spec voter_area_codes() -> list()
voter_area_codes() ->

	[907, 205, 256, 334, 251, 870, 501, 479, 480, 602, 623, 928,
	520, 341, 764, 628, 831, 925, 909, 562, 661, 510, 650, 949, 760, 415, 951, 209,
	669, 408, 559, 626, 442, 530, 916, 627, 714, 707, 310, 323, 213, 424, 747, 818,
	858, 935, 619, 805, 369, 720, 303, 970, 719, 860, 203, 959, 475, 202, 302, 689,
	407, 239, 850, 727, 321, 754, 954, 927, 352, 863, 386, 904, 561, 772, 786, 305,
	941, 813, 478, 770, 470, 404, 762, 706, 678, 912, 229, 808, 515, 319, 563, 641,
	712, 208, 217, 872, 312, 773, 464, 708, 224, 847, 779, 815, 618, 309, 331, 630,
	317, 765, 574, 260, 219, 812, 913, 785, 316, 620, 606, 859, 502, 270, 504, 985,
	225, 318, 337, 774, 508, 339, 781, 857, 617, 978, 351, 413, 443, 410, 301, 240,
	207, 517, 810, 278, 679, 313, 586, 947, 248, 734, 269, 989, 906, 616, 231, 612,
	320, 651, 763, 952, 218, 507, 636, 660, 975, 816, 573, 314, 557, 417, 769, 601,
	662, 228, 406, 336, 252, 984, 919, 980, 910, 828, 704, 701, 402, 308, 603, 908,
	848, 732, 551, 201, 862, 973, 609, 856, 575, 957, 505, 775, 702, 315, 518, 646,
	347, 212, 718, 516, 917, 845, 631, 716, 585, 607, 914, 216, 330, 234, 567, 419,
	440, 380, 740, 614, 283, 513, 937, 918, 580, 405, 503, 541, 971, 814, 717, 570,
	878, 835, 484, 610, 267, 215, 724, 412, 401, 843, 864, 803, 605, 423, 865, 931,
	615, 901, 731, 254, 325, 713, 940, 817, 430, 903, 806, 737, 512, 361, 210, 979,
	936, 409, 972, 469, 214, 682, 832, 281, 830, 956, 432, 915, 435, 801, 385, 434,
	804, 757, 703, 571, 276, 236, 540, 802, 509, 360, 564, 206, 425, 253, 715, 920,
	262, 414, 608, 304, 307].


%% Get pseudo randomly one of the area codes
%% @spec voter_get_phone() -> 2050000000..9079999999.
voter_get_phone() ->

	lists:nth(random:uniform(305), voter_area_codes()) * 10000000 + random:uniform(9999999).

%% @spec call_voter(Link) -> ok.
%% 	Link = mypool::any() |  {_ConnList::list(), _Round:integer()}
vote(Link) ->

	Candidate = voter_get_candidate(),
	Phone = voter_get_phone(),

	Result = (catch erlvolt:call_procedure(Link, "Vote", [Phone, Candidate, 1000000000])),

	case Result of

		%% empty result (not expected)
		{result, { voltresponse, _Response, []}} = Result ->
			?ERLVOLT_PROFILER_COUNT_FAILURE(),
			io:format("RECEIVED EMPTY RESPONSE"),
			io:format("Voter client received unexpected result: ~p~n", [Result]),
			exit(voter_unexpected_empty_response);

		%% good result
		{result, { voltresponse, _, [ _Table | _ ] }} = _Result ->

			% ...
			_Time = erlvolt_wire:roundtrip(_Result),
			?ERLVOLT_PROFILER_COUNT_SUCCESS(_Time);

		%% drop for full queue
		queue_full ->
			io:format("DROPPING FOR FULL QUEUE");

		%% drop for dull queue
		queue_timeout ->
			io:format("DROPPING FOR QUEUE WAIT TIMEOUT");

		%% unexpected other result
		Other ->
			?ERLVOLT_PROFILER_COUNT_FAILURE(),
			io:format("RECEIVED UNEXPECTED RESPONSE"),
			io:format("Voter client received unexpected result: ~p.", [Other]),
			exit(voter_bad_result)
	end.

voter_return_text(?VOTE_SUCCESSFUL) -> "vote successful";
voter_return_text(?ERR_INVALID_CONTESTANT) -> "invalid contestant";
voter_return_text(?ERR_VOTER_OVER_VOTE_LIMIT) -> "voter over vote limit".

query_contestants(mypool) ->

    Result = erlvolt:call_procedure(mypool, "@AdHoc", ["select COUNT(*) as C from contestants"]),
    {result, { voltresponse, _, [ Table | _ ] }} = Result,
    Row = erlvolt:get_row(Table, 1),
	Count = erlvolt:get_integer(Row, Table, "C"),
    ?ERLVOLT_PROFILER_COUNT_SUCCESS(),
    Count.

query_votes(mypool) ->

    Result = erlvolt:call_procedure(mypool, "@AdHoc", ["select COUNT(*) as C from votes"]),
    {result, { voltresponse, _, [ Table | _ ] }} = Result,
    Row = erlvolt:get_row(Table, 1),
	Count = erlvolt:get_integer(Row, Table, "C"),
    ?ERLVOLT_PROFILER_COUNT_SUCCESS(),
    Count.

dump( {result, {voltresponse,_,Tables}}) ->

    [ dump(T) || T <- Tables ];

dump({ volttable, _, _, Rows }) ->

    [ io:format("~21...s: ~7w~n",[Name, Votes]) || {voltrow, [Name, _, Votes]} <- Rows ].
