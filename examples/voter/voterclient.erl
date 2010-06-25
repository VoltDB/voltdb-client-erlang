% This sample works with the catalog and server of voltdb/examples/helloworld.
% Start the server that you built with said Java example and then run 
% $> erlc erlvolt.erl; erlc hello.erl; erl -s hello run -s init stop -noshell

-module(voterclient).
-import(erlvolt).
-include("../../erlvolt.hrl").

-export([run/0]).


%%%
%%% Initialize the database with the requested number of contestants.
%%% If the database was already initialized return the correct number of contestants.
%%%
initialize_database(Connection,NumContestants) ->
    ContestantNames = ["Edwina Burnam","Tabatha Gehling","Kelly Clauss","Jessie Alloway","Alana Bregman","Jessie Eichman","Allie Rogalski","Nita Coster","Kurt Walser","Ericka Dieter","Loraine Nygren","Tania Mattioli"],

    Response = erlvolt:callProcedure(Connection, "Initialize", [{?VOLT_INTINT, NumContestants}, {?VOLT_ARRAY, ContestantNames}]),
    
    case Response of
        [] -> 
            io:format("Procedure Initialize did not return a proper value.~n"),
            exit(bad_lang);
        { voltresponse, _, [ Table | _ ] } ->
            Row = erlvolt:fetchRow(Table, 1),
            io:format("Running for ~s contestant(s)~n",  [ erlvolt:getString(Row, Table, "numContestants") ]),
            erlvolt:getField(erlvolt:fetchRow(Table, 1), 1);
        Other -> 
            io:format("Can't grok ~w.", [Other]),
            exit(bad_result)
    end.


%%%
%%% Cast the "Votes"
%%%
cast_votes(Connection,CurrentVote,TotalVotes,NumContestants,MaxVotesPerPhoneNumber) when CurrentVote =< TotalVotes ->
    _Progress = 
        if ((CurrentVote rem 1000) == 0) ->
            io:format("Casting vote number ~B~n",[CurrentVote]),
            0;
        true ->
            1
        end,

    PhoneNumber = random:uniform(9999999999),
    if ((CurrentVote rem 100) == 0) ->
        % introduce a potentially bad contestant number every 100 votes
        ContestantNumber = (random:uniform(NumContestants) + 1) * 2;
    true ->
        ContestantNumber = ((random:uniform(NumContestants) * random:uniform(NumContestants)) rem NumContestants) + 1
    end,

    Response = erlvolt:callProcedure(Connection, "Vote", [{?VOLT_BIGINT, PhoneNumber}, {?VOLT_TINYINT, ContestantNumber}, {?VOLT_BIGINT,MaxVotesPerPhoneNumber}]),

    case Response of
        [] -> 
            io:format("Procedure Vote did not return a proper value.~n"),
            exit(bad_lang);
        { voltresponse, _, [ Table | _ ] } ->
            Row = erlvolt:fetchRow(Table, 1),
            ReturnValue = erlvolt:getField(Row, 1),
            case ReturnValue of
                0 -> 
                    % io:format("Successful vote.~n"),
                    _Dummy = 0;
                1 -> 
                    % io:format("Invalid contestant number.~n"),
                    _Dummy = 1;
                2 -> 
                    % io:format("Voter over limit.~n"),
                    _Dummy = 2;
                _ -> 
                    io:format("Invalid return value from procedure Vote.~n"),
                    exit(bad_result)
            end;
        Other -> 
            io:format("Can't grok ~w.", [Other]),
            exit(bad_result)
    end,

    cast_votes(Connection,CurrentVote+1,TotalVotes,NumContestants,MaxVotesPerPhoneNumber);


cast_votes(_,_,_,_,_) ->
    io:format("All Votes Cast~n~n",[]).


%%%
%%% Check the results of all votes to date and display the winner
%%%
get_results(Connection, TotalVotes, TimeStartMilliseconds, TimeEndMilliseconds) ->
    Response = erlvolt:callProcedure(Connection, "Results", []),
    
    case Response of
        [] -> 
            io:format("Procedure Results did not return a proper value.~n"),
            exit(bad_lang);
        { voltresponse, _, [ Table | _ ] } ->
            Rows = erlvolt:fetchRowsAsList(Table),
            lists:map(fun (Row) -> io:format("Contestant ~s received ~B votes~n",  [ erlvolt:getField(Row, 1), erlvolt:getField(Row, 2) ]) end, Rows),
            VoteWinner = winner(Rows), 
            io:format("~nContestant ~s was the winner with ~B vote(s)~n",[lists:nth(1,VoteWinner),lists:nth(2,VoteWinner)]);
        Other -> 
            io:format("Can't grok ~w.", [Other]),
            exit(bad_result)
    end,

    % show how many transaction per second were performed
    DurationSeconds = (TimeEndMilliseconds - TimeStartMilliseconds) / 1000,
    TransactionsPerSecond = (TotalVotes / DurationSeconds),
    io:format("~nRan for ~10.2f seconds at ~10.2f transactions per second~n",[DurationSeconds,TransactionsPerSecond]).


%%%
%%% Determine which contestant was the winner.
%%%
winner([{voltrow,[TestName,TestVotes]}|T]) ->
    winner([TestName,TestVotes],T).

winner([Name,Votes], []) ->
    [Name,Votes];

winner([_,Votes], [{voltrow,[TestName,TestVotes]}|L]) when TestVotes > Votes ->
    winner([TestName,TestVotes],L);

winner([Name,Votes],[_|L]) ->
    winner([Name,Votes],L).



run() ->
    try
        MaxVotesPerPhoneNumber = 10,
        NumContestants = 12,
        TotalVotes = 5000,
        ServerName = "10.10.20.138",
        UserName = "program",
        UserPassword = "password",

        %%%
        %%% Initialize the random number generator
        %%%

        {A1,A2,A3} = now(),
        random:seed(A1, A2, A3),

        %%%
        %%% Connect to the database
        %%%
        
        Connection = erlvolt:createConnection(ServerName, UserName, UserPassword),
        io:format("~n"),
        
        %%%
        %%% Initialize the vote, or find out the actual number of contestants
        %%%

        ActualNumContestants = initialize_database(Connection,NumContestants),

        %%%
        %%% Grab the starting time
        %%%
        {TimeStartMegaseconds, TimeStartSeconds, TimeStartMicroseconds} = now(),
        TimeStartMilliseconds = erlang:round((((TimeStartMegaseconds*1000000)+TimeStartSeconds)*1000)+(TimeStartMicroseconds/1000)),
        
        %%%
        %%% Cast the votes
        %%%

        cast_votes(Connection,1,TotalVotes,ActualNumContestants,MaxVotesPerPhoneNumber),

        %%%
        %%% Grab the ending time
        %%%
        {TimeEndMegaseconds, TimeEndSeconds, TimeEndMicroseconds} = now(),
        TimeEndMilliseconds = erlang:round((((TimeEndMegaseconds*1000000)+TimeEndSeconds)*1000)+(TimeEndMicroseconds/1000)),

        %%%
        %%% Find out who won
        %%%

        get_results(Connection, TotalVotes, TimeStartMilliseconds, TimeEndMilliseconds),
        
        io:format("~n")

%tc - add back when no longer throwing error
%		erlvolt:close(Connection)

    catch
		throw:{ open_failed, _, _}=Why ->
			io:format("~n-------------------------------------------------------------------------------~n"),
            io:format("Failed to open server connection.~nIs the VoltDB server running and accessible?"),
			io:format("~n-------------------------------------------------------------------------------~n"),
            io:format("Error details: ~n ~w ~w ~n ~p", [throw, Why, erlang:get_stacktrace()]),
            exit({throw, Why});
		
        What:Why -> 
            io:format("~n ~w ~w ~n ~p", [What, Why, erlang:get_stacktrace()]),
            exit({What, Why})
    end.
