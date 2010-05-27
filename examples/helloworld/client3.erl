% This client demonstrates access via record definition.
%
% This sample works with the catalog and server of voltdb/examples/helloworld.
% Start the server that you built with said Java example, go into this helloworld
% folder and then run 
% $> erlc ../../erlvolt.erl; erlc client3.erl; erl -s client3 run -s init stop -noshell
%
% - or -
%
% run ./client3

-module(client3).
-import(erlvolt).
-include("../../erlvolt.hrl").

-export([run/0]).

%-------------------------------------------------------------------------%
%         Differences to client.erl and client1.erl                       %
%-------------------------------------------------------------------------%
%         #1 - define row record: ...                                     %
%-------------------------------------------------------------------------%

-record(helloquery, { hello, world }).

run() ->

    try

        %%%
        %%% Connect to the database
        %%%
        
        Connection = erlvolt:createConnection("localhost", "program", "password"),
        
    
        %%%
        %%% Load data into the database
        %%%
    
        erlvolt:callProcedure(Connection, "Insert", ["Hello",  "World", "English"]),
        erlvolt:callProcedure(Connection, "Insert", ["Bonjour","Monde", "French"]),
        erlvolt:callProcedure(Connection, "Insert", ["Hola",   "Mundo", "Spanish"]),
        erlvolt:callProcedure(Connection, "Insert", ["Hej",    "Verden","Danish"]),
        erlvolt:callProcedure(Connection, "Insert", ["Ciao",   "Mondo", "Italian"]),
        
        %%%
        %%% Retrieve the message.
        %%%
     
        Response = erlvolt:callProcedure(Connection, "Select", ["Spanish"]),
        
          case Response of

              [] -> 
                  io:format("I can't say Hello in that language."),
                  exit(bad_lang);
    
              { voltresponse, _, [ Table | _ ] } ->
              
	    %-------------------------------------------------------------------------%
	    %         #2 - Fetch the row as a record: ...                             %
	    %-------------------------------------------------------------------------%
                  Record = erlvolt:fetchRecord(Table, helloquery, 1),
	    %-------------------------------------------------------------------------%
		%         Instead of:                                                     %
		%                                                                         %
        %         Row = erlvolt:fetchRow(Table, 1),                               %
	    %-------------------------------------------------------------------------%

                  io:format("~n~n~s, ~s!~n", 
	    
	    %-------------------------------------------------------------------------%
	    %         #3 - use the fast & readable record notation for colum access:  %
	    %-------------------------------------------------------------------------%
	    
                          [ erlvolt:getString(Record, #helloquery.hello), 
                            erlvolt:getString(Record, #helloquery.world) ]);

	    %-------------------------------------------------------------------------%
		%                 Instead of slow:                                        %
		%                                                                         %
		%                 [ erlvolt:getString(Row, Table, "HELLO"),               %
		%                   erlvolt:getString(Row, Table, "WORLD") ]);            %
		%                                                                         %
		%                 or error prone:                                         %
		%                                                                         %
		%                 [ erlvolt:getString(Row, 1),                            %
		%                   erlvolt:getString(Row, 2) ]);                         %
	    %-------------------------------------------------------------------------%
              
              Other -> 
                  io:format("Can't grok ~w.", [Other]),
                  exit(bad_result)
        end,

    	io:format("~n(This was client3)~n") 

    catch

        What:Why -> 
            io:format("~n ~w ~w ~n ~p", [What, Why, erlang:get_stacktrace()]),
            exit({What, Why})
    end.