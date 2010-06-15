% This client demonstrate access via column numbers.
%
% This sample works with the catalog and server of voltdb/examples/helloworld.
% Start the server that you built with said Java example, go into this helloworld
% folder and then run 
% $> erlc ../../erlvolt.erl; erlc client2.erl; erl -s client2 run -s init stop -noshell
%
% - or -
%
% run ./client2

-module(client2).
-import(erlvolt).
-include("../../erlvolt.hrl").

-export([run/0]).

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
              
                  Row = erlvolt:fetchRow(Table, 1),
                  io:format("~n~n~s, ~s!~n", 
	    
	    %-------------------------------------------------------------------------%
	    %                 Use this faster notation of column numbers:             %
	    %-------------------------------------------------------------------------%
	    
                          [ erlvolt:getString(Row, 1), 
                            erlvolt:getString(Row, 2) ]);

	    %-------------------------------------------------------------------------%
		%                 Instead of:                                             %
		%                                                                         %
		%                 [ erlvolt:getString(Row, Table, "HELLO"),               %
		%                   erlvolt:getString(Row, Table, "WORLD") ]);            %
	    %-------------------------------------------------------------------------%
              
              Other -> 
                  io:format("Can't grok ~w.", [Other]),
                  exit(bad_result)
        end,


		erlvolt:close(Connection),
	
    	io:format("~n(This was client2)~n") 

    catch

        What:Why -> 
            io:format("~n ~w ~w ~n ~p", [What, Why, erlang:get_stacktrace()]),
            exit({What, Why})
    end.