-module(hello).
-import(erlvolt).
-include("erlvolt.hrl").

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
    
              { voltresponse, _, [ Table | Tables ] } ->
              
                  Row = erlvolt:fetchRow(Table, 1),
                  io:format("~s, ~s!\n", 
                          [ erlvolt:getString(Row, 1), 
                            erlvolt:getString(Row, Table, <<"WORLD">>) ]);
              
              Other -> 
                  io:format("Can't grok ~w.", [Other]),
                  exit(bad_result)
        end

    catch

        What:Why -> 
            io:format("~w ~w ~n ~p", [What, Why, erlang:get_stacktrace()]),
            exit({What, Why})
    end.