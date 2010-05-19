-module(client).
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
    
        results = erlvolt:callProcedure(Connection, "Insert", ["Hello",  "World", "English"]),
        results = erlvolt:callProcedure(Connection, "Insert", ["Bonjour","Monde", "French"]),
        results = erlvolt:callProcedure(Connection, "Insert", ["Hola",   "Mundo", "Spanish"]),
        results = erlvolt:callProcedure(Connection, "Insert", ["Hej",    "Verden","Danish"]),
        results = erlvolt:callProcedure(Connection, "Insert", ["Ciao",   "Mondo", "Italian"]),
        
        %%%
        %%% Retrieve the message.
        %%%
     
        results = erlvolt:callProcedure("Select", ["Spanish"]),
        
          case results of
              [] -> 
                  io:format("I can't say Hello in that language."),
                  exit(bad_lang);
    
              { ok, ResultTable } ->
              
                  Row = erlvolt:fetchRow(ResultTable, 0),
                  io:format("~s, ~s!\n", 
                          [ erlvolt:getString(Row, "hello"), 
                            erlvolt:getString(Row, "world") ]);
              
              Other -> 
                  io:format("Can't grok ~w.", [Other]),
                  exit(bad_result)
        end

    catch

        What:Why -> 
            io:format("~w ~w ~n ~p", [What, Why, erlang:get_stacktrace()]),
            exit({What, Why})
    end.