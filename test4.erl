%%%-------------------------------------------------------------------------%%%
%%% File        : test4.erl                                                 %%%
%%% Description : Unit Tests #4 for Erlang-VoltDB client API erlvolt.erl    %%% 
%%% Copyright   : VoltDB, LLC - http://www.voltdb.com                       %%%
%%% Production  : Eonblast Corporation - http://www.eonblast.com            %%%
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%
%%% Licence     : GPLv3                                                     %%%
%%% Created     : 14 May 2010                                               %%%
%%% Changed     : 14 May 2010                                               %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         
%%%   @doc                                                                  
%%%                                                                         
%%%   This is a unit test program to test the Erlvolt module ervolt.erl.    
%%%                                                                         
%%%   It tests decoding of VoltTables.    
%%%                                                                         
%%%   == Usage                                                                
%%%   ```                                                                
%%%   $ erlc erlvolt.erl; erlc erlunit/erlunit.erl; erlc test4.erl;         
%%%   $ erl -s test4 run -s init stop -noshell                              
%%%   '''                                                                         
%%%   == Prerequisite                                                          
%%%                                                            
%%%   Erlunit in subdirectory erlunit.                                      
%%%   Get it from: [http://github.com/Eonblast/Erlunit/tarball/master]        
%%%                                                                            
%%%   @end
%%%
%%%----------------------------------------------------------------------------


-module(test4).
-include("erlvolt.hrl").

-import(erlunit). % http://github.com/Eonblast/Erlunit/tarball/master
-include("erlunit/erlunit.hrl").

-export([run/0]).
-import(unicode).

-define(BILLION, 1000000000). % for clarity and to prevent typos.
-define(MILLION, 1000000   ).

%%%----------------------------------------------------------------------------
%%%
%%%    run/1
%%%
%%%    @doc This is the only function in this package, it runs a unit test on
%%%    erlvolt.erl to test decoding of VoltTable wite protocol.
%%%
%%%----------------------------------------------------------------------------

run() ->

	erlunit:start([nopasses]),
	
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%                                                                         %
	%                               VOLTTABLES                                % 
	%                                                                         %
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	%*************************************************************************%
    %                                                                         %
	%                              Simple Cases                               % 
    %                                                                         %
	%*************************************************************************%
	%%%------------------------------------------------------------------------
	%%% 1: Doc Sample
	%%%------------------------------------------------------------------------

	VoltTable_Bin_1 =
	<<
	0,0,0,32, 			% Total Length     
	0,0,0,12,       	% Meta Data Length 
	0,              	% Status Code
	0,1,				% Column Count
	?VOLT_BIGINT,		% Column 1 Type
	0,0,0,4,			% Column 1 Name Length
	84,101,115,116,		% Column 1 Name Value
	0,0,0,1,			% Row Count
	0,0,0,8,			% Row 1 Length
	0,0,0,0,0,0,0,5		% Row 1 Column 1 Value
	>>,

	VoltTable_Erl_1 = { volttable, [<<"Test">>], [?VOLT_BIGINT], [ { voltrow, [5] } ] },
	
	%%%   --- pg. 5,6, VoltDB Client Wire Protocol Version 0, 05/05/10 ---        

	%%%------------------------------------------------------------------------
	%%% 2: Multiple Columns
	%%%------------------------------------------------------------------------

	VoltTable_Bin_2 =
	<<
	0,0,0,66, 			% Total Length     
	0,0,0,30,       	% Meta Data Length 
	0,              	% Status Code
	0,3,				% Column Count
	?VOLT_BIGINT,		% Column 1 Type
	?VOLT_BIGINT,		% Column 2 Type
	?VOLT_BIGINT,		% Column 3 Type
	0,0,0,4,			% Column 1 Name Length
	84,101,115,116,		% Column 1 Name Value
	0,0,0,4,			% Column 2 Name Length
	84,101,115,116,		% Column 2 Name Value
	0,0,0,4,			% Column 3 Name Length
	84,101,115,116,		% Column 3 Name Value
	0,0,0,1,			% Row Count
	0,0,0,24,			% Row 1 Length
	0,0,0,0,0,0,0,5,	% Row 1 Column 1 Value
	0,0,0,0,0,0,0,5,	% Row 1 Column 2 Value
	0,0,0,0,0,0,0,5		% Row 1 Column 3 Value
	>>,

	VoltTable_Erl_2 = { volttable, 
						[<<"Test">>,<<"Test">>,<<"Test">>], 
						[?VOLT_BIGINT,?VOLT_BIGINT,?VOLT_BIGINT], 
						[ { voltrow, [5,5,5] } ] },

	%%%------------------------------------------------------------------------
	%%% 3: Multiple Rows
	%%%------------------------------------------------------------------------

	VoltTable_Bin_3 =
	<<
	0,0,0,56, 			% Total Length     
	0,0,0,12,       	% Meta Data Length 
	0,              	% Status Code
	0,1,				% Column Count
	?VOLT_BIGINT,		% Column 1 Type
	0,0,0,4,			% Column 1 Name Length
	84,101,115,116,		% Column 1 Name Value
	0,0,0,3,			% Row Count
	0,0,0,8,			% Row 1 Length
	0,0,0,0,0,0,0,5,	% Row 1 Column 1 Value
	0,0,0,8,			% Row 2 Length
	0,0,0,0,0,0,0,5,	% Row 2 Column 1 Value
	0,0,0,8,			% Row 3 Length
	0,0,0,0,0,0,0,5 	% Row 3 Column 1 Value
	>>,

	VoltTable_Erl_3 = { volttable, 
						[<<"Test">>], 
						[?VOLT_BIGINT], 
						[ { voltrow, [5] },
						  { voltrow, [5] },
						  { voltrow, [5] } ] },
	
	%%%------------------------------------------------------------------------
	%%% 4: Multiple Rows and Columns
	%%%------------------------------------------------------------------------

	VoltTable_Bin_4 =
	<<
	0,0,0,122, 			% Total Length     
	0,0,0,30,       	% Meta Data Length 
	0,              	% Status Code
	0,3,				% Column Count
	?VOLT_BIGINT,		% Column 1 Type
	?VOLT_BIGINT,		% Column 2 Type
	?VOLT_BIGINT,		% Column 3 Type
	0,0,0,4,			% Column 1 Name Length
	84,101,115,116,		% Column 1 Name Value
	0,0,0,4,			% Column 2 Name Length
	84,101,115,116,		% Column 2 Name Value
	0,0,0,4,			% Column 3 Name Length
	84,101,115,116,		% Column 3 Name Value
	0,0,0,3,			% Row Count
	0,0,0,24,			% Row 1 Length
	0,0,0,0,0,0,0,5,	% Row 1 Column 1 Value
	0,0,0,0,0,0,0,5,	% Row 1 Column 2 Value
	0,0,0,0,0,0,0,5,	% Row 1 Column 3 Value
	0,0,0,24,			% Row 2 Length
	0,0,0,0,0,0,0,5,	% Row 2 Column 1 Value
	0,0,0,0,0,0,0,5,	% Row 2 Column 2 Value
	0,0,0,0,0,0,0,5,	% Row 2 Column 3 Value
	0,0,0,24,			% Row 3 Length
	0,0,0,0,0,0,0,5, 	% Row 3 Column 1 Value
	0,0,0,0,0,0,0,5, 	% Row 3 Column 2 Value
	0,0,0,0,0,0,0,5 	% Row 3 Column 3 Value
	>>,

	VoltTable_Erl_4 = { volttable, 
						[<<"Test">>,<<"Test">>,<<"Test">>], 
						[?VOLT_BIGINT,?VOLT_BIGINT,?VOLT_BIGINT], 
						[ { voltrow, [5,5,5] },
						  { voltrow, [5,5,5] },
						  { voltrow, [5,5,5] } ] },
						  
	%%%------------------------------------------------------------------------
	%%% 5: All Column Types
	%%%------------------------------------------------------------------------

	VoltTable_Bin_5 =
	<<
	0,0,0,139, 			% Total Length     
	0,0,0,75,       	% Meta Data Length 
	0,              	% Status Code

	0,8,				% Column Count

	?VOLT_TINYINT,		% Column 1 Type
	?VOLT_SMALLINT,		% Column 2 Type
	?VOLT_INTINT,		% Column 3 Type
	?VOLT_BIGINT,		% Column 4 Type
	?VOLT_FLOAT,		% Column 5 Type
	?VOLT_DECIMAL,		% Column 6 Type
	?VOLT_TIMESTAMP,	% Column 7 Type
	?VOLT_STRING,		% Column 8 Type

	0,0,0,4,			% Column 1 Name Length
	84,101,115,116,		% Column 1 Name Value
	0,0,0,4,			% Column 2 Name Length
	84,101,115,116,		% Column 2 Name Value
	0,0,0,4,			% Column 3 Name Length
	84,101,115,116,		% Column 3 Name Value
	0,0,0,4,			% Column 4 Name Length
	84,101,115,116,		% Column 4 Name Value
	0,0,0,4,			% Column 5 Name Length
	84,101,115,116,		% Column 5 Name Value
	0,0,0,4,			% Column 6 Name Length
	84,101,115,116,		% Column 6 Name Value
	0,0,0,4,			% Column 7 Name Length
	84,101,115,116,		% Column 7 Name Value
	0,0,0,4,			% Column 8 Name Length
	84,101,115,116,		% Column 8 Name Value

	0,0,0,1,			% Row Count

	0,0,0,52,			% Row 1 Length
	1,					% Row 1 Column 1 Value
	0,2,				% Row 1 Column 2 Value
	0,0,0,3,			% Row 1 Column 3 Value
	0,0,0,0,0,0,0,4,	% Row 1 Column 4 Value
	64,160,0,0,			% Row 1 Column 5 Value
	
	0,0,0,0,0,0,0,0,0,0,5,116,251,222,96,0,	% Row 1 Column 6 Value (6.0)
	0,0,1,19,159,128,213,120,				% Row 1 Column 7 Value (2007-7-7 7:07:07)
	0,0,0,5,69,105,103,104,116				% Row 1 Column 8 Value ("Eight")
	>>,

	VoltTable_Erl_5 = { volttable, 
						[<<"Test">>,<<"Test">>,<<"Test">>,<<"Test">>,
						 <<"Test">>,<<"Test">>,<<"Test">>,<<"Test">>], 
						[?VOLT_TINYINT,?VOLT_SMALLINT,?VOLT_INTINT,?VOLT_BIGINT,
						 ?VOLT_FLOAT,?VOLT_DECIMAL,?VOLT_TIMESTAMP,?VOLT_STRING], 
						[ { voltrow, [1,2,3,4,5.0,6,{1183,792027,0},<<"Eight">>] } ] },						  

	%%%------------------------------------------------------------------------
	%%% 6: All Column Types, Multiple Rows
	%%%------------------------------------------------------------------------

	VoltTable_Bin_6 =
	<<
	0,0,2,131, 			% Total Length     
	0,0,0,75,       	% Meta Data Length 
	0,              	% Status Code

	0,8,				% Column Count

	?VOLT_TINYINT,		% Column 1 Type
	?VOLT_SMALLINT,		% Column 2 Type
	?VOLT_INTINT,		% Column 3 Type
	?VOLT_BIGINT,		% Column 4 Type
	?VOLT_FLOAT,		% Column 5 Type
	?VOLT_DECIMAL,		% Column 6 Type
	?VOLT_TIMESTAMP,	% Column 7 Type
	?VOLT_STRING,		% Column 8 Type

	0,0,0,4,			% Column 1 Name Length
	84,101,115,116,		% Column 1 Name Value
	0,0,0,4,			% Column 2 Name Length
	84,101,115,116,		% Column 2 Name Value
	0,0,0,4,			% Column 3 Name Length
	84,101,115,116,		% Column 3 Name Value
	0,0,0,4,			% Column 4 Name Length
	84,101,115,116,		% Column 4 Name Value
	0,0,0,4,			% Column 5 Name Length
	84,101,115,116,		% Column 5 Name Value
	0,0,0,4,			% Column 6 Name Length
	84,101,115,116,		% Column 6 Name Value
	0,0,0,4,			% Column 7 Name Length
	84,101,115,116,		% Column 7 Name Value
	0,0,0,4,			% Column 8 Name Length
	84,101,115,116,		% Column 8 Name Value

	0,0,0,10,			% Row Count

	% Row 1

	0,0,0,52,			% Row 1 Length
	1,					% Row 1 Column 1 Value
	0,2,				% Row 1 Column 2 Value
	0,0,0,3,			% Row 1 Column 3 Value
	0,0,0,0,0,0,0,4,	% Row 1 Column 4 Value
	64,160,0,0,			% Row 1 Column 5 Value
	
	0,0,0,0,0,0,0,0,0,0,5,116,251,222,96,0,	% Row 1 Column 6 Value (6.0)
	0,0,1,19,159,128,213,120,				% Row 1 Column 7 Value (2007-7-7 7:07:07)
	0,0,0,5,69,105,103,104,116,				% Row 1 Column 8 Value ("Eight")

	% Row 2
	
	0,0,0,52,			% Row 2 Length
	1,					% Row 2 Column 1 Value
	0,2,				% Row 2 Column 2 Value
	0,0,0,3,			% Row 2 Column 3 Value
	0,0,0,0,0,0,0,4,	% Row 2 Column 4 Value
	64,160,0,0,			% Row 2 Column 5 Value
	
	0,0,0,0,0,0,0,0,0,0,5,116,251,222,96,0,	% Row 2 Column 6 Value (6.0)
	0,0,1,19,159,128,213,120,				% Row 2 Column 7 Value (2007-7-7 7:07:07)
	0,0,0,5,69,105,103,104,116,				% Row 2 Column 8 Value ("Eight")

	% Row 3
	
	0,0,0,52,			% Row 3 Length
	1,					% Row 3 Column 1 Value
	0,2,				% Row 3 Column 2 Value
	0,0,0,3,			% Row 3 Column 3 Value
	0,0,0,0,0,0,0,4,	% Row 3 Column 4 Value
	64,160,0,0,			% Row 3 Column 5 Value
	
	0,0,0,0,0,0,0,0,0,0,5,116,251,222,96,0,	% Row 3 Column 6 Value (6.0)
	0,0,1,19,159,128,213,120,				% Row 3 Column 7 Value (2007-7-7 7:07:07)
	0,0,0,5,69,105,103,104,116,				% Row 3 Column 8 Value ("Eight")

	% Row 4
	
	0,0,0,52,			% Row 4 Length
	1,					% Row 4 Column 1 Value
	0,2,				% Row 4 Column 2 Value
	0,0,0,3,			% Row 4 Column 3 Value
	0,0,0,0,0,0,0,4,	% Row 4 Column 4 Value
	64,160,0,0,			% Row 4 Column 5 Value
	
	0,0,0,0,0,0,0,0,0,0,5,116,251,222,96,0,	% Row 4 Column 6 Value (6.0)
	0,0,1,19,159,128,213,120,				% Row 4 Column 7 Value (2007-7-7 7:07:07)
	0,0,0,5,69,105,103,104,116,				% Row 4 Column 8 Value ("Eight")

	% Row 5
	
	0,0,0,52,			% Row 5 Length
	1,					% Row 5 Column 1 Value
	0,2,				% Row 5 Column 2 Value
	0,0,0,3,			% Row 5 Column 3 Value
	0,0,0,0,0,0,0,4,	% Row 5 Column 4 Value
	64,160,0,0,			% Row 5 Column 5 Value
	
	0,0,0,0,0,0,0,0,0,0,5,116,251,222,96,0,	% Row 5 Column 6 Value (6.0)
	0,0,1,19,159,128,213,120,				% Row 5 Column 7 Value (2007-7-7 7:07:07)
	0,0,0,5,69,105,103,104,116,				% Row 5 Column 8 Value ("Eight")

	% Row 6
	
	0,0,0,52,			% Row 6 Length
	1,					% Row 6 Column 1 Value
	0,2,				% Row 6 Column 2 Value
	0,0,0,3,			% Row 6 Column 3 Value
	0,0,0,0,0,0,0,4,	% Row 6 Column 4 Value
	64,160,0,0,			% Row 6 Column 5 Value
	
	0,0,0,0,0,0,0,0,0,0,5,116,251,222,96,0,	% Row 6 Column 6 Value (6.0)
	0,0,1,19,159,128,213,120,				% Row 6 Column 7 Value (2007-7-7 7:07:07)
	0,0,0,5,69,105,103,104,116,				% Row 6 Column 8 Value ("Eight")

	% Row 7
	
	0,0,0,52,			% Row 7 Length
	1,					% Row 7 Column 1 Value
	0,2,				% Row 7 Column 2 Value
	0,0,0,3,			% Row 7 Column 3 Value
	0,0,0,0,0,0,0,4,	% Row 7 Column 4 Value
	64,160,0,0,			% Row 7 Column 5 Value
	
	0,0,0,0,0,0,0,0,0,0,5,116,251,222,96,0,	% Row 7 Column 6 Value (6.0)
	0,0,1,19,159,128,213,120,				% Row 7 Column 7 Value (2007-7-7 7:07:07)
	0,0,0,5,69,105,103,104,116,				% Row 7 Column 8 Value ("Eight")

	% Row 8
	
	0,0,0,52,			% Row 8 Length
	1,					% Row 8 Column 1 Value
	0,2,				% Row 8 Column 2 Value
	0,0,0,3,			% Row 8 Column 3 Value
	0,0,0,0,0,0,0,4,	% Row 8 Column 4 Value
	64,160,0,0,			% Row 8 Column 5 Value
	
	0,0,0,0,0,0,0,0,0,0,5,116,251,222,96,0,	% Row 8 Column 6 Value (6.0)
	0,0,1,19,159,128,213,120,				% Row 8 Column 7 Value (2007-7-7 7:07:07)
	0,0,0,5,69,105,103,104,116,				% Row 8 Column 8 Value ("Eight")

	% Row 9
	
	0,0,0,52,			% Row 9 Length
	1,					% Row 9 Column 1 Value
	0,2,				% Row 9 Column 2 Value
	0,0,0,3,			% Row 9 Column 3 Value
	0,0,0,0,0,0,0,4,	% Row 9 Column 4 Value
	64,160,0,0,			% Row 9 Column 5 Value
	
	0,0,0,0,0,0,0,0,0,0,5,116,251,222,96,0,	% Row 9 Column 6 Value (6.0)
	0,0,1,19,159,128,213,120,				% Row 9 Column 7 Value (2007-7-7 7:07:07)
	0,0,0,5,69,105,103,104,116,				% Row 9 Column 8 Value ("Eight")

	% Row 10
	
	0,0,0,52,			% Row 10 Length
	1,					% Row 10 Column 1 Value
	0,2,				% Row 10 Column 2 Value
	0,0,0,3,			% Row 10 Column 3 Value
	0,0,0,0,0,0,0,4,	% Row 10 Column 4 Value
	64,160,0,0,			% Row 10 Column 5 Value
	
	0,0,0,0,0,0,0,0,0,0,5,116,251,222,96,0,	% Row 10 Column 6 Value (6.0)
	0,0,1,19,159,128,213,120,				% Row 10 Column 7 Value (2007-7-7 7:07:07)
	0,0,0,5,69,105,103,104,116				% Row 10 Column 8 Value ("Eight")


	>>,

	VoltTable_Erl_6 = { volttable, 
						[<<"Test">>,<<"Test">>,<<"Test">>,<<"Test">>,
						 <<"Test">>,<<"Test">>,<<"Test">>,<<"Test">>], 
						[?VOLT_TINYINT,?VOLT_SMALLINT,?VOLT_INTINT,?VOLT_BIGINT,
						 ?VOLT_FLOAT,?VOLT_DECIMAL,?VOLT_TIMESTAMP,?VOLT_STRING], 
						[ { voltrow, [1,2,3,4,5.0,6,{1183,792027,0},<<"Eight">>] },						  
						  { voltrow, [1,2,3,4,5.0,6,{1183,792027,0},<<"Eight">>] },						  
						  { voltrow, [1,2,3,4,5.0,6,{1183,792027,0},<<"Eight">>] },						  
						  { voltrow, [1,2,3,4,5.0,6,{1183,792027,0},<<"Eight">>] },						  
						  { voltrow, [1,2,3,4,5.0,6,{1183,792027,0},<<"Eight">>] },						  
						  { voltrow, [1,2,3,4,5.0,6,{1183,792027,0},<<"Eight">>] },						  
						  { voltrow, [1,2,3,4,5.0,6,{1183,792027,0},<<"Eight">>] },						  
						  { voltrow, [1,2,3,4,5.0,6,{1183,792027,0},<<"Eight">>] },						  
						  { voltrow, [1,2,3,4,5.0,6,{1183,792027,0},<<"Eight">>] },						  
						  { voltrow, [1,2,3,4,5.0,6,{1183,792027,0},<<"Eight">>] } ] },						  


	%%%------------------------------------------------------------------------
	%%% 7: Long Column
	%%%------------------------------------------------------------------------

	MegaString1 = <<1:10000000>>,

	VoltTable_Bin_7 =
	<<
	0,19,18,236,		% Total Length     
	0,0,0,12,       	% Meta Data Length 
	0,              	% Status Code
	0,1,				% Column Count
	?VOLT_STRING,		% Column 1 Type
	0,0,0,4,			% Column 1 Name Length
	84,101,115,116,		% Column 1 Name Value
	0,0,0,1,			% Row Count
	0,19,18,212,		% Row 1 Length
	0,19,18,208,		% Row 1 Column 1 Length
	MegaString1/binary	% Row 1 Column 1 Value
	>>,

	VoltTable_Erl_7 = { volttable, [<<"Test">>], [?VOLT_STRING], [ { voltrow, [MegaString1] } ] },
	
	%%%------------------------------------------------------------------------
	%%% 8: Multiple Long Columns
	%%%------------------------------------------------------------------------

	MillString1 = <<1:1000000>>,

	VoltTable_Bin_8 =
	<<
	0,5,185,14,			% Total Length     
	0,0,0,30,       	% Meta Data Length 
	0,              	% Status Code
	0,3,				% Column Count
	?VOLT_STRING,		% Column 1 Type
	?VOLT_STRING,		% Column 2 Type
	?VOLT_STRING,		% Column 3 Type
	0,0,0,4,			% Column 1 Name Length
	84,101,115,116,		% Column 1 Name Value
	0,0,0,4,			% Column 2 Name Length
	84,101,115,116,		% Column 2 Name Value
	0,0,0,4,			% Column 3 Name Length
	84,101,115,116,		% Column 3 Name Value
	0,0,0,1,			% Row Count
	0,5,184,228,		% Row 1 Length
	0,1,232,72,			% Row 1 Column 1 Length
	MillString1/binary,	% Row 1 Column 1 Value
	0,1,232,72,			% Row 1 Column 1 Length
	MillString1/binary,	% Row 1 Column 1 Value
	0,1,232,72,			% Row 1 Column 1 Length
	MillString1/binary	% Row 1 Column 1 Value
	>>,

	VoltTable_Erl_8 = { volttable, 
						[<<"Test">>,<<"Test">>,<<"Test">>], 
						[?VOLT_STRING,?VOLT_STRING,?VOLT_STRING], 
						[ { voltrow, [MillString1,MillString1,MillString1] } ] },      


	%%%------------------------------------------------------------------------
	%%% 9: Fancy Characters
	%%%------------------------------------------------------------------------

	FancyString1 = <<"& Hallöle!">>,      % size 11 not 10!
	FancyString2 = <<"Nirt∂ælwøld">>,     % size 15!
	FancyString3 = <<"ÄÖÜäöüßåÅøØæÆœŒ">>, % size 30!

	VoltTable_Bin_9 =
	<<
	0,0,0,110, 			% Total Length     
	0,0,0,30,       	% Meta Data Length 
	0,              	% Status Code
	0,3,				% Column Count
	?VOLT_STRING,		% Column 1 Type
	?VOLT_STRING,		% Column 2 Type
	?VOLT_STRING,		% Column 3 Type
	0,0,0,4,			% Column 1 Name Length
	84,101,115,116,		% Column 1 Name Value
	0,0,0,4,			% Column 2 Name Length
	84,101,115,116,		% Column 2 Name Value
	0,0,0,4,			% Column 3 Name Length
	84,101,115,116,		% Column 3 Name Value
	0,0,0,1,			% Row Count
	0,0,0,68,			% Row 1 Length
	0,0,0,11,			% Row 1 Column 1 Size
	FancyString1/binary,% Row 1 Column 1 Value
	0,0,0,15,			% Row 1 Column 1 Size
	FancyString2/binary,% Row 1 Column 1 Value
	0,0,0,30,			% Row 1 Column 1 Size
	FancyString3/binary	% Row 1 Column 1 Value
	>>,

	VoltTable_Erl_9 = { volttable, 
						[<<"Test">>,<<"Test">>,<<"Test">>], 
						[?VOLT_STRING,?VOLT_STRING,?VOLT_STRING], 
						[ { voltrow, [FancyString1,FancyString2,FancyString3] } ] },
	


	%*************************************************************************%
    %                                                                         %
	%                             Mounting Tests                              % 
    %                                                                         %
	%*************************************************************************%


	%*************************************************************************%
	%                                Encoding                                 % 
	%*************************************************************************%
	
	%%%------------------------------------------------------------------------
	%%% Subfunctions
	%%%------------------------------------------------------------------------

	erlunit:suite("VoltTables/Encoder Subfunctions"),

	%%%-field-conversion-------------------------------------------------------

	?ERLUNIT_EQUAL(erlvolt:volt_fields([?VOLT_STRING], ["Test"]), [<<4:32,"Test">>]),
	?ERLUNIT_EQUAL(erlvolt:volt_fields([?VOLT_STRING, ?VOLT_STRING], ["Test", "Test"]), [<<4:32,"Test">>,<<4:32,"Test">>]),

	%%%-row-conversion---------------------------------------------------------

	?ERLUNIT_EQUAL(erlvolt:volt_rows([?VOLT_STRING], [ { voltrow, ["Test"] } ]),
									 [<<8:32, 4:32, "Test">>]),
	?ERLUNIT_EQUAL(erlvolt:volt_rows([?VOLT_STRING, ?VOLT_STRING], [ { voltrow, ["Te1", "Test2"] } ]),
									 [<<16:32, 3:32,"Te1", 5:32,"Test2">>]),
	?ERLUNIT_EQUAL(erlvolt:volt_rows([?VOLT_STRING], [ { voltrow, ["Test"] }, { voltrow, ["Test"] } ]),
									 [<<8:32, 4:32,"Test">>,<<8:32, 4:32,"Test">>]),
	?ERLUNIT_EQUAL(erlvolt:volt_rows([?VOLT_STRING, ?VOLT_STRING], 
									 [ { voltrow, ["Test1", "Test10"] }, { voltrow, ["Test11", "Test110"] } ]),
									 [<<19:32, 5:32,"Test1", 6:32,"Test10">>, <<21:32, 6:32,"Test11", 7:32,"Test110">>]),
	
	%%%------------------------------------------------------------------------
	%%% Complete Tables
	%%%------------------------------------------------------------------------
	
	erlunit:suite("VoltTables/Simple Encodes"),

	?ERLUNIT_EQUAL(erlvolt:volt_table(VoltTable_Erl_1), VoltTable_Bin_1),
	?ERLUNIT_EQUAL(erlvolt:volt_table(VoltTable_Erl_2), VoltTable_Bin_2),
	?ERLUNIT_EQUAL(erlvolt:volt_table(VoltTable_Erl_3), VoltTable_Bin_3),
	?ERLUNIT_EQUAL(erlvolt:volt_table(VoltTable_Erl_4), VoltTable_Bin_4),
	?ERLUNIT_EQUAL(erlvolt:volt_table(VoltTable_Erl_5), VoltTable_Bin_5),
	?ERLUNIT_EQUAL(erlvolt:volt_table(VoltTable_Erl_6), VoltTable_Bin_6),
	?ERLUNIT_EQUAL(erlvolt:volt_table(VoltTable_Erl_7), VoltTable_Bin_7),
	?ERLUNIT_EQUAL(erlvolt:volt_table(VoltTable_Erl_8), VoltTable_Bin_8),
	?ERLUNIT_EQUAL(erlvolt:volt_table(VoltTable_Erl_9), VoltTable_Bin_9),

	
	%*************************************************************************%
	%                                Decoding                                 % 
	%*************************************************************************%
	
	erlunit:suite("VoltTables/Simple Decodes"),

	?ERLUNIT_EQUAL(erlvolt:erl_table(VoltTable_Bin_1), VoltTable_Erl_1),
	?ERLUNIT_EQUAL(erlvolt:erl_table(VoltTable_Bin_2), VoltTable_Erl_2),
	?ERLUNIT_EQUAL(erlvolt:erl_table(VoltTable_Bin_3), VoltTable_Erl_3),
	?ERLUNIT_EQUAL(erlvolt:erl_table(VoltTable_Bin_4), VoltTable_Erl_4),
	?ERLUNIT_EQUAL(erlvolt:erl_table(VoltTable_Bin_5), VoltTable_Erl_5),
	?ERLUNIT_EQUAL(erlvolt:erl_table(VoltTable_Bin_6), VoltTable_Erl_6),
	?ERLUNIT_EQUAL(erlvolt:erl_table(VoltTable_Bin_7), VoltTable_Erl_7),
	?ERLUNIT_EQUAL(erlvolt:erl_table(VoltTable_Bin_8), VoltTable_Erl_8),
	?ERLUNIT_EQUAL(erlvolt:erl_table(VoltTable_Bin_9), VoltTable_Erl_9),

	%*************************************************************************%
	%                                Two-Way                                  % 
	%*************************************************************************%

	erlunit:suite("VoltTables/Two-Way"),

	?ERLUNIT_EQUAL(erlvolt:erl_table(erlvolt:volt_table(VoltTable_Erl_1)), VoltTable_Erl_1),
	?ERLUNIT_EQUAL(erlvolt:erl_table(erlvolt:volt_table(VoltTable_Erl_2)), VoltTable_Erl_2),
	?ERLUNIT_EQUAL(erlvolt:erl_table(erlvolt:volt_table(VoltTable_Erl_3)), VoltTable_Erl_3),
	?ERLUNIT_EQUAL(erlvolt:erl_table(erlvolt:volt_table(VoltTable_Erl_4)), VoltTable_Erl_4),
	?ERLUNIT_EQUAL(erlvolt:erl_table(erlvolt:volt_table(VoltTable_Erl_5)), VoltTable_Erl_5),
	?ERLUNIT_EQUAL(erlvolt:erl_table(erlvolt:volt_table(VoltTable_Erl_6)), VoltTable_Erl_6),
	?ERLUNIT_EQUAL(erlvolt:erl_table(erlvolt:volt_table(VoltTable_Erl_7)), VoltTable_Erl_7),
	?ERLUNIT_EQUAL(erlvolt:erl_table(erlvolt:volt_table(VoltTable_Erl_8)), VoltTable_Erl_8),
	?ERLUNIT_EQUAL(erlvolt:erl_table(erlvolt:volt_table(VoltTable_Erl_9)), VoltTable_Erl_9),

	?ERLUNIT_EQUAL(erlvolt:volt_table(erlvolt:erl_table(VoltTable_Bin_1)), VoltTable_Bin_1),
	?ERLUNIT_EQUAL(erlvolt:volt_table(erlvolt:erl_table(VoltTable_Bin_2)), VoltTable_Bin_2),
	?ERLUNIT_EQUAL(erlvolt:volt_table(erlvolt:erl_table(VoltTable_Bin_3)), VoltTable_Bin_3),
	?ERLUNIT_EQUAL(erlvolt:volt_table(erlvolt:erl_table(VoltTable_Bin_4)), VoltTable_Bin_4),
	?ERLUNIT_EQUAL(erlvolt:volt_table(erlvolt:erl_table(VoltTable_Bin_5)), VoltTable_Bin_5),
	?ERLUNIT_EQUAL(erlvolt:volt_table(erlvolt:erl_table(VoltTable_Bin_6)), VoltTable_Bin_6),
	?ERLUNIT_EQUAL(erlvolt:volt_table(erlvolt:erl_table(VoltTable_Bin_7)), VoltTable_Bin_7),
	?ERLUNIT_EQUAL(erlvolt:volt_table(erlvolt:erl_table(VoltTable_Bin_8)), VoltTable_Bin_8),
	?ERLUNIT_EQUAL(erlvolt:volt_table(erlvolt:erl_table(VoltTable_Bin_9)), VoltTable_Bin_9),


	%%%------------------------------------------------------------------------
	%%% Mounting of Tests Done. Execute.
	%%%------------------------------------------------------------------------

	erlunit:execute().
	

