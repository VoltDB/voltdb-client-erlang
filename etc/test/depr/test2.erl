%%%-------------------------------------------------------------------------%%%
%%% File        : test2.erl                                                 %%%
%%% Description : Unit Tests #2 for Erlang-VoltDB client API erlvolt.erl    %%% 
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%  
%%% License     : GPLv3                                                     %%%  
%%% Created     : 11 May 2010                                               %%%
%%% Changed     : 07 Jul 2012                                               %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%   This is a unit test program to test the Erlvolt module ervolt.erl.    %%%
%%%                                                                         %%%
%%%   Usage:                                                                %%%
%%%   ------                                                                %%%
%%%   $ erlc erlvolt.erl; erlc erlunit/erlunit.erl; erlc test2.erl;         %%%
%%%   $ erl -s test2 run -s init stop -noshell                              %%%
%%%                                                                         %%%
%%%   Prerequisite:                                                         %%%
%%%   -------------                                                         %%%
%%%   Erlunit in subdirectory erlunit.                                      %%%
%%%   Get it from: http://github.com/Eonblast/Erlunit/tarball/master        %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%    Erlvolt 0.3.01/alpha - an Erlang-VoltDB client API.                  %%%
%%%                                                                         %%%
%%%    This file is part of VoltDB.                                         %%%
%%%    Copyright (C) 2008-2010 VoltDB, LLC http://www.voltdb.com            %%%
%%%    Author H. Diedrich <hd2010@eonblast.com> http://www.eonblast.com     %%%
%%%                                                                         %%%
%%%    VoltDB is free software:  you can redistribute it  and/or  modify    %%%
%%%    it under the terms of the GNU General Public License as published    %%%
%%%    by the Free Software Foundation, either version 3 of the License,    %%%
%%%    or (at your option) any later version.                               %%%
%%%                                                                         %%%
%%%    VoltDB  is distributed  in the hope  that it will be useful,  but    %%%
%%%    WITHOUT  ANY  WARRANTY;  without  even  the  implied  warranty of    %%%
%%%    MERCHANTABILITY  or  FITNESS  FOR A  PARTICULAR PURPOSE.  See the    %%%
%%%    GNU General Public License for more details.                         %%%
%%%                                                                         %%%
%%%    You should have received a copy of the GNU General Public License    %%%
%%%    along with  VoltDB.  If not,  see <http://www.gnu.org/licenses/>.    %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%


-module(test2).
-include("../include/erlvolt.hrl").
-import(erlvolt).

-import(erlunit). % http://github.com/Eonblast/Erlunit/tarball/master
-include("../etc/erlunit/erlunit.hrl").

-export([run/0]).
-import(unicode).

-define(BILLION,  1000000000). % for clarity and to prevent typos.
-define(MILLION,  1000000   ).

run() ->

	erlunit:start([colors]),
	erlunit:banner("Client-Server Connection"),

%	?ERLUNIT_PASS(emulated_echo()),

	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
	%                                                                         %
	%                      SERVER-CLIENT COMMUNICATION                        % 
	%                                                                         %
	%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

	erlunit:suite("Server-Client Com"),
	
	%%%------------------------------------------------------------------------
	%%% Header (Protocol number)
	%%%------------------------------------------------------------------------

    %%%-encode-----------------------------------------------------------------

	?ERLUNIT_EQUAL(erlvolt:volt_header(), ?VOLT_PROTOCOL_VERSION_BINARY),

    %%%-decode-----------------------------------------------------------------

	?ERLUNIT_EQUAL(erlvolt:erl_header(?VOLT_PROTOCOL_VERSION_BINARY), { ?VOLT_PROTOCOL_VERSION, 1 }),
	?ERLUNIT_EQUAL(erlvolt:erl_header(<<?VOLT_PROTOCOL_VERSION,1>>), { ?VOLT_PROTOCOL_VERSION, 2 }),
	?ERLUNIT_EQUAL(erlvolt:erl_header(<<?VOLT_PROTOCOL_VERSION,1,2,3,4,5,6,7,8,9>>), { ?VOLT_PROTOCOL_VERSION, 10 }),
	?ERLUNIT_EQUAL(erlvolt:erl_header(<<?VOLT_PROTOCOL_VERSION,1:8000>>), { ?VOLT_PROTOCOL_VERSION, 1001 }),

	?ERLUNIT_FAIL (erlvolt:erl_header(<<(?VOLT_PROTOCOL_VERSION + 1)>>)),
	?ERLUNIT_FAIL (erlvolt:erl_header(<<(?VOLT_PROTOCOL_VERSION + 1),1>>)),
	?ERLUNIT_FAIL (erlvolt:erl_header(<<(?VOLT_PROTOCOL_VERSION + 1),1,2,3,4,5,7,8,9>>)),
	?ERLUNIT_FAIL (erlvolt:erl_header(<<(?VOLT_PROTOCOL_VERSION + 1),1:8000>>)),

	%*************************************************************************%
	%                                                                         %
	%                                   Login                                 % 
	%                                                                         %
	%*************************************************************************%

	%%%------------------------------------------------------------------------
	%%% Doc Sample
	%%%------------------------------------------------------------------------

	Name_1 = "scooby",
	Password_1 = "doo",
	LoginBin_1 = 
	<<0,0,0,0,8,100,97,116,97,98,97,115,101,0,0,0,6,115,99,111,111,98,121,
	  100,0,206,195,125,204,35,157,11,249,130,253,108,114,251,3,200,166,183,143>>,
	
	%%% wrong: <<0,0,0,0,6,115,99,111,111,98,121,
	%%% 100 ,0 ,-50 ,-61 ,125 ,-52 ,35 ,-99 ,11 ,-7 ,-126 ,-3 ,108 ,114 ,-5 ,3 ,-56 ,-90 ,-73 ,113>>,
	%%% (Wire frame document is wrong in showing last byte as 113 and omits 
	%%% literal string 'database'.)
	%%% --- pg. 9+10, VoltDB Client Wire Protocol Version 0, 05/05/10 ---
	%%% Note: not using first four bytes: length is prepended by tcp_gen::send().
	
	?ERLUNIT_EQUAL(erlvolt:volt_login("database", Name_1, Password_1), LoginBin_1),

	%%%------------------------------------------------------------------------
	%%% NOT ANY MORE: Captured Sample
	%%%------------------------------------------------------------------------

	Name_2 = "program",
	Password_2 = "password",
	LoginBin_2 = <<0,0,0,0,8,100,97,116,97,98,97,115,101,0,0,0,7,112,114,111,
	               103,114,97,109,91,170,97,228,201,185,63,63,6,130,37,11,108,
	               248,51,27,126,230,143,216>>,
	%%% NOT ANY MORE: <<16#000000000770726f6772616d5baa61e4c9b93f3f0682250b6cf8331b7ee68fd8:(32*8)>>,
	%%% NOT ANY MORE: aptured from helloworld sample of VoltDB 0.9.01. 
	%%% Not using first four bytes as the length is prepended by tcp_gen::send().

	?ERLUNIT_EQUAL(erlvolt:volt_login("database", Name_2, Password_2), LoginBin_2),

	% TODO: missing name or password, too long name or password (?)
	% TODO: make real capture again or at least verify.

	%%%------------------------------------------------------------------------
	%%% SHA-1 Hash
	%%%------------------------------------------------------------------------

	Sha1_doo = <<100 ,0 ,-50 ,-61 ,125 ,-52 ,35 ,-99 ,11 ,-7 ,-126 ,-3 ,108 ,114 ,-5 ,3 ,-56 ,-90 ,-73 ,143>>,
	
	%%% (Wire frame doc 05/05/10 seems to be wrong in showing last byte as 113.)
	%%% --- pg. 10, VoltDB Client Wire Protocol Version 0, 05/05/10 ---
	%%% For an easy neutral source check http://www.functions-online.com/sha1.html
	
	Sha1_wee = <<16#766865e81f4abe0d05a886d42c68263083bf3014:160>>,
	Sha1_password = <<16#5baa61e4c9b93f3f0682250b6cf8331b7ee68fd8:160>>,

	?ERLUNIT_EQUAL(begin crypto:start(), C = erlvolt:volt_hash("doo"), C end, Sha1_doo),
	?ERLUNIT_EQUAL(begin crypto:start(), C = erlvolt:volt_hash("wee"), C end, Sha1_wee),
	?ERLUNIT_EQUAL(begin crypto:start(), C = erlvolt:volt_hash("password"), C end, Sha1_password),
	% TODO: find out about stopping the crypto engine. crypto:stop() gives 'REPORT'.
	

	%*************************************************************************%
	%                                                                         %
	%                               Parameter                                 % 
	%                                                                         %
	%*************************************************************************%

	%%%------------------------------------------------------------------------
	%%% All Parameter Types: Single Parameter
	%%%------------------------------------------------------------------------

	erlunit:suite("Client Parameters"),

	% TODO: ARRAY
	% TODO: NULL - ?ERLUNIT_EQUAL(erlvolt:volt_parameter(NULL), ....),

	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_TINYINT, 0}),   <<?VOLT_TINYINT:8,  0:?VOLT_TINYINT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_SMALLINT, 0}),  <<?VOLT_SMALLINT:8, 0:?VOLT_SMALLINT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_INTINT, 0}),    <<?VOLT_INTINT:8,   0:?VOLT_INTINT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_BIGINT, 0}),    <<?VOLT_BIGINT:8,   0:?VOLT_BIGINT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_FLOAT, 0.0}),   <<?VOLT_FLOAT:8,  0.0:?VOLT_FLOAT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_STRING, ""}),   <<?VOLT_STRING:8,   0:?VOLT_STRING_SIZE_BINARY_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_TIMESTAMP,     {{0,1,1},{0,0,0}}}), <<?VOLT_TIMESTAMP:8, 255,35,35,62,86,233,0,0>>), % TODO: verify.
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_DECIMAL, 0}),   <<?VOLT_DECIMAL:8,  0:?VOLT_DECIMAL_TYPE>>), % TODO: verify.
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_ARRAY, []}),    <<?VOLT_ARRAY:8, ?VOLT_INTEGER:8, 0:16>>), 

	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_TINYINT, 1}),   <<?VOLT_TINYINT:8,  1:?VOLT_TINYINT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_SMALLINT, 1}),  <<?VOLT_SMALLINT:8, 1:?VOLT_SMALLINT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_INTINT, 1}),    <<?VOLT_INTINT:8,   1:?VOLT_INTINT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_BIGINT, 1}),    <<?VOLT_BIGINT:8,   1:?VOLT_BIGINT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_FLOAT, 1.0}),   <<?VOLT_FLOAT:8,  1.0:?VOLT_FLOAT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_STRING, "1"}),  <<?VOLT_STRING:8,   1:32, "1">>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_TIMESTAMP,     {{2000,1,1},{1,0,0}}}), <<?VOLT_TIMESTAMP:8, 0,3,93,2,17,203,132,0>>), % TODO: verify.
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_DECIMAL, 1}),   <<?VOLT_DECIMAL:8, 0,0,0,0,0,0,0,0,0,0,0,232,212,165,16,0>>), % TODO: verify.
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_ARRAY, [1]}),    <<?VOLT_ARRAY:8, ?VOLT_INTEGER:8, 1:16, 1:?VOLT_INTEGER_TYPE>>), 

	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_TINYINT, -1}),  <<?VOLT_TINYINT:8, -1:?VOLT_TINYINT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_SMALLINT, -1}), <<?VOLT_SMALLINT:8,-1:?VOLT_SMALLINT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_INTINT, -1}),   <<?VOLT_INTINT:8,  -1:?VOLT_INTINT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_BIGINT, -1}),   <<?VOLT_BIGINT:8,  -1:?VOLT_BIGINT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_FLOAT, -1.0}),  <<?VOLT_FLOAT:8, -1.0:?VOLT_FLOAT_TYPE>>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_STRING, "-1"}), <<?VOLT_STRING:8,   2:32, "-1">>),
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_TIMESTAMP,     {{1999,12,31},{23,59,59}}}), <<?VOLT_TIMESTAMP:8, 0,3,93,1,59,40,157,192>>), % TODO: verify.
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_DECIMAL, -1}),  <<?VOLT_DECIMAL:8, 255,255,255,255,255,255,255,255,255,255,255,23,43,90,240,0>>), % TODO: verify.
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_ARRAY, [-1]}),  <<?VOLT_ARRAY:8, ?VOLT_INTEGER:8, 1:16, -1:?VOLT_INTEGER_TYPE>>), 
	?ERLUNIT_EQUAL(erlvolt:volt_parameter({?VOLT_ARRAY, [-1,0,1]}),    <<?VOLT_ARRAY:8, ?VOLT_INTEGER:8, 3:16,
																        -1:?VOLT_INTEGER_TYPE, 0:?VOLT_INTEGER_TYPE, 1:?VOLT_INTEGER_TYPE>>), 
	%%%------------------------------------------------------------------------
	%%% Multiple Parameters (note 's' at end of function name here)
	%%%------------------------------------------------------------------------

	?ERLUNIT_EQUAL(erlvolt:volt_parameters(
		[{?VOLT_INTINT, 6},
		 {?VOLT_ARRAY, ["Edwina Burnam","Tabatha Gehling","Kelly Clauss","Jessie Alloway","Alana Bregman",
		 			    "Jessie Eichman","Allie Rogalski","Nita Coster","Kurt Walser","Ericka Dieter",
		 			    "Loraine Nygren","Tania Mattioli"]}]),
		
		Parameters_Bin_12Names = <<0,2,
			?VOLT_INTINT,0,0,0,6,
			?VOLT_ARRAY, ?VOLT_STRING,0,12,
				0,0,0,13,
				69,100,119,105,110,97,32,66,117,114,110,97,109,
				0,0,0,15,
				84,97,98,97,116,104,97,32,71,101,104,108,105,110,103,
				0,0,0,12,
				75,101,108,108,121,32,67,108,97,117,115,115,
				0,0,0,14,
				74,101,115,115,105,101,32,65,108,108,111,119,97,121,
				0,0,0,13,
				65,108,97,110,97,32,66,114,101,103,109,97,110,
				0,0,0,14,
				74,101,115,115,105,101,32,69,105,99,104,109,97,110,
				0,0,0,14,
				65,108,108,105,101,32,82,111,103,97,108,115,107,105,
				0,0,0,11,
				78,105,116,97,32,67,111,115,116,101,114,
				0,0,0,11,
				75,117,114,116,32,87,97,108,115,101,114,
				0,0,0,13,
				69,114,105,99,107,97,32,68,105,101,116,101,114,
				0,0,0,14,
				76,111,114,97,105,110,101,32,78,121,103,114,101,110,
				0,0,0,14,
				84,97,110,105,97,32,77,97,116,116,105,111,108,105>>),


	?ERLUNIT_EQUAL(erlvolt:volt_parameters(
		[6,
		 ["Edwina Burnam","Tabatha Gehling","Kelly Clauss","Jessie Alloway","Alana Bregman",
		 			    "Jessie Eichman","Allie Rogalski","Nita Coster","Kurt Walser","Ericka Dieter",
		 			    "Loraine Nygren","Tania Mattioli"]]),
		 Parameters_Bin_12Names),		
				
	%%%------------------------------------------------------------------------
	%%% Doc Sample
	%%%------------------------------------------------------------------------

	%%% --- pg. 14-15, VoltDB Client Wire Protocol Version 0, 05/05/10 ---

	Parameters_Bin_1 = <<0,2,-99, 9,0,2,0,0,0,4,102,111,111,49,0,0,0,4,102,111,111,50,22,
						-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,-83 ,33,-46 ,-78 ,57,-39,-128>>,

	Parameters_Erl_1 = [{ ?VOLT_ARRAY, { voltarray, ?VOLT_STRING, [ <<"foo1">>, <<"foo2">> ]}},
						{ ?VOLT_DECIMAL, -23325.23425 }],

	?ERLUNIT_EQUAL(erlvolt:volt_parameters(Parameters_Erl_1),  Parameters_Bin_1),

%	c = <<0,0,0,56,0,0,0,0,4,112,114,111,99,0,1,2,3,4,5,6,7,0,2,-99,
% 						9,0,2,0,0,0,4,102,111,111,49,0,0,0,4,102,111,111,50,22,
%						-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,-1 ,-83 ,33,-46 ,-78 ,57,-39,-128>>,

	%%%---------------------------------------------------------------------%%%
	%%% Captured Sample                                                     %%%
	%%%---------------------------------------------------------------------%%%
	%                                                                         %
	%   Frame 40 (111 bytes on wire, 111 bytes captured)                      %
	%   Seq: 208, Ack: 164, Len: 55                                           %
	%   Data (55 bytes)                                                       %
	%                                                                         %
	%   0000 00 00 00 33 00 00 00 00 06 49 6e 73 65 72 74 80 ...3.....Insert. %
	%   0010 00 00 00 00 00 00 03 00 03 09 00 00 00 03 48 65 ..............He %
	%   0020 6a 09 00 00 00 06 56 65 72 64 65 6e 09 00 00 00 j.....Verden.... %
	%   0030 06 44 61 6e 69 73 68                            .Danish          %
	%                                                                         %
	%   --- Part of client invocation, example/helloworld, 10.0.01 r640 ---   %
	%                                                                         %
	%%%---------------------------------------------------------------------%%%

	Parameters_Bin_2 = <<00,03,                            % 3 parameters
						09,00,00,00,03,16#48,16#65,16#6a,           % Hej
						09,00,00,00,06,16#56,16#65,16#72,16#64,16#65,16#6e,  % Verden
						09,00,00,00,06,16#44,16#61,16#6e,16#69,16#73,16#68>>, % Danish

	Parameters_Erl_2 = [{ ?VOLT_STRING, <<"Hej">> }, 
						{ ?VOLT_STRING, <<"Verden">> },
						{ ?VOLT_STRING, <<"Danish">> }],

	?ERLUNIT_EQUAL(erlvolt:volt_parameters(Parameters_Erl_2),  Parameters_Bin_2),

	% TODO: more volt_parameters tests
	
	% TODO: volt_invoke tests

	%*************************************************************************%
	%                                                                         %
	%                                Response                                 % 
	%                                                                         %
	%*************************************************************************%

	erlunit:suite("Server Response"),

	%%%------------------------------------------------------------------------
	%%% Respone Test #1 Doc Sample (corrected)
	%%%------------------------------------------------------------------------

	%%% --- pg. 16-18, VoltDB Client Wire Protocol Version 0, 05/05/10 ---

	Response_Bin_1 =
	<<
%%	0,0,0,109,         % Msg Length %% cut off by Erlang see (***)
	0,                 % Protocol version
	0,1,2,3,4,5,6,7,   % Client data
	
	-32,               % Fields present
	
	2,                 % Status Code
	0,0,0,4,           % Status String length
	102,97,105,108,    % Status String: "fail"
	99,                % App status code
	0,0,0,4,           % App status string length
	118,111,108,116,   % App status string: "volt"
	
	00,00,00,01,       % Roundtrip time (added to example)

	0,0,0,5,           % Serlzd Exc length
	1,                 % Exc ordinal
	0,0,0,0,           % Exc body

	0,2,               % result table count
	
	0,0,0,32,          % Total table length
	0,0,0,12,          % Table metadata length
	
	0,                 % Status code
	0,1,               % Column count
	6,                 % Column 1 Type: BIGINT
	0,0,0,4,           % Col 1 Name length
	84,101,115,116,    % Col 1 Name: "Test"
	0,0,0,1,           % Row count
	0,0,0,8,           % Row 1 Length
	0,0,0,0,0,0,0,5,   % Row 1 Col 1 Value: 5
	
	0,0,0,32,          % Total table length
	0,0,0,12,          % Table metadata length
	
	0,                 % Status code
	0,1,               % Column count
	6,                 % Column 1 Type: BIGINT
	0,0,0,4,           % Col 1 Name length
	84,101,115,116,    % Col 1 Name: "Test"
	0,0,0,1,           % Row count
	0,0,0,8,           % Row 1 Length
	0,0,0,0,0,0,0,5    % Row 1 Col 1 Value: 5
	>>,

	Response_Erl_1 =
	
	{ voltresponse, 
		% protocol, client data, status code, status string, app status, app status string, serialized exc
		{ 0, <<0,1,2,3,4,5,6,7>>, 2, <<"fail">>, 99, <<"volt">>, <<1,0,0,0,0>>, 1 },
	
    	[
    	  { volttable, 
        	[<<"Test">>], 
            [?VOLT_BIGINT], 
            [ { voltrow, [5] } ]
          },
    	  { volttable, 
        	[<<"Test">>], 
            [?VOLT_BIGINT], 
            [ { voltrow, [5] } ]
          }
        ]
	},

	?ERLUNIT_EQUAL(erlvolt:erl_response(Response_Bin_1),  Response_Erl_1),

	%%%------------------------------------------------------------------------
	%%% Response Test #2 Hello World from wire
	%%%------------------------------------------------------------------------
	%
	%   Wire transmit of Hello World example / VoltDB 0.9.01 r ~600
    %
    %   0000  00 00 00 00 00 00 00 00  00 00 00 00 08 00 45 00   ........ ......E.
    %   0010  00 82 77 1a 40 00 40 06  c5 59 7f 00 00 01 7f 00   ..w.@.@. .Y......
    %   0020  00 01 52 dc a3 04 84 8c  91 a9 84 a1 c0 00 80 18   ..R..... ........
    %   0030  01 00 fe 76 00 00 01 01  08 0a 28 ea e6 7b 28 ea   ...v.... ..(..{(.
    %   0040  e6 79
    %   starts here:00 00 00 4a 00 80  00 00 00 00 00 00 05 00   .y...J.. ........
    %   0050  01 80 00 00 00 05 00 01  00 00 00 34 00 00 00 17   ........ ...4....
    %   0060  80 00 02 09 09 00 00 00  05 48 45 4c 4c 4f 00 00   ........ .HELLO..
    %   0070  00 05 57 4f 52 4c 44 00  00 00 01 00 00 00 11 00   ..WORLD. ........
    %   0080  00 00 04 48 6f 6c 61 00  00 00 05 4d 75 6e 64 6f   ...Hola. ...Mundo
	%
	%   --- Ariel Weisberg, VoltDB, 05/24/10

	Response_Bin_2 =
	<< %% 0,0,0,16#4a,              % Msg Length %% cut off by Erlang, see (***)
	00,                             % Protocol version
	16#80,0,0,0, 0,0,0,5,           % Client data
	
	00,                             % Fields present: NO Status string, NO App Status string, NO Serialzd Exc
	
	01,                             % Status Code
	16#80,                          % App status code

	00,00,00,05,                    % Roundtrip time
	
	0,1,                            % result table count
	
	0,0,0,16#34,                    % Total table length: x34 = 52
	0,0,0,16#17,                    % Table metadata length: x17 = 23
	
	16#80,                          % Status code
	
	00,02,                          % Column count
	
	09,                             % Column 1 Type: STRING
	09,                             % Column 2 Type: STRING
	
	0,0,0,5,                        % Col 1 Name length
	16#48,16#45,16#4c,16#4c,16#4f,  % Col 1 Name: "HELLO"
	0,0,0,5,                        % Col 1 Name length
	16#57,16#4f,16#52,16#4c,16#44,  % Col 1 Name: "WORLD"
	
	0,0,0,1,                        % Row count
	0,0,0,16#11,                    % Row 1 Length
	00,00,00,04,                    % Row 1 Col 1 string length: 4
	16#48,16#6f,16#6c,16#61,        % Row 1 Col 1 Value: "Hola"
	00,00,00,05,                    % Row 1 Col 2 string length: 5
	16#4d,16#75,16#6e,16#64,16#6f   % Row 1 Col 2 Value: "Mundo"
	>>,

	Response_Erl_2 =
	
	{ voltresponse,
	
		% protocol, client data, status code, status string, app status, app status string, serialized exc, roundtrip time
		{ 0, <<16#80,0,0,0,0,0,0,5>>, 1, <<"">>, 16#80, <<"">>, <<>>, 5 },
    	[ 
    	  { volttable, 
        	[<<"HELLO">>, <<"WORLD">>], 
            [?VOLT_STRING, ?VOLT_STRING], 
            [ { voltrow, [<<"Hola">>,<<"Mundo">>] } ]
          } 
        ]
	},

	?ERLUNIT_EQUAL(erlvolt:erl_response(Response_Bin_2),  Response_Erl_2),


	%%%------------------------------------------------------------------------
	%%% Mounting of Tests Done. Execute.
	%%%------------------------------------------------------------------------

	erlunit:execute().
	
	
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%                                                                         %%%
%%%                               CONNECT                                   %%% 
%%%                                                                         %%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%*************************************************************************%
%                             Emulated Login                              % 
%*************************************************************************%
%                                                                         %
%**************************************************************************

%%%------------------------------------------------------------------------
%%% Low Level Echo Over Emulated Connection - No VoltDB Specifics Just Yet.
%%%------------------------------------------------------------------------

%emulated_echo() ->
%
%	Port = 20000,
%
%	Server = erlvolt:start_server_emulator(20000, [echo]),
%	Client = erlvolt:start_client([dump]),
%	
%	erlvolt:connect(Client, 20000),
%	erlvolt:send(Client, "Hello!"),
%	"Hello!" = erlvolt:receive().


%%%-----------------------------------°%°-----------------------------------%%%
