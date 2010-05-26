%%%-------------------------------------------------------------------------%%%
%%% File        : erlvolt.erl                                               %%% 
%%% Version     : 0.1/alpha                                                 %%%
%%% Description : Erlang-VoltDB client API                                  %%%
%%% Copyright   : VoltDB, LLC - http://www.voltdb.com                       %%%
%%% Production  : Eonblast Corporation - http://www.eonblast.com            %%%
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%
%%% Licence     : GPLv3                                                     %%%
%%% Created     : 17 Apr 2010                                               %%%
%%% Changed     : 25 May 2010                                               %%% 
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%   Erlvolt is an Erlang interface to a VoltDB server. It allows for      %%%
%%%   Erlang programs to talk with the VoltDB server as VoltDB clients.     %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%   This API is being contributed to VoltDB by Eonblast Corporation.      %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%   STATUS                                                                %%%
%%%                                                                         %%%
%%%   All basic functionality is implemented and unit tested. Test pro-     %%%
%%%   grams are being written to test usability. Function signatures are    %%%
%%%   yet to change mildly. Asynchronous calls are not yet supported.       %%%
%%%                                                                         %%%
%%%   REQUIREMENTS                                                          %%%
%%%                                                                         %%%
%%%   + VoltDB 1.0.01                                                       %%%
%%%   + Tested on Java 1.6.0-17 (use -18+!)                                 %%%
%%%   + Tested on Erlang R13B01                                             %%%
%%%                                                                         %%%
%%%   TESTS                                                                 %%%
%%%                                                                         %%%
%%%   + Have at least Erlang R12B-4                                         %%%
%%%   + Get Erlunit from http://github.com/Eonblast/Erlunit/tarball/master  %%%
%%%     and put it into subfolder erlunit, inside your Erlvolt folder.      %%%
%%%   + From the OS command line run ./test                                 %%%
%%%                                                                         %%%
%%%   SAMPLE                                                                %%%
%%%                                                                         %%%
%%%   + From the OS command line run ./hello                                %%% 
%%%                                                                         %%%
%%%   DOCS                                                                  %%%
%%%                                                                         %%%
%%%   + point your browser to doc/index.html                                %%% 
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%     Erlvolt is used for Solar Pirates http://www.eonblast.com.          %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%    Erlvolt 0.1/alpha - an Erlang-VoltDB client API.                     %%%
%%%    Copyright (C) 2010 VoltDB, LLC http://www.voltdb.com                 %%%
%%%    Author H. Diedrich <hd2010@eonblast.com> http://www.eonblast.com     %%%
%%%                                                                         %%%
%%%    This program is  free software:  you can redistribute it and / or    %%%
%%%    modify it  under the terms of the  GNU  General Public License as    %%%
%%%    published  by the Free Software Foundation,  either version  3 of    %%%
%%%    the License,  or (at your option) any later version.                 %%%
%%%                                                                         %%%
%%%    This program is distributed  in the hope  that it will be useful,    %%%
%%%    but  WITHOUT ANY WARRANTY;  without  even the implied warranty of    %%%
%%%    MERCHANTABILITY  or  FITNESS FOR  A PARTICULAR PURPOSE.  See  the    %%%
%%%    GNU General Public License for more details.                         %%%
%%%                                                                         %%%
%%%    You should have received a copy of the GNU General Public License    %%%
%%%    along with this program. If not, see <http://www.gnu.org/licenses/>. %%%
%%%                                                                         %%%
%%%-------------------------------------------------------------------------%%%
%%%
%%%    @doc
%%%
%%%    This is the module to import into your client module to access 
%%%    VoltDB.
%%%
%%%    It provides you with functions to communicate with the VoltDB
%%%    server.
%%%
%%%    @end
%%%
%%%-------------------------------------------------------------------------%%%
%%%
%%%    To generate up to date documentation from edoc tags, run:
%%%
%%%    1> edoc:files([erlvolt.erl, test1.erl],[{dir, "doc"}, {new,true}, {stylesheet,"erlvolt.css"}]).
%%%
%%%-------------------------------------------------------------------------%%%

-module(erlvolt).

-vsn("0.1/alpha").
-author("H. Diedrich <hd2010@eonblast.com>").
-license("MIT - http://www.opensource.org/licenses/mit-license.php").
-copyright("(c) 2010 VoltDB, LLC - http://www.voltdb.com").

-define(VERSION, "0.1/alpha").
-define(LIBRARY, "Erlvolt").
-define(EXPLAIN, "Erlang VoltDB Client API").

-define(V, false). % verbosity

%%%-------------------------------------------------------------------------%%%

-include("erlvolt.hrl").

-import(lists, [reverse/1]).
-import(ets).

-export([ 	add_callback/1,
			banner/0,
			banner/1,
			create_callback_id/0,
			create_callback_table/0,
			delete_callback/1,
			erl_any/2,
			erl_array_feed/1,
			erl_array/1,
			erl_bigint_feed/1,
			erl_binary_array_feed/1,
			erl_binary_array/1,
			erl_datetime/1,
			erl_decimal_feed/1,
			erl_elements_feed/5,
			erl_elements/4,
			erl_float_feed/1,
			erl_float_or_atom/1,
			erl_float_or_null_from_decimal/1,
			erl_float/1,
			erl_header/1,
			erl_integer_from_decimal/1,
			erl_integer_or_null_from_decimal/1,
			erl_integer/1,
			erl_intint_feed/1,
			erl_nowtime/1,
			erl_number_or_null/1,
			erl_number/1,
			erl_smallint_feed/1,
			erl_string_or_null/1,
			erl_table/1,
			erl_plaintable/1,
			erl_time/1,
			erl_timestamp_feed/1,
			erl_tinyint_feed/1,
			erl_unixtime/1,
			erl_response/1,
			execute_callback/2,
			get_callback_or_nil/1,
			get_callback/1,
  			help/0,
			milli_epoch/1,
			resolve_callback/2,
			volt_array/1, volt_array/2,
			volt_byte/1,
			volt_decimal/1,
			volt_rows/2,
			volt_fields/2,
			volt_float/1,
			volt_integer/1,
			volt_long/1,
			volt_short/1,
			volt_string/1,
			volt_table/1,
			volt_table/2,
			volt_time_binary/1,
			volt_time/1,
			volt_type/1,
			volt_small/1,
			volt_intint/1,
			volt_bigint/1,
			vecho/3,
			
			login/3, 
			login/4, 
			open/4, 
			connect/2,
			createConnection/0,
			createConnection/1,
			createConnection/3,
			createConnection/4,
			callProcedure/3,
			callProcedure/4,
			callProcedure/5,
			volt_login/2,
			volt_hash/1,
			volt_header/0,
			volt_invoke/3,
			volt_parameters/1,
			volt_parameter/1,
			
			getField/2,
			getString/2,
			getString/3,
			listOrd/2,
			fetchRow/2,
			fetchRecord/3]).


% TODO: sort export lines.
% TODO: use defines to import those that are exported only for testing only 
% then, when testing.

%%%----------------------------------------------------------------------------
%%% Call for 1st test: emulating server communication
%%%----------------------------------------------------------------------------
% emulate() ->
%	banner("Testing standalone, emulating a VoltDB server.").

%%%----------------------------------------------------------------------------
%%% Call for 2nd test: real server communication
%%%----------------------------------------------------------------------------
% run() ->
%	banner("Testing with real VoltDB server.").



% test_get_url() ->
%     test_get_url("www.google.com").
% 
% test_get_url(Host) ->
%    {ok,Socket} = gen_tcp:connect(Host,80,[binary, {packet, 0}]), %% (1)
%     ok = gen_tcp:send(Socket, "GET / HTTP/1.0\r\n\r\n"),  %% (2)
%     receive_data(Socket, []).
% 
% receive_data(Socket, SoFar) ->
%     receive
% 	{tcp,Socket,Bin} ->    %% (3)
% 	    receive_data(Socket, [Bin|SoFar]);
% 	{tcp_closed,Socket} -> %% (4)
% 	    list_to_binary(reverse(SoFar)) %% (5)
%   end.


%%%****************************************************************************
%%% HELP
%%%****************************************************************************
%%% @doc Displays a brief pointer about were to get more help.

help() ->
	banner(),
    io:format("See sample*.erl and test.erl for more information.~n").



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                              BASIC TYPES                                    % 
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%*****************************************************************************%
%                        Floating Point Numbers                               % 
%*****************************************************************************%
%
% See erlvolt.hrl for the official Java bit patterns for NaN and Infinities.
% IEEE 754 does not define 'the' bit pattern for, e.g. NaN. Java does. In any
% case there are many bit patterns that are NaN, meaning, they don't have a
% meaningful value as float numbers. But again, Java defines one as 'the' NaN.
% This probably needs to be used for communicating NaN to a Stored Procedure.
%
% -- Wikipedia on IEEE 754: [http://en.wikipedia.org/wiki/IEEE_754-1985] --
%
% "Only 8-Byte Double types are supported using the byte representation in
% IEEE 754 "double format." Positive and negative infinity, as well as NaN, are
% supported on the wire, but not guaranteed to work as SQL values."
%
% -- pg. 1, VoltDB Client Wire Protocol Version 0, 12/11/09 --
%
% "When most people talk of IEEE-754, they are referring to  IEEE-754-1985
% which which encompasses single and double precision floating points, which
% are both binary FP formats. Erlang's floating point type is the double
% precision binary." 
%
% -- D. Smith [http://www.erlang.org/cgi-bin/ezmlm-cgi/4/47070] --
%
% "... you can construct invalid floating-point bit patterns [...], and since
% Erlang doesn't include "not-a-numbers" in the float data type, an exit is
% signalled. If you expect there to be non-numbers in the binary, you can use
% 'catch' to catch the exit: ```
%
% bytes_to_float(A,B,C,D) ->
%     <<Float:32/signed-float>> = <<A, B, C, D>>,
%     Float.
% 
% convert(A,B,C,D) ->
%     case catch bytes_to_float(A,B,C,D) of
% 	{'EXIT', _} ->
% 	    not_a_number;
% 	F ->
% 	    F
%     end.                                                               '''
% 
% -- D. Walling, http://www.erlang.org/cgi-bin/ezmlm-cgi/4/3254 --
%
% The Zuse Z3, the world's first working computer, implemented floating point
% with infinities and undefined values which were passed through operations. 
% These were not implemented properly in hardware in any other machine until 
% the Intel 8087, which was announced in 1980. 
%
% -- Wikipedia on IEEE 754: [http://en.wikipedia.org/wiki/IEEE_754-1985] --

% Needs to be defined here because of the erl_integer() defintion for decimals.
-define(VOLT_DECIMALS_SCALESHIFT, 1000000000000).

% @type wiretype() = volttype() + binary()

%%%-encode---------------------------------------------------------------------

%%%----------------------------------------------------------------------------
%%% @doc Erlang value to VoltDB float.
%%% @spec volt_float(float() | nan | positive_infinity | negative_infinity ) -> wiretype()

%%%----------------------------------------------------------------------------
%%% VoltDB not-a-number wire code (official Java bit pattern)
%%%----------------------------------------------------------------------------

volt_float(nan) -> 

	?VOLT_NAN;
	

%%%----------------------------------------------------------------------------
%%% VoltDB positive infinity float wire code (official Java bit pattern)
%%%----------------------------------------------------------------------------

volt_float(positive_infinity) -> 

	?VOLT_POSITIVE_INFINITY;


%%%----------------------------------------------------------------------------
%%% VoltDB negative infinity float wire code (official Java bit pattern)
%%%----------------------------------------------------------------------------

volt_float(negative_infinity) -> 

	?VOLT_NEGATIVE_INFINITY;


%%%----------------------------------------------------------------------------
%%% Erlang float to VoltDB float wire code
%%%----------------------------------------------------------------------------

volt_float(E) when is_float(E) -> 

	<<E:32/float>>.


%%%----------------------------------------------------------------------------
%%%-decode---------------------------------------------------------------------
%%%----------------------------------------------------------------------------
	

%%%----------------------------------------------------------------------------
%%% @doc  VoltDB float wire code to Erlang float - throws on NaN/Infinities
%%% @see volt_float/1.

erl_float(<<V:32/signed-float>>) -> 
	
	V.

%%%----------------------------------------------------------------------------
%%% @doc  VoltDB float wire code to Erlang float - with NaN/Infinities to atoms - any 32bit.
%%% @see volt_float/1.

erl_float_or_atom(<<V:32/signed-float>>) -> 

	V;

erl_float_or_atom(?VOLT_NAN) ->

	nan;

erl_float_or_atom(?VOLT_POSITIVE_INFINITY) ->

	positive_infinity;

erl_float_or_atom(?VOLT_NEGATIVE_INFINITY) ->

	negative_infinity;

erl_float_or_atom(<<_:32/signed-float>>) -> 

	nan.


%*****************************************************************************%
%                                 Integers                                    % 
%*****************************************************************************%
%                                                                             
%   All integer types are signed, twos-compliment and big-endian.             
%   * Byte - 1 Byte                                                       <br/>
%   * Short - 2 Bytes                                                     <br/>    
%   * Integer - 4 Bytes                                                   <br/>
%   * Long - 8 Bytes                                                      <br/>    
%
%   --- pg. 1, VoltDB Client Wire Protocol Version 0, 12/11/09 ---            
%                                                                             
%   Also see http://en.wikipedia.org/wiki/Integer_%28computer_science%29      
%                                                                             
%******************************************************************************

%%%-encode------------------------------------------------------------integers-

%%%----------------------------------------------------------------------------
%%% @doc Erlang integer to VoltDB TINYINT (8 bit) wire code.

volt_byte(E) when is_integer(E), E >= -16#80, E =< 16#7f ->
	
	<<E:8>>.

%%%----------------------------------------------------------------------------
%%% @doc Erlang integer to VoltDB SHORTINT (16 bit) wire code. 

volt_short(E) when is_integer(E), E >= -16#8000, E =< 16#7fff ->
	
	<<E:16>>.

%%%----------------------------------------------------------------------------
%%% @doc Erlang integer to VoltDB SHORTINT (16 bit) wire code. 

volt_small(E) when is_integer(E), E >= -16#8000, E =< 16#7fff ->
	
	<<E:16>>.

%%%----------------------------------------------------------------------------
%%% @doc Erlang integer to VoltDB INTEGER (32 bit) wire code. 

volt_integer(E) when is_integer(E), E >= -16#80000000, E =< 16#7fffffff ->
	
	<<E:32>>.

%%%----------------------------------------------------------------------------
%%% @doc Erlang integer to VoltDB INTEGER (32 bit) wire code. 

volt_intint(E) when is_integer(E), E >= -16#80000000, E =< 16#7fffffff ->
	
	<<E:32>>.

%%%----------------------------------------------------------------------------
%%% @doc Erlang integer to VoltDB BIGINT (64 bit) wire code.

volt_long(E) when is_integer(E), E >= -16#8000000000000000, E =< 16#7fffffffffffffff ->
	
	<<E:64>>.

%%%----------------------------------------------------------------------------
%%% @doc Erlang integer to VoltDB BIGINT (64 bit) wire code.

volt_bigint(E) when is_integer(E), E >= -16#8000000000000000, E =< 16#7fffffffffffffff ->
	
	<<E:64>>.

%%%-decode---------------------------------------------------------------------
	
%%%----------------------------------------------------------------------------
%%% @doc VoltDB integer wire code to Erlang integer
%%% @spec erl_integer(binary()) -> integer() 
%%% @end 
%%%----------------------------------------------------------------------------

%%%----------------------------------------------------------------------------
%%% VoltDB empty integer wire code to Erlang integer.
%%%----------------------------------------------------------------------------
erl_integer(<<>>) ->
	
	0;

%%%----------------------------------------------------------------------------
%%% VoltDB TINYINT wire code to Erlang integer.
%%%----------------------------------------------------------------------------
% TODO: replace all BYTE references with TINYINT.

erl_integer(<<E:8/signed>>=V) when is_binary(V), E >= ?VOLT_BYTE_MIN, E =< ?VOLT_BYTE_MAX ->
	
	E;

%%%----------------------------------------------------------------------------
%%% VoltDB SHORT wire code to Erlang integer.
%%%----------------------------------------------------------------------------
erl_integer(<<E:16/signed>>=V) when is_binary(V), E >= ?VOLT_SHORT_MIN, E =< ?VOLT_SHORT_MAX ->
	
	E;

%%%----------------------------------------------------------------------------
%%% VoltDB INTEGER wire code to Erlang integer.
%%%----------------------------------------------------------------------------
erl_integer(<<E:32/signed>>=V) when is_binary(V), E >= ?VOLT_INTEGER_MIN, E =< ?VOLT_INTEGER_MAX ->
	
	E;

%%%----------------------------------------------------------------------------
%%% VoltDB BIGINT wire code to Erlang integer.
%%%----------------------------------------------------------------------------
erl_integer(<<E:64/signed>>=V) when is_binary(V), E >= ?VOLT_LONG_MIN, E =< ?VOLT_LONG_MAX ->
	
	E;

%%%----------------------------------------------------------------------------
%%% VoltDB decimals wire code to Erlang integer.
%%%----------------------------------------------------------------------------
erl_integer(<<E:128/signed>>=V) when is_binary(V) ->

	case E rem ?VOLT_DECIMALS_SCALESHIFT of
		0 -> E div ?VOLT_DECIMALS_SCALESHIFT;
		true -> throw(loosing_precision)
	end.

%%%-decode-stream--------------------------------------------------------------


%*****************************************************************************%
%                                 Strings                                     % 
%*****************************************************************************%
%                                                                             %
%   VoltDB: Strings begin with a 4-byte integer storing the number of bytes   %
%   of character data, followed by the character data.  UTF-8 is the only     %
%   supported character encoding. Note: Strings are artificially limited to   %
%   1 megabyte. The NULL string has a length preceded value of -1 (negative   %
%   one) followed by 0 (zero) bytes of string data.The empty string is re-    %
%   presented with a length preceding value of 0.                             %
%   --- Pg. 2, VoltDB Client Wire Protocol Version 0, 05/05/10 ---            % 
%                                                                             %
%   TODO: 1M LIMIT respecting UTF-8 multibytes                                %
%                                                                             %
%   Erlang default is big endian.                                             %
%                                                                             %
%******************************************************************************

% Hint: erlvolt.htl ... -define(VOLT_STRING_SIZE_BINARY_TYPE, 32/big-signed) ...

%%%-encode---------------------------------------------------------------------


%%%----------------------------------------------------------------------------
%%% @doc VoltDB string wire code for NULL

volt_string(null) -> <<?VOLT_STRING_NULL>>;
	

%%%----------------------------------------------------------------------------
%%% @doc VoltDB string wire code for empty string

volt_string("")   -> <<?VOLT_STRING_EMPTY>>;
volt_string(<<>>) -> <<?VOLT_STRING_EMPTY>>;
	

%%%----------------------------------------------------------------------------
%%% @doc Erlang string (list) to VoltDB string wire code

volt_string(V) when is_list(V) -> 

	volt_string(list_to_binary(V));


%%%----------------------------------------------------------------------------
%%% @doc Erlang string (binary) to VoltDB string wire code

volt_string(V) when is_binary(V), erlang:size(V) > ?VOLT_MAX_STRING_SIZE ->

	throw(string_to_large);
	
volt_string(V) when is_binary(V) ->
	
	Size = erlang:size(V),
	<<Size:?VOLT_STRING_SIZE_BINARY_TYPE, V/binary>>.


%%%----------------------------------------------------------------------------
%%% @doc Erlang string (list) to VoltDB string wire code

% TODO: ? erl_string(A) when is_list(A) ->
%
%	null.

%%%-decode---------------------------------------------------------------------
	
%%%----------------------------------------------------------------------------
% VoltDB string code to Erlang string (list)
% TODO: ? volt_string2(A) when is_binary(A) ->
%	
%	null.
	
%%%----------------------------------------------------------------------------
% VoltDB string code to Erlang binary (list)
% TODO: ? erl_binary(A) when is_binary(A) ->
%	
%	null.

%%%----------------------------------------------------------------------------
% VoltDB string code to Erlang binary (list)
erl_string_or_null(<<?VOLT_STRING_NULL>>) -> 

	null;

erl_string_or_null(<<?VOLT_STRING_BINARY(String)>>) -> 

	String.


%*****************************************************************************%
%                                    Time                                     % 
%******************************************************************************


% Volt: All dates are represented on the wire as Long values. This signed 
% number represents milliseconds before or after Jan. 1 1970 00:00:00 GMT, 
% the Unix epoch. This covers roughly 4000BC to 8000AD.
% - pg. 2, VoltDB Client Wire Protocol Version 0, 12/11/09

% Erlang Standard: {Date,Time} = {{Hour,Minutes,Seconds},{Year,Month,Day}}.
% Unique Timestamps: {Megaseconds,Seconds,Microseconds} = erlang:now() 
% from 1/1/19070 0:00. erlang:now() is guaranteed to deliver unique results.
% Note that erlang:now() and erlang:universal_time() can be out of sync.
%
% Samples: erlang:now() -> {1272,805301,939371} 
% calendar:now_to_datetime({1272,805301,939371}) -> {{2010,5,2},{13,1,41}}. 

% Regarding the use of BILLION below, remember that Volt uses MILLI-seconds:
% 1 megasec == 1 billion millisec == 1 trillion microsec.


-define(VOLT_TIME_BINARY_TYPE(V), <<V:64/signed-big>>).

-define(VOLT_TIME_MIN, ?VOLT_LONG_MIN).
-define(VOLT_TIME_MAX, ?VOLT_LONG_MAX).

-define(TRILLION, 1000000000000).
-define(BILLION,  1000000000).
-define(MILLION,  1000000).


%%%-encode----------------------------------------------------------------time-


%%%----------------------------------------------------------------------------
%%% @doc Erlang 'now format' time to VoltDB wire code time binary.

volt_time({Mega, Sec, Micro}) when is_integer(Mega), is_integer(Sec), is_integer(Micro) ->

    volt_time_binary(milli_epoch({Mega, Sec, Micro}));


%%%----------------------------------------------------------------------------
%%% Erlang 'datetime' format to VoltDB wire code time binary

volt_time({Date,Time}) ->

	UnixEpoch = {{1970,1,1},{0,0,0}},
	MilliEpoch = (calendar:datetime_to_gregorian_seconds({Date,Time}) 
		- calendar:datetime_to_gregorian_seconds(UnixEpoch)) 
		* 1000,
    volt_time_binary(MilliEpoch);


%%%----------------------------------------------------------------------------
%%% UTC time to VoltDB wire code time binary

volt_time(UTC) when is_integer(UTC) ->

    volt_time_binary(UTC * 1000).


%%%----------------------------------------------------------------------------
%%% @doc Over/Underrun guards and cast to binary

volt_time_binary(MilliEpoch) when MilliEpoch < ?VOLT_TIME_MIN ->
    erlang:error(time_underrun);

volt_time_binary(MilliEpoch) when MilliEpoch > ?VOLT_TIME_MAX ->
    erlang:error(time_overrun);

volt_time_binary(MilliEpoch) when is_integer(MilliEpoch) ->
	?VOLT_TIME_BINARY_TYPE(MilliEpoch).


%%%----------------------------------------------------------------------------
%%% @doc actual calculation 'now' format to VoltDB wire format

milli_epoch({Mega, Sec, Micro}) ->
    Mega * ?BILLION + Sec * 1000 + trunc(Micro / 1000).


%%%-decode----------------------------------------------------------------time-
	

%%%----------------------------------------------------------------------------
%%% @doc VoltDB wire code time /as binary/ to Erlang 'DateTime' format

erl_datetime(?VOLT_TIME_BINARY_TYPE(Int)=V) when is_binary(V) -> 

	calendar:now_to_universal_time(erl_nowtime(Int)). % -> {Date,Time}


%%%----------------------------------------------------------------------------
%%% @doc VoltDB wire code time from binary to Erlang 'Now' format.
%%% Note: the 'now' format in Erlang means {Megasecs, Secs, Microsecs}.
%%% Because this 'now' can be confusing to read for, say, devs coming from Java,
%%% there are synomyms introduced, leaving the 'now' out: erl_time().
%%%
%%% Alternate Variant:
%%% VoltDB wire code time from integer to Erlang 'Now' format.
%%% Integer input represents an interm'ry. step but might come in handy somewhere.
%%%
%%% @spec erl_nowtime(wire() | integer()) -> {Megasecs, Secs, Microsecs}

erl_nowtime(?VOLT_TIME_BINARY_TYPE(VInt)=V) when is_binary(V) -> 

	erl_nowtime(VInt); % -> {Megasecs, Secs, Microsecs};

	
erl_nowtime(V) when is_integer(V) -> % TODO: make better
	
	Mega  = trunc(V / ?BILLION),
	Sec   = trunc((V - Mega * ?BILLION) / 1000),
	Micro = (V - Mega * ?BILLION - Sec * 1000) * 1000,
	{Mega, Sec, Micro}.


%%%----------------------------------------------------------------------------
%%% @doc synonyms of erl_nowtime/1.

erl_time(?VOLT_TIME_BINARY_TYPE(VInt)=V) when is_binary(V) -> 
	
	erl_nowtime(VInt); 
	
erl_time(V) when is_integer(V) ->
	
	erl_nowtime(V).


%%%----------------------------------------------------------------------------
%%% @doc Unix epoch seconds from VoltDB wire code time binary. 

erl_unixtime(?VOLT_TIME_BINARY_TYPE(VInt)=V) when is_binary(V) -> 

	erl_unixtime(VInt); % -> int() Seconds since 1/1/1970 0:00 GMT;

%%%----------------------------------------------------------------------------
%%% @doc Unix epoch seconds from VoltDB wire code time long integer.  

erl_unixtime(V) when is_integer(V) ->
	
	trunc(V / 1000).  % -> int() Seconds since 1/1/1970 0:00 GMT;


%*****************************************************************************%
%                                Decimals                                     % 
%*****************************************************************************%
%                                                                             %
%   "VoltDB implements a fixed precision and scale DECIMAL(38,12) type. This  %   
%   type is serialized as a 128 bit signed twos complement integer repre-     %
%   senting the unscaled decimal value. The integer must be in big-endian     %
%   byte order with the most significant bytes first. Null is serialized as   %
%   the smallest representable value which is                                 %
%   -170141183460469231731687303715884105728. Serializing values (Null        %
%   excluded) that are greater than 99999999999999999999999999999999999999    %
%   or less than -99999999999999999999999999999999999999" will result in an   %
%   error response."                                                          %
%                                                                             %
%   Erlang works with limited float precision and will therefore not be able  %
%   to feed floats into volt_decimal that have precision truncated. Try:      %
%                                                                             %
%   1> 23325.123456789012345.                                                 %
%   23325.123456789013                                                        %
%   -> the literal is accepted but the last decimals are rounded.             %
%                                                                             %
%   Note that big integers will work as expected:                             %
%   volt_decimal( 99999999999999999999999999999999999999 ) will work.         %
%   But big floats will not                                                   %
%   volt_decimal( 99999999999999999999999999999999999999.0) will crash,       %
%   because precision of floats is much lower:                                %
%                                                                             %
%   1> 99999999999999999999999999999999999999.                                %
%   99999999999999999999999999999999999999                                    %
%   2> 99999999999999999999999999999999999999.0.                              %
%   1.0e38                                                                    %
%   3> erlvolt:volt_decimal(1.0e38).                                          %
%   <<146,104,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>                                   %
%   4> erlvolt:volt_decimal(99999999999999999999999999999999999999).          %
%   <<146,102,135,210,196,5,52,253,181,99,255,23,43,90,240,0>>                %
%                                                                             %
%   This also makes for ambiguity when converting back that can even mask     %
%   the exact same problem of lacking float precision (binaries as above!):   %
%                                                                             %
%   5> erlvolt:erl_float(<<146,104,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>).            %
%   -1.4567508066305312e26                                                    %
%   6> erlvolt:erl_float(                                                     %
%              <<146,102,135,210,196,5,52,253,181,99,255,23,43,90,240,0>>).   %
%   -1.4568271043289421e26                                                    %
%                                                                             %
%   So eventually we erroneously get                                          %
%   erlvolt:volt_decimal(-1.4567508066305312e26)                              %
%   =:= erlvolt:volt_decimal(99999999999999999999999999999999999999.0)        %
%   =:= erlvolt:volt_decimal(1.0e38)                                          %
%   All three resulting into <<146,104,0,0,0,0,0,0,0,0,0,0,0,0,0,0>> which    %
%   is reconverted into                                                       %
%   erlvolt:erl_float(<<146,104,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>)                %
%   =:= erlvolt:erl_number(<<146,104,0,0,0,0,0,0,0,0,0,0,0,0,0,0>>)           %
%   =:= -1.4567508066305312e26                                                %
%                                                                             %
%   TODO: use/introduce an Erlang Decimal type using a Bignum lib.            %
%                                                                             %
%   header defines:                                                           %
%   -define(VOLT_DECIMAL_BINARY_TYPE(E), E:128/signed-big).                   %
%   -define(VOLT_DECIMAL_MAX,    99999999999999999999999999999999999999).     %
%   -define(VOLT_DECIMAL_MIN,   -99999999999999999999999999999999999999).     %
%   -define(VOLT_DECIMAL_NULL, -170141183460469231731687303715884105728).     %
%                                                                             %
%%%------------------------------------------------------------------decimals-%

% (needs to be defined above, as erl_integer is defined above:)
% -define(VOLT_DECIMALS_SCALESHIFT, 1000000000000).


%%%----------------------------------------------------------------------------
%%% @doc Erlang float to VoltDB binary decimals wire code.

volt_decimal(null) ->
	
	?VOLT_DECIMAL_BINARY_TYPE(?VOLT_DECIMAL_NULL);

volt_decimal(E) when (is_integer(E) or is_float(E)), E >= ?VOLT_DECIMAL_MIN,
	E =< ?VOLT_DECIMAL_MAX ->

	D = E * ?VOLT_DECIMALS_SCALESHIFT,
	T = trunc(D),
	if D /= T -> throw(loosing_precision); true -> nil end, %%% @doc will not happen.
	
	?VOLT_DECIMAL_BINARY_TYPE(T).

%%%-decode-----------------------------------------------------------decimals-

%%%----------------------------------------------------------------------------
%%% @doc VoltDB binary decimals wire code to Erlang integer, failing on NULL.

erl_number(?VOLT_DECIMAL_BINARY_TYPE(?VOLT_DECIMAL_NULL)=V) when is_binary(V) ->

	throw('this is the null value');
	
erl_number(?VOLT_DECIMAL_BINARY_TYPE(E)=V) when is_binary(V) ->

	case E rem ?VOLT_DECIMALS_SCALESHIFT of
		0 -> E div ?VOLT_DECIMALS_SCALESHIFT;
		_ -> E / ?VOLT_DECIMALS_SCALESHIFT
	end.

%%%----------------------------------------------------------------------------
%%% @doc VoltDB binary decimals wire code to Erlang integer, or null atom.

erl_number_or_null(?VOLT_DECIMAL_BINARY_TYPE(?VOLT_DECIMAL_NULL)=V) when is_binary(V) ->

	null;
	
erl_number_or_null(V) when is_binary(V) ->

	erl_number(V).
	

%%%----------------------------------------------------------------------------
%%% @doc VoltDB binary decimals wire code to Erlang float, failing on NULL.

erl_float_from_decimal(?VOLT_DECIMAL_BINARY_TYPE(E)=V) when is_binary(V) ->

	E / ?VOLT_DECIMALS_SCALESHIFT.

%%%----------------------------------------------------------------------------
%%% @doc VoltDB binary decimals wire code to Erlang float, or null atom.

erl_float_or_null_from_decimal(?VOLT_DECIMAL_BINARY_TYPE(?VOLT_DECIMAL_NULL)=V) when is_binary(V) ->

	null;
	
erl_float_or_null_from_decimal(V) when is_binary(V) ->

	erl_float_from_decimal(V).

%%%----------------------------------------------------------------------------
%%% @doc VoltDB binary decimals wire code to Erlang integer, failing on NULL.

erl_integer_from_decimal(?VOLT_DECIMAL_BINARY_TYPE(?VOLT_DECIMAL_NULL)=V) when is_binary(V) ->

	throw('this is the null value');
	
erl_integer_from_decimal(?VOLT_DECIMAL_BINARY_TYPE(E)=V) when is_binary(V) ->

	E div ?VOLT_DECIMALS_SCALESHIFT.

%%%----------------------------------------------------------------------------
%%% @doc VoltDB binary decimals wire code to Erlang integer, or null atom.

erl_integer_or_null_from_decimal(?VOLT_DECIMAL_BINARY_TYPE(?VOLT_DECIMAL_NULL)=V) when is_binary(V) ->

	null;
	
erl_integer_or_null_from_decimal(V) when is_binary(V) ->

	erl_integer(V).
	

%*****************************************************************************%
%                                   Arrays                                    % 
%*****************************************************************************%
%                                                                             %
%    Arrays are represented as Byte value indicating the  wire type  of the   %
%    elements and a  2 byte Short value  indicating the  number of elements   %
%    in the array, followed by the specified number of elements. The length   % 
%    preceding value  for the  TINYINT (byte) type  is length preceded by a   %
%    4 byte integer instead of a 2 byte short. This important exception al-   %
%    lows  large quantities of binary or string data to be passed as a byte   %
%    array.  The size of byte arrays is artificially limited to 1 megabyte.   %
%    Each array is serialized according to its type  (Strings  as  Strings,   %
%    VoltTables as VoltTables, Integers as Integers).  Arrays are only pre-   %
%    sent as parameters in parameter sets.                                    %
%                                                                             %
%    * Size is limited to 32,767 values due to the signed short length with   %
%      the exception of  TINYINT (byte)  arrays which use a  4 byte integer   %
%      length and are limited to 1 megabyte.                                  %
%                                                                             %
%    * All values must be homogeneous with respect to type.                   %
%                                                                             %
%   --- pg. 4, VoltDB Client Wire Protocol Version 0, 12/11/09 ---            %
%                                                                             %
%   TODO: 1M size limit                                                       %
%                                                                             %
%******************************************************************************

%%%-encode--------------------------------------------------------------arrays-

%%%----------------------------------------------------------------------------
%%% @doc Make Erlang List to VoltDB wire binary array, explicit type.
volt_array({ voltarray, Type, List }) ->

	volt_array(Type, List);

%%%----------------------------------------------------------------------------
%%% @doc Make Erlang List to VoltDB wire binary array, guessing the type.
volt_array({ voltarray, List }) ->

	volt_array(List);

volt_array([H|_]=List) ->

	volt_array(volt_type(H), List);

volt_array([]) ->

	volt_array(?VOLT_INTEGER, []).


%%%----------------------------------------------------------------------------
%%% @doc Make Erlang List to VoltDB wire binary array, explicit type.

volt_array(Type, { voltarray, List }) ->

	volt_array(Type, List);

volt_array(Type, List) when is_list(List)->

	L = [ volt_any(Type, X) || X <- List ],
	S = length(L),
	B = list_to_binary(L),
	<<Type:8, S:16, B/binary>>.
	 
%%%----------------------------------------------------------------------------
%%% @doc  Guessing types.

volt_type(null)                       -> ?VOLT_NULL;
volt_type(X) when is_binary(X)        -> ?VOLT_STRING;
volt_type(X) when is_integer(X)       -> ?VOLT_INTEGER;
volt_type([H|_]) when is_integer(H)   -> ?VOLT_STRING; % sic
volt_type(X) when is_list(X)          -> ?VOLT_ARRAY;  % sic
volt_type(X) when is_float(X)         -> ?VOLT_FLOAT;
volt_type({{A,B,C},{D,E,F}}) when is_integer(A), is_integer(B), 
	is_integer(C), is_integer(D), is_integer(E), is_integer(F) -> ?VOLT_TIMESTAMP.


%%%-decode--------------------------------------------------------------arrays-

%%% NOTE: Array need not be decoded in regular use.

%%%----------------------------------------------------------------------------
%%% @private not tested nor used
% % @doc EXCEPTION: TINY INT

erl_binary_array(<<?VOLT_TINYINT:8, Count:32, Stream>>=Bin) when is_binary(Bin) ->

	<<Binary:Count/binary, Rest>> = Stream,
	{ Binary, Rest }.

%%%----------------------------------------------------------------------------
%%% @private not tested nor used

erl_array(<<?VOLT_TINYINT:8, Count:32, Stream>>=Bin) when is_binary(Bin) ->

	erl_elements(?VOLT_TINYINT, Count, Stream, Stream);

%%%----------------------------------------------------------------------------
%%% @private not tested nor used
% % @doc ALL OTHER

erl_array(<<Type:8, Count:16, Stream>>=Bin) when is_binary(Bin) ->

	erl_elements(Type, Count, Stream, Stream).

%%%----------------------------------------------------------------------------
%%% @private not tested nor used

erl_elements(_, 0, _, _) ->
	
	[];

%%%----------------------------------------------------------------------------
%%% @private not tested nor used

erl_elements(Type, Left, Parse, Full) when Left > 0, Parse /= <<>> ->

	{ Element, Rest } = erl_any(Type, Parse),

	[ Element | erl_elements(Type, Left-1, Rest, Full)].


%%%----------------------------------------------------------------------------
%%% @private not tested nor used
% % @doc EXCEPTION: TINY INT
erl_binary_array_feed(<<?VOLT_TINYINT:8, Count:32, Stream>>=Bin) when is_binary(Bin) ->

	<<Binary:Count/binary, Rest>> = Stream,
	{ Binary, Rest }.

%%%----------------------------------------------------------------------------
%%% @private not tested nor used

erl_array_feed(<<?VOLT_TINYINT:8, Count:32, Stream>>=Bin) when is_binary(Bin) ->

	<<Binary:Count/binary, Rest>> = Stream,
	{ binary_to_list(Binary), Rest };

%%%----------------------------------------------------------------------------
%%% @private not tested nor used
% % @doc ALL OTHER
erl_array_feed(<<Type:8, Count:16, Stream>>=Bin) when is_binary(Bin) ->

	erl_elements_feed(Type, [], Count, Stream, Stream).

%%%----------------------------------------------------------------------------
%%% @private not tested nor used

erl_elements_feed(_, Result, 0, Rest, _) ->
	
	{ Result, Rest };


%%%----------------------------------------------------------------------------
%%% @private not tested nor used

erl_elements_feed(Type, Result, Left, Stream, Full) when Left > 0, Stream /= <<>> ->

	{ Element, Rest } = erl_any(Type, Stream),
	erl_elements_feed(Type, [ Element | Result ], Left-1, Rest, Full).


%*****************************************************************************%
%                                                                             %
%                               Type Maps                                     % 
%                                                                             %
%*****************************************************************************%

%%%-encode--------------------------------------------------------------arrays-

%% @doc Make a VoltDB wire binary from an Erlang value, w/type specified.
%% @spec volt_any(volttype(), any()) -> wire()

% TODO: volt_any(?VOLT_NULL,      Value) -> 
volt_any(?VOLT_TINYINT,   Value) -> <<Value:?VOLT_TINYINT_TYPE>>;
volt_any(?VOLT_SMALLINT,  Value) -> <<Value:?VOLT_SMALLINT_TYPE>>;
volt_any(?VOLT_INTEGER,   Value) -> <<Value:?VOLT_INTINT_TYPE>>;
volt_any(?VOLT_BIGINT,    Value) -> <<Value:?VOLT_BIGINT_TYPE>>;
volt_any(?VOLT_FLOAT,     Value) -> <<Value:?VOLT_FLOAT_TYPE>>;
volt_any(?VOLT_STRING,    Value) -> volt_string(Value);
volt_any(?VOLT_TIMESTAMP, Value) -> volt_time(Value);
volt_any(?VOLT_DECIMAL,   Value) -> volt_decimal(Value);
volt_any(?VOLT_ARRAY,     Value) -> volt_array(Value); 
volt_any(T,_) -> throw({ bad_type, T }).

%%%-decode--------------------------------------------------------------arrays-

%% @doc Make an Erlang value from any VoltDB wire type, w/type specified.
%% @spec erl_any(volttype(), binary()) -> { Value::erltype(), Rest::binary() }

% erl_any(?VOLT_ARRAY,     Stream) -> erl_array_feed     (Stream);
% TODO: erl_any(?VOLT_NULL,      Stream) -> erl_null_feed      (Stream);
erl_any(?VOLT_TINYINT,   Stream) -> erl_tinyint_feed   (Stream);
erl_any(?VOLT_SMALLINT,  Stream) -> erl_smallint_feed  (Stream);
erl_any(?VOLT_INTEGER,   Stream) -> erl_intint_feed    (Stream);
erl_any(?VOLT_BIGINT,    Stream) -> erl_bigint_feed    (Stream);
erl_any(?VOLT_FLOAT,     Stream) -> erl_float_feed     (Stream);
erl_any(?VOLT_STRING,    Stream) -> erl_string_feed    (Stream);
erl_any(?VOLT_TIMESTAMP, Stream) -> erl_timestamp_feed (Stream);
erl_any(?VOLT_DECIMAL,   Stream) -> erl_decimal_feed   (Stream);
erl_any(_,<<>>) -> { nil, [] };
erl_any(T,_) -> throw({ bad_type, T }).


%%%----------------------------------------------------------------------------
% % @doc Make an Erlang value from any VoltDB wire type.
% % @spec erl_any(wiretype() + binary()) -> { erltype(), Rest::binary() }
% TODO: "erl_null_feed     (?VOLT_NULL,            Rest/binary) -> { null, Rest }."

%%%----------------------------------------------------------------------------
%% @doc Make an Erlang integer from a TINYINT (8 bit) VoltDB wire type.
%% @spec erl_tinyint_feed(wiretype() + binary()) -> { erltype(), Rest::binary() }
erl_tinyint_feed  (<<Element:?VOLT_TINYINT_TYPE,    Rest/binary>>) -> { Element, Rest }.

%%%----------------------------------------------------------------------------
%% @doc Make an Erlang integer from a SMALLINT (16 bit) VoltDB wire type.
%% @spec erl_smallint_feed(wiretype() + binary()) -> { erltype(), Rest::binary() }
erl_smallint_feed (<<Element:?VOLT_SMALLINT_TYPE,   Rest/binary>>) -> { Element, Rest }.

%%%----------------------------------------------------------------------------
%% @doc Make an Erlang integer from an INTEGER (32 bit) VoltDB wire type.
%% @spec erl_intint_feed(wiretype() + binary()) -> { erltype(), Rest::binary() }
erl_intint_feed   (<<Element:?VOLT_INTINT_TYPE,     Rest/binary>>) -> { Element, Rest }.

%%%----------------------------------------------------------------------------
%% @doc Make an Erlang integer from a BIGINT (64 bit) VoltDB wire type.
%% @spec erl_bigint_feed(wiretype() + binary()) -> { erltype(), Rest::binary() }
erl_bigint_feed   (<<Element:?VOLT_BIGINT_TYPE,     Rest/binary>>) -> { Element, Rest }.

%%%----------------------------------------------------------------------------
%% @doc Make an Erlang float from a float VoltDB wire type.
%% @spec erl_float_feed(wiretype() + binary()) -> { erltype(), Rest::binary() }
erl_float_feed    (<<Binary:?VOLT_FLOAT_BINARY,     Rest/binary>>) -> { erl_float_or_atom(Binary), Rest }. % TODO: is '_or_atom' sensible?

%%%----------------------------------------------------------------------------
%% @doc Make an Erlang null value from a NULL string VoltDB wire type.
%% @spec erl_string_feed(wiretype() + binary()) -> { erltype(), Rest::binary() }
erl_string_feed   (<<?VOLT_STRING_NULL,             Rest/binary>>) -> { null, Rest }; 

%%%----------------------------------------------------------------------------
%% @doc Make an Erlang binary string from an empty string VoltDB wire type.
%% @spec erl_string_feed(wiretype() + binary()) -> { erltype(), Rest::binary() }
erl_string_feed   (<<?VOLT_STRING_EMPTY,            Rest/binary>>) -> { <<"">>, Rest }; 

%%%----------------------------------------------------------------------------
%% @doc Make an Erlang binary string from a string VoltDB wire type.
%% @spec erl_string_feed(wiretype() + binary()) -> { erltype(), Rest::binary() }
erl_string_feed   (<<?VOLT_STRING_BINARY(Binary),   Rest/binary>>) -> { Binary, Rest }.

%%%----------------------------------------------------------------------------
%% @doc Make an Erlang value from a timestamp VoltDB wire type.
%% @spec erl_timestamp_feed(wiretype() + binary()) -> { erltype(), Rest::binary() }
erl_timestamp_feed(<<Binary:?VOLT_TIMESTAMP_BINARY, Rest/binary>>) -> { erl_time(Binary), Rest }.

%%%----------------------------------------------------------------------------
%% @doc Make an Erlang value from a decimal VoltDB wire type.
%% @spec erl_decimal_feed(wiretype() + binary()) -> { erltype(), Rest::binary() }
erl_decimal_feed  (<<Binary:?VOLT_DECIMAL_BINARY,   Rest/binary>>) -> { erl_number_or_null(Binary), Rest }. % TODO: is '_or_null' sensible?

% for erl_array_feed see above
% for erl_table_feed see below


%*****************************************************************************%
%                                                                             %
%                               VoltTables                                    % 
%                                                                             %
%*****************************************************************************%
%                                                                             %
%    VoltDB: On the wire a VoltTable is serialized as a header followed by    %
%    tuple data.  VoltTables,  like all  VoltDB serialized structures  are    %
%    stored in network byte order.*                                           %
%                                                                             %
%    Header Format:                                                           %
%                                                                             %
%    ---------------------------------------------------------------------    %
%    + Total table length:    Integer 4                                  |    %
%    + Table Metadata Length: Integer 4                                  |    %
%    + Status Code:           Byte 1                                     |    %
%    + Column Count:          Short 2                                    |    %
%    + Column Types:          Array** of Bytes                           |    %
%    + Column Names:          Array** of Strings                         |    %
%    + Row Count:             Integer 4                                  |    %
%    --------------------------------------------------------------------+    %
%    |                    ... row data...                                |    %
%    ---------------------------------------------------------------------    %
%                                                                             %
%    Notes on the Header Format:                                              %
%    * The "Table Metadata Length"  stores the  length in bytes of the con-   %
%      tents of the table from byte 8(the end of the metadata length field)   %
%      all the way to the end of the "Column Names" array. NOTE:It does not   %
%      include the row count value. See below for an example.                 %
%    * The size of the "Column Types" and "Column Names" arrays** is expec-   %
%      ted to equal the value stored in "Column Count".                       %
%    * Column names are limited to the ASCII character set.  Strings in row   %
%      values are still UTF-8 encoded. (TODO: clarify Erlang / UTF-16)        % 
%    * Values with 4-byte (integer) length fields  are signed and are limi-   %
%      ted to a max of 1 megabyte.                                            %
%                                                                             %
%    Row Data Format:                                                         %
%                                                                             %
%    Each row is prefixed by a 4 byte integer that holds the non-inclusive    %
%    length of the row.  If a row is a  single 4-byte integer column,  the    %
%    value of this  length prefix will be  4.  Row size is artifically re-    %
%    stricted to 2 megabytes.  The body of the row is packed array of val-    %
%    ues.  The value at index i is is of type specified by the column type    %
%    field for index i.The values are serialized according to the seriali-    %
%    zation rules in "Basic Data Types" above.                                %
%                                                                             %
%    ---------------------------------------------------------------------    %
%    + Row Length:            Integer 4                                  |    %
%    ---------------------------------------------------------------------    %
%    |                ... single row value array** ...                   |    %
%    ---------------------------------------------------------------------    %
%                                                                             %
%    --- pg. 4+5, VoltDB Client Wire Protocol Version 0, 05/05/10 ---         %
%                                                                             %
%    *) big-endian                                                            %
%    **) NOT VoltDB wire protocol Arrays as described above. Check sample.    %
%                                                                             %
%******************************************************************************
%                                                                             %
%    Erlang:  There are four  types of binary objects  internally.  Two of    %
%    them are containers for binary data and two of them are merely refer-    %
%    ences to a part of a binary.                                             %
%                                                                             %
%    The binary containers are called refc binaries  (short for reference-    %
%    counted binaries) and heap binaries.                                     %
%                                                                             %
%    Refc binaries  consist of two parts:  an object stored on the process    %
%    heap,  called a ProcBin,  and the binary object itself stored outside    %
%    all process heaps.                                                       %
%                                                                             %
%    The  binary object  can be referenced by any number of  ProcBins from    %
%    any number of processes;  the object contains  a reference counter to    %
%    keep track  of the number  of  references,  so that it can be removed    %
%    when the last reference disappears.  All ProcBin objects in a process    %
%    are part of a  linked list,  so that the  garbage collector  can keep    %
%    track of them and decrement the reference counters in the binary when    %
%    a ProcBin disappears. Heap binaries are small binaries, up to 64bytes,   %
%    that are stored directly on the process heap.They will be copied when    %
%    the process is garbage collected and when they are sent as a message.    %
%    They don't require any special handling by the garbage collector.        %
%                                                                             %
%    There are  two types of reference objects that can  reference part of    %
%    a refc binary or heap binary.  They are called sub binaries and match    %
%    contexts.                                                                %
%                                                                             %
%    A sub binary is created by split_binary/2  & when a binary is matched    %
%    out in a binary pattern.  A  sub binary is a reference into a part of    %
%    another binary (refc or heap binary, never into a another sub binary).   %
%    Therefore,  matching out a binary is *relatively* *cheap* because the    % 
%    actual binary data is *never* *copied*.                                  %
%                                                                             %
%    A  match context  is similar to a  sub binary,  but is  optimized for    %
%    binary matching;  for instance,  it contains  a direct pointer to the    %
%    binary data.  For each field that is matched out of a binary, the po-    %
%    sition in the match context will be incremented.  [...]  the compiler    %
%    tries  to avoid  generating code  that creates a sub binary,  only to    %
%    shortly afterwards  create  a new match context  and discard the  sub    %
%    binary.  Instead of creating a sub binary, the match context is kept.    %
%                                                                             %
%    The compiler  can only do this optimization  if it can know  for sure    %
%    that the match context will not be shared. If it would be shared, the    %
%    functional  properties  (also  called  referential  transparency)  of    %
%    Erlang would break.                                                      %
%                                                                             %
%    --- pg. 168 Erlang/OTP System Documentation 5.7.5 02/22/10 ---           %
%                                                                             %
%******************************************************************************

%%% @type  volttable()    = { volttable, rows(), column_names(), column_types() }.
%%% @type  rows()         = [row()].
%%% @type  row()          = [field()].
%%% @type  column_names() = [name()].
%%% @type  column_type()  = [volttype()].
%%% @type  volttype()     = [integer()].
%%% @type  field()        = [string() | integer() | float() | null ].
%%% @type  name()         = [atom()].
%%%
%%% @type  erltype()      = [ integer() | float() | list() | binary() ]. Target type of erl_* functions.
%%%


%%%-encode-----------------------------------------------------------volttable-

%%%----------------------------------------------------------------------------
%%% @doc Encode Erlang terms to VoltTable rows, with status code 0.

volt_table({ volttable, ColumnNames, ColumnTypes, Rows }) ->
	
	volt_table({ volttable, ColumnNames, ColumnTypes, Rows }, 0).

%%%----------------------------------------------------------------------------
%%% @doc Encode Erlang terms to VoltTable rows, with explicit status code.

volt_table({ volttable, ColumnNames, ColumnTypes, Rows }, StatusCode)

	when is_list(ColumnNames), is_list(ColumnTypes), is_list(Rows), 
	     StatusCode >= -128, StatusCode =< 127 ->

	ColumnCount = length(ColumnNames),
	ColumnCount = length(ColumnTypes),
	RowCount    = length(Rows),
	
	ColumnTypesBin = list_to_binary(ColumnTypes),
	
	ColumnNamesBin = << <<(size(Name)):?VOLT_STRING_SIZE_BINARY_TYPE, Name/binary>> || Name <- ColumnNames >>,

	MetaBin = <<StatusCode:8, ColumnCount:16, ColumnTypesBin/binary, ColumnNamesBin/binary>>,
	
	RowsBin = list_to_binary(volt_rows(ColumnTypes, Rows)),
	
	MetaSize  = size(MetaBin),                     % From byte #8 to excl RowCount
	TotalSize = 4 + MetaSize + 4 + size(RowsBin),  % From byte #4 (sic) to end. 
												   % First 4 for MetaSize Value itself.
	<< TotalSize:32, 
	   MetaSize:32, 
	   MetaBin/binary, 
	   RowCount:32,
	   RowsBin/binary >>.

%%%----------------------------------------------------------------------------
%%% @doc Encode Erlang terms to VoltTable rows.
	   
volt_rows(_, []) -> [];

volt_rows(ColumnTypes, [ { voltrow, Fields } | Rows ]) ->

	Row = list_to_binary(volt_fields(ColumnTypes, Fields)),
	Size = size(Row),
	[ <<Size:32, Row/binary>>  | volt_rows(ColumnTypes, Rows) ].


%%%----------------------------------------------------------------------------
%% @doc Converting two lists (Types, Contents) into a list of voltdb wire encoded binaries.
%% E.g.  [?VOLT_STRING , ?VOLT_STRING], ["Test", "Test"] ->  `[<<4:32,"Test">>,<<4:32,"Test">>]'
%% This returns a list of binaries. Converting it to one binary in the end is thought to be faster.	

volt_fields([], []) -> [];

volt_fields([Type | Types], [Value | Values]) ->

	% over defense
	try true = length(Types) == length(Values) catch error:{badmatch,_} -> throw(parameter_list_length_mismatch) end,

	[ volt_any(Type, Value) | volt_fields(Types, Values)].



%%%-decode-----------------------------------------------------------volttable-

%%%----------------------------------------------------------------------------
%%% @doc   Parse a VoltTable from VoltDB wire protocol data. The Table is 
%%%        translated into an Erlang volttable() structure. ```
%%%
%%%        { volttable, 
%%%						[ColumnName, ColumnName, ...]
%%%						[ColumnType, ColumnType, ...]
%%%						[ { voltrow, [Value, Value, ...] },
%%%						  { voltrow, [Value, Value, ...] },
%%%						  ...
%%%						]
%%%		   }
%%%
%%% E.g.: 
%%%
%%%	       { volttable, [<<"TestCol1">>,<<"TestCol2">>,<<"TestCol3">>], 
%%%						[?VOLT_BIGINT,?VOLT_BIGINT,?VOLT_STRING], 
%%%
%%%						[ { voltrow, [42, 7, <<"volt">>] },
%%%						  { voltrow, [28,13, <<"vorlt">>] },
%%%						  { voltrow, [11, 1, <<"world">>] } ] },                         '''
%%%
%%% TODO: use above as test.

%%% @spec  erl_table(binary()) -> volttable() 
	
erl_table(<<Length:32, MetaLength:32, _Status:8, ColumnCount:16, Stream/binary>>=Bin) 
	when Length > 0, is_binary(Bin) ->

	% TODO: conditional, defensive switch for the following block
	
	% assert length of binary
	try	Length = size(Bin) - 4
	catch
		What:Why ->	throw({maybe_bad_total_size, Length + 4, size(Bin), What, Why})
	end,
	
	%% Calculate byte count of column names
	ColumnNamesSpace = MetaLength - 3 - ColumnCount,
	
	%% Scan Rest of Meta Data 
	<<ColumnTypeBinaries:ColumnCount/binary, 
	  ColumnNamesBinary:ColumnNamesSpace/binary, 
	  RowCount:32,
	  RowsBinary/binary>> = Stream,
	
	%% Make List of Column Types (integers)
	ColumnTypes = binary_to_list(ColumnTypeBinaries),
	try ColumnCount = length(ColumnTypes) catch error:{badmatch,_} -> throw({bad_columncount_with_types, ColumnCount, length(ColumnTypes)}) end,

	%% Make List of Column Names
	ColumnNames = [ Name || <<Size:32, Name:Size/binary>> <= ColumnNamesBinary ],
	try ColumnCount = length(ColumnNames) catch error:{badmatch,_} -> throw({bad_columncount_with_names, ColumnCount, length(ColumnNames)}) end,
	% TODO: cache this globally
	
	%% Scan Rows
	RowBinaries = [ RowBinary || <<Size:32, RowBinary:Size/binary>> <= RowsBinary ],
	try RowCount = length(RowBinaries) catch error:{badmatch,_} -> throw({bad_rowcount, 'could also be too small row length value', RowCount, length(RowBinaries), RowBinaries, RowsBinary}) end,

	%% Scan Rows
	Rows = try erl_table_rows(ColumnTypes, RowBinaries) 
	       catch error:E -> throw({row_parse_failure,E}) 
	       end,

	{ volttable, ColumnNames, ColumnTypes, Rows }.


erl_table_feed(<<Length:32, _/binary>>=Bin) when Length > 0, is_binary(Bin) ->

	UseLength = Length + 4,
	<<UseBin:UseLength/binary, Rest/binary>> = Bin,

	% debug io:format("erl_table_feed ~n~w~n~w", [UseBin, Rest]),

	{ erl_table(UseBin), Rest }.

	% TODO: binary optimization may work better when not putting this in own function body?


%%% @doc   Parse a VoltTable from VoltDB wire protocol data, return a once-nested list. 
%%% @spec  erl_plaintable(binary()) -> volttable() 
	
erl_plaintable(<<Length:32, MetaLength:32, _Status:8, ColumnCount:16, Stream/binary>>=Bin) 
	when Length > 0, is_binary(Bin) ->

	% TODO: conditional, defensive switch for the following block
	
	% assert length of binary
	try	Length = size(Bin) - 4
	catch
		What:Why ->	throw({maybe_bad_total_size, Length + 4, size(Bin), What, Why})
	end,
	
	%% Calculate byte count of column names
	ColumnNamesSpace = MetaLength - 3 - ColumnCount,
	
	%% Scan Rest of Meta Data 
	<<ColumnTypeBinaries:ColumnCount/binary, 
	  _:ColumnNamesSpace/binary, 
	  RowCount:32,
	  RowsBinary/binary>> = Stream,
	
	%% Make List of Column Types (integers)
	ColumnTypes = binary_to_list(ColumnTypeBinaries),
	try ColumnCount = length(ColumnTypes) catch error:{badmatch,_} -> throw({bad_columncount_with_types, ColumnCount, length(ColumnTypes)}) end,

	%% Scan Rows
	RowBinaries = [ RowBinary || <<Size:32, RowBinary:Size/binary>> <= RowsBinary ],
	try RowCount = length(RowBinaries) catch error:{badmatch,_} -> throw({bad_rowcount, RowCount, length(RowBinaries)}) end,
	
	%% Scan Rows
	Rows = try erl_plaintable_rows(ColumnTypes, RowBinaries) 
	       catch error:E -> throw({row_parse_failure,E}) 
	       end,
	
	Rows.
	
erl_table_rows(_, []) ->

	[];

erl_table_rows(Types, [ RowBinary | RowBinaryTail ]) ->

	[ { voltrow, 
		try
		 % debug: io:format("~n~p~n", [RowBinary]),
		 erl_table_fields(Types, RowBinary) 
		catch error:E -> throw({field_parse_failure, E, erlang:get_stacktrace()})  
		end
	 } |
		
		erl_table_rows(Types, RowBinaryTail ) ].

erl_plaintable_rows(_, []) ->

	[];

erl_plaintable_rows(Types, [ RowBinary | RowBinaryTail ]) ->

	[ try
		 % debug: io:format("~n~p~n", [RowBinary]),
		 erl_table_fields(Types, RowBinary) 
		catch error:E -> throw({field_parse_failure, E, erlang:get_stacktrace()})  
		end
	  |
		
	  erl_plaintable_rows(Types, RowBinaryTail ) ].

 
erl_table_fields([], <<>>) ->

	[];

erl_table_fields([ Type | TypesTail ], RowBinaryStream ) ->

	% debug: io:format("~n~p~n", [RowBinaryStream]),

	{ Value, RowBinaryRest } = erl_any(Type, RowBinaryStream),

	[ Value | erl_table_fields(TypesTail, RowBinaryRest ) ].


%*****************************************************************************%
%                           Table Access Functions                            % 
%*****************************************************************************%
%%%----------------------------------------------------------------------------
%%% @doc Get a row out of a given table, by index number. First == 1.
%%% @spec fetchRow(volttable(), Pos) -> voltrow()

fetchRow({ volttable, _, _, List}, Pos) when is_list(List) ->

	lists:nth(Pos, List).

%%%----------------------------------------------------------------------------
%%% @doc Get a row out of a given table as record, by index number. First == 1.
%%% @spec fetchRecord(volttable(), RecordTag, Pos) -> record()

fetchRecord({ volttable, _, _, List}, RecordTag, Pos) when is_list(List) ->

	{ voltrow, FieldList } = lists:nth(Pos, List),
	list_to_tuple([ RecordTag | FieldList ]).


%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row, by index number. First == 1.
%%% @spec getField(voltrow(), Pos) -> term()

getField({ voltrow, List }, Pos) ->

	lists:nth(Pos, List).


%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as string, by index number. First == 1.
%%% @spec getString(voltrow(), Pos) -> binary()
%%% TODO: is that true?

getString({ voltrow, _ }=VoltRow, Pos) when is_integer(Pos), Pos < 1 ->

	throw({out_of_range, Pos, lt_one, VoltRow });

getString({ voltrow, List }, Pos) when is_integer(Pos), is_list(List), Pos > length(List) ->

	throw({out_of_range, Pos, length(List), List});

getString({ voltrow, List }, Pos) when is_integer(Pos), is_list(List) ->

	vecho(?V, "getString( { voltrow, ~w }, ~w )", [List, Pos]),

	to_list(lists:nth(Pos, List));

	% TODO: make this to_binary

% Note: RecPos can be expected to be = Pos+1, as it observes the preceding tag atom.
% TODO: write tests for this variant

getString(Record, RecPos) when is_integer(RecPos), is_tuple(Record), RecPos < 2 ->

	throw({out_of_range, RecPos, lt_two, Record });

getString(Record, RecPos) when is_integer(RecPos), is_tuple(Record), RecPos > tuple_size(Record) ->

	throw({out_of_range, RecPos, tuple_size(Record) -1, Record});

getString(Record, RecPos) when is_integer(RecPos), is_tuple(Record) ->

	vecho(?V, "getString( ~w, ~w )", [Record, RecPos]),

	to_list(element(RecPos, Record)).
	
	% TODO: make this to_binary

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as string, by column name. 
%%% The complete VoltTable is used to exract column names from it.
%%% @spec getString(voltrow(), volttable(), Pos) -> binary()

getString(VoltRow, VoltTable, Name) when is_list(Name) ->
	
	getString(VoltRow, VoltTable, list_to_binary(Name));

getString({ voltrow, _ }=VoltRow, VoltTable, Name) when is_binary(Name)->

	{ volttable, ColumnNames, _ColumnTypes, _Rows } = VoltTable,
	getString(VoltRow, listOrd(Name, ColumnNames)).
	
% TODO: tests
	
%%%----------------------------------------------------------------------------
%%% @doc Return index number of a given list element.   


listOrd(_, []) -> nil;

listOrd(Searched, List) when is_list(List) ->

	listOrd(Searched, List, 1).

listOrd(_, [], _) -> nil;

listOrd(Searched, [ Element | Tail ], Count) ->
	
	case Searched == Element of
		true -> Count;
		_ -> listOrd(Searched, Tail, Count + 1)
	end.

%%%----------------------------------------------------------------------------
%%% @ doc   


to_list(L) when is_list(L) -> L;
to_list(L) when is_binary(L) -> binary_to_list(L);
to_list(L) when is_integer(L) -> integer_to_list(L);
to_list(L) when is_float(L) -> float_to_list(L).

% TODO: tests


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                        SERVER-CLIENT COMMUNICATION                          % 
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%*****************************************************************************%
%                                                                             %
%                           Establish Connection                              % 
%                                                                             %
%*****************************************************************************%

%%%----------------------------------------------------------------------------
%%% @doc  Client opens connection and logs in to the VoltDB server cluster.
%%% Use defaults: localhost, port 21212, client name "program" and password "password". 
%%% Login name and password can be irrelevant, check the VoltDB docs. 

-define(PORT, 21212).

createConnection() ->

	createConnection("localhost", ?PORT, "program", "password").

%%%----------------------------------------------------------------------------
%%% @doc  Client opens connection and logs in to the VoltDB server cluster.
%%% Use specified host and defaults: port 21212, client name "program" and password "password". 
%%% Login name and password can be irrelevant, check the VoltDB docs. 

createConnection(Host) ->

	createConnection(Host, ?PORT, "program", "password").

%%%----------------------------------------------------------------------------
%%% @doc  Client opens connection and logs in to the VoltDB server cluster.
%%% Use specified host, login name and password, default port 21212. 
%%% Login name and password can be irrelevant, check the VoltDB docs. 

createConnection(Host, Login, Password) ->

	createConnection(Host, ?PORT, Login, Password).

%%%----------------------------------------------------------------------------
%%% @doc  Client opens connection and logs in to the VoltDB server cluster.
%%% Specify host, port, login name and password.
%%% Login name and password can be irrelevant, check the VoltDB docs. 

createConnection(Host, Port, Login, Password) ->

	open(Host, Port, Login, Password).

%%%----------------------------------------------------------------------------
%%% @private  Workhorse: Client opens connection and logs in to the VoltDB server cluster.
%%% Login name and password can be irrelevant, check the VoltDB docs. 

open(Host, Port, Login, Password) ->

	try
	    Socket = connect(Host, Port),
    
    	{ ok, _ } = login(Socket, Login, Password),

    	Socket

    catch
    
    	Type:Reason -> throw({open_failed, Type, Reason})
    
    end.

%%%----------------------------------------------------------------------------
%%% @private  Workhorse: Client opens connection to the VoltDB server cluster.

connect(Host, Port) ->

    {ok, Socket} = gen_tcp:connect(Host, Port, [binary, {packet, 4}]),

	Socket.
	

%*****************************************************************************%
%                                                                             %
%                             Message Header                                  % 
%                                                                             %
%*****************************************************************************%
%
%    @doc VoltDB message header bytes - actually protocol version only.	    ```
%
%    ---------------------------------------------------------------------    
%    + Message Length:        Integer 4                                  |    
%    + Protocol Version:      Byte    1                                  |    
%    ---------------------------------------------------------------------    
%                                                                           '''
%    The wire protocol header is included at the beginning of all messages.
%    The length value includes the protocol version byte but not the 4 byte
%    length value.
%                                                                             
%    --- pg. 8+9, VoltDB Client Wire Protocol Version 0, 05/05/10 ---         
%                                                                             
%    For Erlang,  the 4 leading length bytes  are effected by the flags of
%    `[binary, {packet, 4}]'  to the `gen_tcp:connect call'.  Therefore,  
%    only the lone protocol version byte needs adding. (***)
%
%    TODO: should that be changed to have more better control over errors?
%
%    @end
%
%******************************************************************************
	
%%%----------------------------------------------------------------------------
%%% @spec volt_header() -> wire()
	
volt_header() ->

	?VOLT_PROTOCOL_VERSION_BINARY.
	
%%%----------------------------------------------------------------------------
%%% @doc Parse wire protocol header.
%%% Erlang reads 4 length value bytes away by itself. See volt_header/0 (***)
%%% @spec erl_header(binary()) -> { ProtocolVersion::integer(), Size::integer() }
%%% | exception({ protocol_error, is, should })
	
erl_header(W) ->

	<<Protocol:?VOLT_PROTOCOL_VERSION_TYPE, _/binary>> = W,
	case Protocol of
		?VOLT_PROTOCOL_VERSION -> ok;
		_ -> throw({ protocol_error, Protocol, ?VOLT_PROTOCOL_VERSION })
	end,
	
	{ ?VOLT_PROTOCOL_VERSION, size(W) }.
	
%*****************************************************************************%
%                                                                             %
%                                 Login                                       % 
%                                                                             %
%*****************************************************************************%
%                                                                             %
%    @doc Client log in to the VoltDB server cluster, blocking.
%
%    The login message is the first message a client can send to a server
%    after opening a connection. A client does not need to wait for a res-
%    ponse to the login message to begin sending invocation requests. ```
%
%    --------------------------------------------------------------------+    
%    |                    ... Message Header ...                         |    
%    ---------------------------------------------------------------------    
%    + "database"             String                                     |    
%    + User name:             String                                     |    
%    + SHA-1 password hash:   Binary 20                                  |    
%    ---------------------------------------------------------------------    
%                                                                      '''        
%    --- pg. 8+9, VoltDB Client Wire Protocol Version 0, 05/05/10 ---         
%                                                                             
%    @end                                                                             
%                                                                             %                         
%******************************************************************************


login(Socket, Name, Password) ->

	login(Socket, Name, Password, blocking).
	
	
%%%----------------------------------------------------------------------------
%%% @doc Client log in to the VoltDB server cluster, blocking or non-blocking.
%%% @spec login(Socket, Name, Password, blocking | atom() ) -> LoginResponse::term() | { ok, no_response_since_async } 


login(Socket, Name, Password, Mode) ->

    gen_tcp:send(Socket, L=volt_login(Name, Password)),
    vecho(?V, "Login: ~w", [L]),
    
	case Mode of
	
		blocking ->
			
			receive
		
				{tcp,Socket,ResultBin} ->
					
					erl_login_response(ResultBin);
		
				{tcp_closed,_}=D -> 
					
					erlang:error({ server_denied, D });

				Else -> 
					
					erlang:error({ unexpected_response_format, Else })
			end;

		_ -> { ok, no_response_since_async }
		
	end.


%%%----------------------------------------------------------------------------
%%% @doc  Make VoltDB wire binary for login message from name and password.

volt_login(Name, Password) ->
	
	HeaderBin   = volt_header(),          % all return binaries
	DatabaseBin = volt_string("database"),
	NameBin     = volt_string(Name),
	HashBin     = volt_hash(Password),

	<<HeaderBin/binary, DatabaseBin/binary, NameBin/binary, HashBin/binary>>.

	
%%%----------------------------------------------------------------------------
%%% @doc  Make SHA-1 hash for client log in to the VoltDB server cluster.

volt_hash(Secret) when is_list(Secret) ->

	volt_hash(list_to_binary(Secret));

volt_hash(Secret) when is_binary(Secret) ->

	crypto:start(),
	crypto:sha(Secret).


%%%

erl_login_response(Bin) ->

	{ Protocol, _ } = erl_header(Bin), 
	
	<<_:?VOLT_HEADER_TYPE, 
	  AuthCode:8,
	  HostID:32,
	  ConnectionID:64,
	  ClusterStart:64,
	  LeaderIP:32,
	  BuildStringSize:32,
	  BuildString:BuildStringSize/binary>> = Bin,

	io:format("Logged in. Server buildstring: ~s", [BuildString]),
	
	{ ok, { Protocol, AuthCode, HostID, ConnectionID, ClusterStart, LeaderIP, BuildString }}.
	

%*****************************************************************************%
%                                                                             %
%                         Invoke Stored Procedures                            % 
%                                                                             %
%*****************************************************************************%
%	                                                                          %
%	 A request to invoke a stored procedure identifies the procedure to in-   %
%	 voke by name, the parameters to pass to the procedure,  and an 8 byte    %
%	 piece of client data  that will be returned  with the response to the    %
%	 invocation request.  A client does not need to wait for a response to    %
%	 a request to continue sending requests. The server will use TCP back-    %
%	 pressure to avoid running out of memory  when a client sends too many    %
%	 invocations for the server to handle.                                    %
%	                                                                          %
%    --------------------------------------------------------------------+    %
%    |                    ... Message Header ...                         |    %
%    ---------------------------------------------------------------------    %
%    + Procedure name:        String                                     |    %
%    + Client data:           Binary                                     |    %
%    + Parameters:            Parameter Set                              |    %
%    ---------------------------------------------------------------------    %
%	                                                                          %
%    --- pg. 13, VoltDB Client Wire Protocol Version 0, 05/05/10 ---          %
%	                                                                          %
%	                                                                          %
%*****************************************************************************%

%%%----------------------------------------------------------------------------
%%% @doc  Send a stored procedure call to the VoltDB server. 
%%% Use a default client tag and default time out.

-define(TIMEOUT, 1000).
-define(DEFAULT_CLIENT_TAG, <<1:(8*8)>>).

callProcedure(Socket, ProcedureName, Parameters ) ->
	callProcedure(Socket, ProcedureName, Parameters, ?DEFAULT_CLIENT_TAG, ?TIMEOUT).

%%%----------------------------------------------------------------------------
%%% @doc  Send a stored procedure call to the VoltDB server. 
%%% Use a specified client tag and default time out.

callProcedure(Socket, ProcedureName, Parameters, ClientTag ) ->
	callProcedure(Socket, ProcedureName, Parameters, ClientTag, ?TIMEOUT).

%%%----------------------------------------------------------------------------
%%% @doc  Send a stored procedure call to the VoltDB server. 
%%% Use a specified client tag and a specified time out.

callProcedure(Socket, ProcedureName, Parameters, ClientData, TimeOut) ->

    gen_tcp:send(Socket, volt_invoke(ProcedureName, Parameters, ClientData)),
    
	receive
	
		{tcp,Socket,ResultBin} ->
		
			erl_response(ResultBin);

		Else -> erlang:error({ unexpected_response_format, Else })
		
	after TimeOut ->
		
		erlang:throw({receive_time_out, ProcedureName, Parameters})
	
	end.
    

%%%----------------------------------------------------------------------------
%%% @doc  Make VoltDB wire binary for login message from name and password.
	
volt_invoke(ProcedureName, Parameters, ClientData) when is_binary(ClientData) ->

	H  = volt_header(),           % all return binaries
	N  = volt_string(ProcedureName),
	P  = volt_parameters(Parameters),
	
	Bin = <<H/binary, N/binary, ClientData/binary, P/binary>>,
	
	vecho(?V, "~nSend invoke: ~w~n", [Bin]),

	Bin.


%*****************************************************************************%
%                                                                             %
%                             Client Parameters                               % 
%                                                                             %
%*****************************************************************************%
%                                                                             %
%	 A  parameter set  contains all the parameters to be passed to a stored   %
%	 procedure and it is one of the structures bundled inside a stored pro-   %
%	 cedure invocation request.  The  first value of a  parameter set  is a   %
%	 Short indicating  the number of parameters that follow.  The following   %
%	 values are  a series of  <wire type, value> pairs.  Each value is pre-   %
%	 ceded by its  wire type represented as  a Byte.  NULL is a  valid wire   %
%	 type and value and it is not followed by any additional value.           %
%	 	                                                                      %
%	 Arrays are preceded by the wire type -99  and the array value contains   %
%	 the type of the array elements as well as the number of elements  (see   %
%	 Array type).  A parameter set  cannot  contain  a nested parameter set   %
%	 (there is no wire type for parameter set).                               %
%	                                                                          %
%	                                                                          %
%	 Parameter Set	                                                          %
%	                                                                          %
%    ---------------------------------------------------------------------    %
%    + Parameter Count:       Short     2                                |    %
%    --------------------------------------------------------------------+    %
%    |                    ... Parameters ...                             |    %
%    ---------------------------------------------------------------------    %
%	                                                                          %
%	                                                                          %
%	 Parameter                                                                %            
%                                                                             %
%    ---------------------------------------------------------------------    %
%    + Parameter Type:       Byte       1                                |    %
%    --------------------------------------------------------------------+    %
%    + Parameter [Value]:    Binary                                      |    %
%    --------------------------------------------------------------------+    %
%	                                                                          %
%    --- pg. 7, VoltDB Client Wire Protocol Version 0, 05/05/10 ---           %
%	                                                                          %
%	                                                                          %
%*****************************************************************************%

%%%----------------------------------------------------------------------------
%%% @doc  Encode parameters for Stored Procedure Calls

volt_parameters({ voltparameters, Parameters }) when is_list(Parameters) ->

	volt_parameters(Parameters);

volt_parameters(Parameters) when is_list(Parameters) ->

	List = [ volt_parameter(Parameter) || Parameter <- Parameters ],
	Count = length(List),
	list_to_binary([<<Count:16>> | List ]).

%%%----------------------------------------------------------------------------
%%% @doc  Encode, with type, one parameter for Stored Procedure Calls

volt_parameter({ Type, Value }) ->

	V = volt_any(Type, Value),
	
	<<Type:8, V/binary>>;

volt_parameter(Value) ->

	volt_parameter({?VOLT_STRING, Value}).



%*****************************************************************************%
%                                                                             %
%                         Decode Invocation Result                            % 
%                                                                             %
%*****************************************************************************%
%	                                                                          %
%	An invocation response contains the results  of the server's attempt to   %
%	execute the stored procedure. The response includes optional fields and   %
%	the  first byte  after the header  is used to indicate,  which optional   %
%	fields are present.  The status string,  application status string, and   %
%	serializable exception  are all  optional fields.  Bit 7  indicates the   %
%	presence of a serializable exception, bit 6 indicates the presence of a   %
%	status string, and bit 8 indicates the presence of an app status string   % 
%	The  serializable exception  that can be included  in some responses is   %
%   currently  not a part of the wire protocol.  The exception length value   %
%   should  be used  to skip  exceptions  if  they are present.  The status   %
%   string is used to return any  human readable information  the server or   %
%	stored procedure wants to return with the response. The app status code   %
%	and app status string can be set by application code from within stored   %
%	procedures and is returned with the response.                             %
%	                                                                          %
%    --- pg. 15, VoltDB Client Wire Protocol Version 0, 05/05/10 ---          %
%	                                                                          %
%	                                                                          %
%   .. there is a new 4 byte latency field in the client response message     %
%   after the status byte. This field measures the roundtrip latency mea-     %
%   sured by the initiating node in the cluster measuring when the trans-     %
%   action was initiated and when the response was received and forwarded     %
%   to the client.                                                            %
%	                                                                          %
%    --- Ariel Weisberg, VoltDB, email 04/28/10                               %
%                                                                             %
%                                                                             %
%   ClientResponseImpl.java Revision 495 of 05/24/10:                         %
%                                                                             %
%	181  public void writeExternal(FastSerializer out) throws IOException {   %
%	182	        assert setProperly;                                           %
%	183	        out.writeByte(0);//version                                    %
%	184	        out.writeLong(clientHandle);                                  %
%	185	        byte presentFields = 0;                                       %
%	186	        if (appStatusString != null) {                                %
%	187	            presentFields |= 1 << 7;                                  %
%	188	        }                                                             %
%	189	        if (m_exception != null) {                                    %
%	190	            presentFields |= 1 << 6;                                  %
%	191	        }                                                             %
%	192	        if (statusString != null) {                                   %
%	193	            presentFields |= 1 << 5;                                  %
%	194	        }                                                             %
%	195	        out.writeByte(presentFields);                                 %
%	196	        out.write(status);                                            %
%	197	        if (statusString != null) {                                   %
%	198	            out.writeString(statusString);                            %
%	199	        }                                                             %
%	200	        out.write(appStatus);                                         %
%	201	        if (appStatusString != null) {                                %
%	202	            out.writeString(appStatusString);                         %
%	203	        }                                                             %
%	204	        out.writeInt(clusterRoundTripTime);                           %
%	205	        if (m_exception != null) {                                    %
%	206	            final ByteBuffer b =                                      %
%   206		          ByteBuffer.allocate(m_exception.getSerializedSize());   %
%	207	            m_exception.serializeToBuffer(b);                         %
%	208	            out.write(b.array());                                     %
%	209	        }                                                             %
%	210	        out.writeArray(results);                                      % 
%	211	 }                                                                    %
%                                                                             %
%   --- https://source.voltdb.com/browse/Engineering/trunk                    %
%       /src/frontend/org/voltdb/ClientResponseImpl.java?r=495#l184 ---       %
%                                                                             %
%                                                                             %
%    Response                                                                 %
%                                                                             %
%    --------------------------------------------------------------------+    %
%    |                      ... Message Header ...                       |    %
%    ---------------------------------------------------------------------    %
%    + Client data            Binary                     8               |    %
%    + Fields present         Byte (bit field)           1               |    %
%    + Status                 Byte                       1               |    %
%    o Status string          String (opt field)         variable        |    %
%    + Appl Status            Byte                       1               |    %
%    o Appl Status String     String (opt field)         variable        |    %
%    + Roundtrip Time         Integer                    4               |    %
%    o Serialzd Exc Length    Integer (opt field)        4               |    %
%    o Serialzd Exc           Serialzd Exc (opt field)   variable        |    %
%    + Result count           Short                      2               |    %
%    + Result tables          Series of VoltTables       variable        |    %
%    ---------------------------------------------------------------------    %
%                                                                             %
%                                                                             %
%    'Fields-Present' Bit Field  [ 8 7 6 5 4 3 2 1 ]:                         %
%                                                                             %
%    --------------------------------------------------------------------+    %
%    +  Bit 8                 App Status string present                  |    %
%    +      7                 Serialzd Exception present                 |    %
%    +      6                 Status string present                      |    %
%    +      1..5              not used                                   |    %
%    ---------------------------------------------------------------------    %
%                                                                             %
%                                                                             %
%*****************************************************************************%
%%%
%%% @type voltresponse() = 
%%% 
%%% 	{ voltresponse,
%%% 	  
%%% 	  {
%%% 	  	Protocol, 
%%% 		ClientData, 
%%% 		Status, 
%%% 		StatusString, 
%%% 		AppStatus, 
%%% 		AppStatusString, 
%%% 		SerializedException,
%%%     	RoundTripTime
%%%       },
%%%       
%%%       [ volttable() ]
%%%    
%%% 	}.
%%% @end
%%%----------------------------------------------------------------------------
%%% @doc Parse VoltDB server response  to stored procedure invocation.
%%% @spec erl_response(wire()) -> voltresponse()


erl_response(W) -> 

	vecho(?V, "~nResponse received: ~w~n", [W]),

	{ Protocol, _ } = erl_header(W), 
	
	<<_:?VOLT_HEADER_TYPE, AfterHeader/binary>> = W, % TODO: clumsy.

	<<ClientData:8/binary, 
	  HasAppStatusString:1/integer,
	  HasSerializedException:1/integer,
	  HasStatusString:1/integer,
	  _:5/integer,
	  Status:8,
	  AfterStatusByte/binary>> = AfterHeader,

	% optional status string
	{ StatusString, AfterStatusString } = erl_cond_str(HasStatusString, AfterStatusByte),

	% app status
	<<AppStatus:8, AfterAppStatus/binary>> = AfterStatusString,

	{ AppStatusString, AfterAppStatusString } = erl_cond_str(HasAppStatusString, AfterAppStatus),

	<<RoundTripTime:32, AfterRoundTripTime/binary>> = AfterAppStatusString,

	{ SerializedException, AfterSerializedException } = erl_cond_str(HasSerializedException, AfterRoundTripTime),

	<<TableCount:16, StartOfTables/binary>> = AfterSerializedException,

	Tables = erl_tables(TableCount, StartOfTables),

	case HasStatusString of 1 -> io:format("~s", [StatusString]); _ -> ok end,
	case HasAppStatusString of 1 -> io:format("~s", [AppStatusString]); _ -> ok end,
	

	{ voltresponse,
	  
	  {
	  	Protocol, 
		ClientData, 
		Status, 
		StatusString, 
		AppStatus, 
		AppStatusString, 
		SerializedException,
    	RoundTripTime
      },
      
      Tables
      
	}.
	
%%%----------------------------------------------------------------------------
%%% @private return an empty binary, and the whole input. Or a string and rest.
%%% @spec erl_cond_str(0 | 1, binary()) -> { String::binary(), Remainder::binary() }

erl_cond_str(0, Stream) -> { <<"">>, Stream};

erl_cond_str(1, Stream) -> erl_string_feed(Stream).

%%%----------------------------------------------------------------------------
%%% @private: scan a wire binary into a number of tables, expect no remainders.
%%% @spec erl_tables(Count::integer(), Wire::binary()) -> [ volttable() ]

erl_tables(0, <<>>) -> [];

erl_tables(0, Rest) -> throw({unscanned_rest, Rest});

erl_tables(Count, <<>>) -> throw({binary_too_short, Count});

erl_tables(Count, Wire) when Count > 0 -> 

	{ Table, Rest } = erl_table_feed(Wire),
	% debug io:format("~n~w~n", [Table]),
	[ Table | erl_tables(Count - 1, Rest) ]. 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                CALLBACKS                                    % 
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%   This is a list of callbacks that are kept to be called on execution and   %
%   return of results to the client, from the server.                         %
%                                                                             %
%   On the Java Cient and Server:                                             %
%                                                                             %
%	"... there is an  AtomicLong used to generate the client data and a map   %
%	from  Long values  to the provided  callbacks  for each connection to a   %
%	VoltDB node.  Synchronous  invocations  use an a utomatically generated   %
%	SynchronousCallback.  To  the server the client data  is opaque and not   %
%	necessarily unique  so it generates  a transaction id  and uses that to   %
%	map to the client connection and client data for a response."             %
%	--- A. Weisberg, VoltDB, https://community.voltdb.com/node/83 ---         %
%                                                                             %
%******************************************************************************
%                                                                             %
%    Test using test3.erl.                                                    %
%                                                                             %
%******************************************************************************

%%%----------------------------------------------------------------------------
%%% @doc  Create the callback table. Only one is expected to exist. It goes by
%%%       the registered name of 'callback_table'.
%%% @spec create_callback_table() -> TableId::atom()

create_callback_table() ->

	ets:new(callback_table, [ set, public, named_table, { keypos, 1 }, { write_concurrency, true  } ] ).
	

%%%----------------------------------------------------------------------------
%%% @doc  Create an id as handle to callback functions in the callback list. 
%%% @spec create_callback_id() -> Id::any()

create_callback_id() ->
	
	{ callback_id, { node(), self(), erlang:now() }}.

	
%%%----------------------------------------------------------------------------
%%% @doc  Add a callback function to the callback list.
%%% @spec add_callback(function()) -> Id::any()

add_callback(Fun) when is_function(Fun, 1) ->

	Id = create_callback_id(),
	case ets:insert_new( callback_table, { Id, Fun } ) of
		false -> erlang:error(double_id);
		_ -> Id
	end.
	
%%%----------------------------------------------------------------------------
%%% @doc Internal: find a callback function from internal callback list, or fail.

get_callback({ callback_id, _} = Id) ->

	[ { Id, Fun } ] = ets:lookup(callback_table, Id),
	Fun.

%%%----------------------------------------------------------------------------
%%% @doc Internal: find a callback function in internal callback list.

get_callback_or_nil(Id) ->

	case Id of
		{ callback_id, _} ->
			try
				get_callback(Id)
			catch
				error:{ badmatch, _ } -> nil
			end;
		_ -> nil
	end.

%%%----------------------------------------------------------------------------
%%% @doc Internal: execute callback from internal callback list.

execute_callback(Id, Param) ->

	Fun = get_callback(Id),
	Fun(Param).

%%%----------------------------------------------------------------------------
%%% @doc Internal: execute and delete callback from internal callback list.

resolve_callback({ callback_id, _} = Id, Param) ->

	Fun = get_callback(Id),
	ets:delete(callback_table, Id),
	Fun(Param).

%%%----------------------------------------------------------------------------
%%% @doc Internal: delete callback from internal callback list.

delete_callback({ callback_id, _} = Id) ->

	ets:delete(callback_table, Id).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                 UTILITY                                     % 
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% Verbose Echo
%%%----------------------------------------------------------------------------
%%% @doc Conditionally print String x Format to standard out.

vecho(Condition, String, Format) ->

	case Condition of 
		true -> 
			io:format("~n" ++ String ++ "~n", Format), true;
		_ -> false
	end.

%%%----------------------------------------------------------------------------
%%% Banner
%%%----------------------------------------------------------------------------
%%% @doc Prints a banner with library name and version, and a note.

banner() -> banner(?EXPLAIN).

%%%----------------------------------------------------------------------------
%%% @doc Prints a banner with library name and version, and a note, plus message.

banner(Message) ->
    io:format("---------------------------------------------------------------------------~n"),
    io:format("                                             %                             ~n"),
    io:format("             %%%%% %%%%%   %%   %%   %%    %%%    %%  %%%%%%%%             ~n"),
    io:format("             %%%   %%  %%  %%    %%  %  %%   %%%  %%     %%                ~n"),
    io:format("             %%    %%%%%   %%  %  %%%   %%%   %%  %%  %  %%                ~n"),
    io:format("             %%%%% %%  %%  %%%%%   %      %%%     %%%%%  %%                ~n"),
    io:format("                                          %                                ~n"),
    io:format("---------------------------------------------------------------------------~n"),
    io:format("~s ~s - ~s~n",[?LIBRARY, ?VERSION, Message]),
    ok.


%%%-----------------------------------°%°-----------------------------------%%%

	
	
		