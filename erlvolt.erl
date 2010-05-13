%%%-------------------------------------------------------------------------%%%
%%% File        : erlvolt.erl                                               %%% 
%%% Version     : 0.1/alpha                                                 %%%
%%% Description : Erlang-VoltDB client API                                  %%%
%%% Copyright   : VoltDB, LLC - http://www.voltdb.com                       %%%
%%% Production  : Eonblast Corporation - http://www.eonblast.com            %%%
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%
%%% Licence     : GPLv3                                                     %%%
%%% Created     : 17 Apr 2010                                               %%%
%%% Changed     : 12 May 2010                                               %%% 
%%%-------------------------------------------------------------------------%%%
%%%                                                                         %%%
%%%   THE FUTURE:                                                           %%%
%%%                                                                         %%%
%%%   Erlvolt is an Erlang interface to a VoltDB server. It allows for an   %%%
%%%   Erlang program  to take the place of a  VoltDB client.  Just as the   %%%
%%%   original Java clients, Erlvolt clients:                               %%%
%%%                                                                         %%%
%%%   * can be multiple clients                                             %%%
%%%   * can make synchronous or asynchronous request                        %%%
%%%   * asynchronous requests can be fed a call back                        %%%
%%%                                                                         %%%
%%%   Erlvolt communicates with the VoltDB server using the binary VoltDB   %%%
%%%   wire protocol.  The server does not know wether it is  talking to a   %%%
%%%   Java or an Erlang client.                                             %%%  
%%%                                                                         %%% 
%%%   In many cases the  Erlang  program  will itself be a server and the   %%%
%%%   VoltDB server its database back end.  Implementation of  Erlvolt is   %%%
%%%   as a library.                                                         %%%
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


-module(erlvolt).
-include("erlvolt.hrl").

-import(lists, [reverse/1]).
-import(ets).

-define(VERSION, "0.1").
-define(LIBRARY, "Erlvolt").
-define(EXPLAIN, "Erlang VoltDB client API").

-export([   volt_float/1,
			erlang_float/1,
			erlang_float_or_atom/1,
			volt_byte/1,
			volt_short/1,
			volt_integer/1,
			volt_long/1,
			erl_integer/1,
			volt_string/1,
			volt_time/1,
			volt_time_binary/1,
			milli_epoch/1,
			erl_datetime/1,
			erl_nowtime/1,
			erl_time/1,
			erl_unixtime/1,
			volt_decimal/1,
			erl_number/1,
			erl_number_or_null/1,
			erl_float/1,
			erl_float_or_null/1,
			erl_integer_or_null/1,
			erl_binary_array/1,
			erl_array/1,
			erl_elements/4,
			erl_binary_array_feed/1,
			erl_array_feed/1,
			erl_elements_feed/5,
			erl_any/2,
			erl_tinyint_feed/1,
			erl_smallint_feed/1,
			erl_intint_feed/1,
			erl_bigint_feed/1,
			erl_float_feed/1,
			erl_timestamp_feed/1,
			erl_decimal_feed/1,
			erl_table/1,
			erl_table_rows/3,
			erl_table_fields/3,
			create_callback_table/0,
			create_callback_id/0,
			add_callback/1,
			get_callback/1,
			get_callback_or_nil/1,
			execute_callback/2,
			resolve_callback/2,
			delete_callback/1]).


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

% See erlvolt.hrl for the official Java bit patterns for NaN and Infinities.
% IEEE 754 does not define 'the' bit pattern for, e.g. NaN. Java does. In any
% case there are many bit patterns that are NaN, meaning, they don't have a
% meaningful value as float numbers. But again, Java defines one as 'the' NaN.
% This probably needs to be used for communicating NaN to a Stored Procedure.

% Wikipedia on IEEE 754: - http://en.wikipedia.org/wiki/IEEE_754-1985

% "Only 8-Byte Double types are supported using the byte representation in
% IEEE 754 "double format." Positive and negative infinity, as well as NaN, are
% supported on the wire, but not guaranteed to work as SQL values."
% - pg. 1, VoltDB Client Wire Protocol Version 0, 12/11/09

% "When most people talk of IEEE-754, they are referring to  IEEE-754-1985
% which which encompasses single and double precision floating points, which
% are both binary FP formats. Erlang's floating point type is the double
% precision binary." - D. Smith http://www.erlang.org/cgi-bin/ezmlm-cgi/4/47070

% "... you can construct invalid floating-point bit patterns [...], and since
% Erlang doesn't include "not-a-numbers" in the float data type, an exit is
% signalled. If you expect there to be non-numbers in the binary, you can use
% 'catch' to catch the exit:
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
%     end.
% 
% - D. Walling, http://www.erlang.org/cgi-bin/ezmlm-cgi/4/3254

% The Zuse Z3, the world's first working computer, implemented floating point
% with infinities and undefined values which were passed through operations. 
% These were not implemented properly in hardware in any other machine until 
% the Intel 8087, which was announced in 1980. 
% - % http://en.wikipedia.org/wiki/IEEE_754-1985

% Needs to be defined here because of the erl_integer() defintion for decimals.
-define(VOLT_DECIMALS_SCALESHIFT, 1000000000000).

%%%-encode---------------------------------------------------------------------


%%%----------------------------------------------------------------------------
%%% @doc VoltDB not-a-number wire code (official Java bit pattern)

volt_float(nan) -> 

	?VOLT_NAN;
	

%%% @doc  VoltDB positive infinity float wire code (official Java bit pattern)

volt_float(positive_infinity) -> 

	?VOLT_POSITIVE_INFINITY;


%%% @doc  VoltDB negative infinity float wire code (official Java bit pattern)

volt_float(negative_infinity) -> 

	?VOLT_NEGATIVE_INFINITY;


%%% @doc  Erlang float to VoltDB float wire code

volt_float(E) when is_float(E) -> 

	<<E:32/float>>.


%%%-decode---------------------------------------------------------------------
	

%%% @doc  VoltDB float wire code to Erlang float - throws on NaN/Infinities

erlang_float(<<V:32/signed-float>>) -> 
	
	V.


%%% @doc  VoltDB float wire code to Erlang float - with NaN/Infinities to atoms - any 32bit.

erlang_float_or_atom(<<V:32/signed-float>>) -> 

	V;

erlang_float_or_atom(?VOLT_NAN) ->

	nan;

erlang_float_or_atom(?VOLT_POSITIVE_INFINITY) ->

	positive_infinity;

erlang_float_or_atom(?VOLT_NEGATIVE_INFINITY) ->

	negative_infinity;

erlang_float_or_atom(<<_:32>>) -> 

	nan.


%*****************************************************************************%
%                                 Integers                                    % 
%*****************************************************************************%
%                                                                             %
%   All integer types are signed, twos-compliment and big-endian.             %
%   * Byte - 1 Byte                                                           %
%   * Short - 2 Bytes                                                         %
%   * Integer - 4 Bytes                                                       %
%   * Long - 8 Bytes                                                          %
%   --- pg. 1, VoltDB Client Wire Protocol Version 0, 12/11/09 ---            %
%                                                                             %
%   Also see http://en.wikipedia.org/wiki/Integer_%28computer_science%29      %
%                                                                             %
%******************************************************************************


%%%-encode------------------------------------------------------------integers-

%%% @doc Erlang integer to VoltDB byte (1B)wire code
volt_byte(E) when is_integer(E), E >= -16#80, E =< 16#7f ->
	
	<<E:8>>.

%%% @doc Erlang integer to VoltDB short (2B)wire code
volt_short(E) when is_integer(E), E >= -16#8000, E =< 16#7fff ->
	
	<<E:16>>.

%%% @doc Erlang integer to VoltDB integer (4B)wire code
volt_integer(E) when is_integer(E), E >= -16#80000000, E =< 16#7fffffff ->
	
	<<E:32>>.

%%% @doc Erlang integer to VoltDB long (8B)wire code
volt_long(E) when is_integer(E), E >= -16#8000000000000000, E =< 16#7fffffffffffffff ->
	
	<<E:64>>.

%%%-decode---------------------------------------------------------------------
	
%%% @doc VoltDB empty integer wire code to Erlang integer
erl_integer(<<>>) ->
	
	0;

%%% @doc VoltDB byte wire code to Erlang integer
erl_integer(<<E:8/signed>>=V) when is_binary(V), E >= ?VOLT_BYTE_MIN, E =< ?VOLT_BYTE_MAX ->
	
	E;

%%% @doc VoltDB short wire code to Erlang integer
erl_integer(<<E:16/signed>>=V) when is_binary(V), E >= ?VOLT_SHORT_MIN, E =< ?VOLT_SHORT_MAX ->
	
	E;

%%% @doc VoltDB integer wire code to Erlang integer
erl_integer(<<E:32/signed>>=V) when is_binary(V), E >= ?VOLT_INTEGER_MIN, E =< ?VOLT_INTEGER_MAX ->
	
	E;

%%% @doc VoltDB long wire code to Erlang integer
erl_integer(<<E:64/signed>>=V) when is_binary(V), E >= ?VOLT_LONG_MIN, E =< ?VOLT_LONG_MAX ->
	
	E;

%%% @doc VoltDB decimals wire code to Erlang integer
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
%   TODO: 1M LIMIT.                                                           %
%                                                                             %
%   Erlang default is big endian.                                             %
%                                                                             %
%******************************************************************************

% Hint: erlvolt.htl ... -define(VOLT_STRING_SIZE_BINARY_TYPE, 32/big-signed) ...

%%%-encode---------------------------------------------------------------------


%%% @doc VoltDB string wire code for NULL

volt_string(null) -> <<-1:?VOLT_STRING_SIZE_BINARY_TYPE>>;
	

%%% @doc VoltDB string wire code for empty string

volt_string("")   -> <<0:?VOLT_STRING_SIZE_BINARY_TYPE>>;
volt_string(<<>>) -> <<0:?VOLT_STRING_SIZE_BINARY_TYPE>>;
	

%%% @doc Erlang string (list) to VoltDB string wire code

volt_string(V) when is_list(V) -> 

	volt_string(list_to_binary(V));


%%% @doc Erlang string (binary) to VoltDB string wire code

volt_string(V) when is_binary(V), erlang:size(V) =< ?VOLT_SHORT_MAX ->
	
	Size = erlang:size(V),
	<<Size:?VOLT_STRING_SIZE_BINARY_TYPE, V/binary>>.


%%% ----> @ doc Erlang string (list) to VoltDB string wire code

% TODO: ? erl_string(A) when is_list(A) ->
%
%	null.

%%%-decode---------------------------------------------------------------------
	
% VoltDB string code to Erlang string (list)
% TODO: ? volt_string2(A) when is_binary(A) ->
%	
%	null.
	
% VoltDB string code to Erlang binary (list)
% TODO: ? erl_binary(A) when is_binary(A) ->
%	
%	null.


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


%%% @doc Erlang 'now format' time to VoltDB wire code time binary.

volt_time({Mega, Sec, Micro}) when is_integer(Mega), is_integer(Sec), is_integer(Micro) ->

    volt_time_binary(milli_epoch({Mega, Sec, Micro}));


%%% Erlang 'datetime' format to VoltDB wire code time binary

volt_time({Date,Time}) ->

	UnixEpoch = {{1970,1,1},{0,0,0}},
	MilliEpoch = (calendar:datetime_to_gregorian_seconds({Date,Time}) 
		- calendar:datetime_to_gregorian_seconds(UnixEpoch)) 
		* 1000,
    volt_time_binary(MilliEpoch);


%%% UTC time to VoltDB wire code time binary

volt_time(UTC) when is_integer(UTC) ->

    volt_time_binary(UTC * 1000).


%%% @doc Over/Underrun guards and cast to binary

volt_time_binary(MilliEpoch) when MilliEpoch < ?VOLT_TIME_MIN ->
    erlang:error(time_underrun);

volt_time_binary(MilliEpoch) when MilliEpoch > ?VOLT_TIME_MAX ->
    erlang:error(time_overrun);

volt_time_binary(MilliEpoch) when is_integer(MilliEpoch) ->
	?VOLT_TIME_BINARY_TYPE(MilliEpoch).


%%% @doc actual calculation 'now' format to VoltDB wire format

milli_epoch({Mega, Sec, Micro}) ->
    Mega * ?BILLION + Sec * 1000 + trunc(Micro / 1000).


%%%-decode----------------------------------------------------------------time-
	

%%% @doc VoltDB wire code time /as binary/ to Erlang 'DateTime' format

erl_datetime(?VOLT_TIME_BINARY_TYPE(Int)=V) when is_binary(V) -> 

	calendar:now_to_universal_time(erl_nowtime(Int)). % -> {Date,Time}


%%% VoltDB wire code time /as binary/ to Erlang 'Now' format
%%% Note: the 'now' format in Erlang means {Megasecs, Secs, Microsecs}.
%%% Because this can be confusing to read for, say, Java devs,
%%% there are synomyms introduced below, leaving the 'now' out.

erl_nowtime(?VOLT_TIME_BINARY_TYPE(VInt)=V) when is_binary(V) -> 

	erl_nowtime(VInt); % -> {Megasecs, Secs, Microsecs};

	
%%% VoltDB wire code time /already as integer/ to Erlang 'Now' format
%%% integer input represents an interm'ry. step but might come in handy somewhere.

erl_nowtime(V) when is_integer(V) -> % TODO: make better
	
	Mega  = trunc(V / ?BILLION),
	Sec   = trunc((V - Mega * ?BILLION) / 1000),
	Micro = (V - Mega * ?BILLION - Sec * 1000) * 1000,
	{Mega, Sec, Micro}.


%%% @doc synonyms of erl_nowtime/1.

erl_time(?VOLT_TIME_BINARY_TYPE(VInt)=V) when is_binary(V) -> 
	
	erl_nowtime(VInt); 
	
erl_time(V) when is_integer(V) ->
	
	erl_nowtime(V).


%%% @doc Unix epoch seconds from VoltDB wire code time binary. 

erl_unixtime(?VOLT_TIME_BINARY_TYPE(VInt)=V) when is_binary(V) -> 

	erl_unixtime(VInt); % -> int() Seconds since 1/1/1970 0:00 GMT;

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
%   -define(VOLT_DECIMAL_BINARY_TYPE(E), E:128/signed-big).               %
%   -define(VOLT_DECIMAL_MAX,    99999999999999999999999999999999999999).     %
%   -define(VOLT_DECIMAL_MIN,   -99999999999999999999999999999999999999).     %
%   -define(VOLT_DECIMAL_NULL, -170141183460469231731687303715884105728).     %
%                                                                             %
%%%------------------------------------------------------------------decimals-%

% (needs to be defined above, as erl_integer is defined above:)
% -define(VOLT_DECIMALS_SCALESHIFT, 1000000000000).


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

%%% @doc VoltDB binary decimals wire code to Erlang integer.

erl_number(?VOLT_DECIMAL_BINARY_TYPE(?VOLT_DECIMAL_NULL)=V) when is_binary(V) ->

	throw('this is the null value');
	
erl_number(?VOLT_DECIMAL_BINARY_TYPE(E)=V) when is_binary(V) ->

	case E rem ?VOLT_DECIMALS_SCALESHIFT of
		0 -> E div ?VOLT_DECIMALS_SCALESHIFT;
		_ -> E / ?VOLT_DECIMALS_SCALESHIFT
	end.

erl_number_or_null(?VOLT_DECIMAL_BINARY_TYPE(?VOLT_DECIMAL_NULL)=V) when is_binary(V) ->

	null;
	
erl_number_or_null(V) when is_binary(V) ->

	erl_number(V).
	

erl_float(?VOLT_DECIMAL_BINARY_TYPE(E)=V) when is_binary(V) ->

	E / ?VOLT_DECIMALS_SCALESHIFT.

erl_float_or_null(?VOLT_DECIMAL_BINARY_TYPE(?VOLT_DECIMAL_NULL)=V) when is_binary(V) ->

	null;
	
erl_float_or_null(V) when is_binary(V) ->

	erl_float(V).
	

%%% @doc erl_integer(?VOLT_DECIMAL_BINARY_TYPE(E)=V) when is_binary(V) ->
%
%	E div ?VOLT_DECIMALS_SCALESHIFT

erl_integer_or_null(?VOLT_DECIMAL_BINARY_TYPE(?VOLT_DECIMAL_NULL)=V) when is_binary(V) ->

	null;
	
erl_integer_or_null(V) when is_binary(V) ->

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
%   TODO: size limit                                                          %
%                                                                             %
%******************************************************************************


%%%-encode--------------------------------------------------------------arrays-

%%%-decode--------------------------------------------------------------arrays-

%%% @doc EXCEPTION: TINY INT
erl_binary_array(<<?VOLT_TINYINT:8, Count:32, Stream>>=Bin) when is_binary(Bin) ->

	<<Binary:Count/binary, Rest>> = Stream,
	{ Binary, Rest }.

erl_array(<<?VOLT_TINYINT:8, Count:32, Stream>>=Bin) when is_binary(Bin) ->

	erl_elements(?VOLT_TINYINT, Count, Stream, Stream);

%%% @doc ALL OTHER
erl_array(<<Type:8, Count:16, Stream>>=Bin) when is_binary(Bin) ->

	erl_elements(Type, Count, Stream, Stream).

erl_elements(_, 0, _, _) ->
	
	[];

erl_elements(Type, Left, Parse, Full) when Left > 0, Parse /= <<>> ->

	{ Element, Rest } = erl_any(Type, Parse),

	[ Element | erl_elements(Type, Left-1, Rest, Full)].



%%% @doc EXCEPTION: TINY INT
erl_binary_array_feed(<<?VOLT_TINYINT:8, Count:32, Stream>>=Bin) when is_binary(Bin) ->

	<<Binary:Count/binary, Rest>> = Stream,
	{ Binary, Rest }.

erl_array_feed(<<?VOLT_TINYINT:8, Count:32, Stream>>=Bin) when is_binary(Bin) ->

	<<Binary:Count/binary, Rest>> = Stream,
	{ binary_to_list(Binary), Rest };

%%% @doc ALL OTHER
erl_array_feed(<<Type:8, Count:16, Stream>>=Bin) when is_binary(Bin) ->

	erl_elements_feed(Type, [], Count, Stream, Stream).

erl_elements_feed(_, Result, 0, Rest, _) ->
	
	{ Result, Rest };

erl_elements_feed(Type, Result, Left, Stream, Full) when Left > 0, Stream /= <<>> ->

	{ Element, Rest } = erl_any(Type, Stream),
	erl_elements_feed(Type, [ Element | Result ], Left-1, Rest, Full).


%%%-map-over-read-stream------------------------------------------------arrays-

% These all -> { Element, Rest }

erl_any(?VOLT_ARRAY,     Stream) -> erl_array_feed     (Stream);
% erl_any(?VOLT_NULL,      Stream) -> erl_null_feed      (Stream);
erl_any(?VOLT_TINYINT,   Stream) -> erl_tinyint_feed   (Stream);
erl_any(?VOLT_SMALLINT,  Stream) -> erl_smallint_feed  (Stream);
erl_any(?VOLT_INTEGER,   Stream) -> erl_intint_feed    (Stream);
erl_any(?VOLT_BIGINT,    Stream) -> erl_bigint_feed    (Stream);
erl_any(?VOLT_FLOAT,     Stream) -> erl_float_feed     (Stream);
%erl_any(?VOLT_STRING,    Stream) -> erl_string_feed    (Stream);
erl_any(?VOLT_TIMESTAMP, Stream) -> erl_timestamp_feed (Stream);
erl_any(?VOLT_DECIMAL,   Stream) -> erl_decimal_feed   (Stream).

% erl_null_feed     (<<?VOLT_NULL,                     Rest>>) -> { null, Rest }.
erl_tinyint_feed  (<<Element:?VOLT_TINYINT_TYPE,    Rest>>) -> { Element, Rest }.
erl_smallint_feed (<<Element:?VOLT_SMALLINT_TYPE,   Rest>>) -> { Element, Rest }.
erl_intint_feed   (<<Element:?VOLT_INTINT_TYPE,     Rest>>) -> { Element, Rest }.
erl_bigint_feed   (<<Element:?VOLT_BIGINT_TYPE,     Rest>>) -> { Element, Rest }.
erl_float_feed    (<<Binary:?VOLT_FLOAT_BINARY,     Rest>>) -> { erlang_float_or_atom(Binary), Rest }. % TODO: is '_or_atom' right?
%erl_string_feed   (<<?VOLT_STRING_BINARY(Binary),   Rest>>) -> { erl_string_or_null(Binary), Rest }. % TODO: is '_or_null' right?
erl_timestamp_feed(<<Binary:?VOLT_TIMESTAMP_BINARY, Rest>>) -> { erl_time(Binary), Rest }.
erl_decimal_feed  (<<Binary:?VOLT_DECIMAL_BINARY,   Rest>>) -> { erl_number_or_null(Binary), Rest }. % TODO: is '_or_null' right?

% for erl_array_feed see above
% for erl_volttable_feed see below

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


%%%-encode-----------------------------------------------------------volttable-

%%%-decode-----------------------------------------------------------volttable-

erl_table(<<_Length:32, MetaLength:32, _Status:8, ColumnCount:16, Stream>>=Bin) 
	when is_binary(Bin) ->

	%% Calculate byte count of column names
	ColumnNamesSpace = MetaLength - 3 - ColumnCount,
	
	%% Scan Rest of Meta Data 
	<<ColumnTypeBinaries:ColumnCount/binary, 
	  ColumnNamesBinary:ColumnNamesSpace/binary, 
	  RowCount:32,
	  RowsBinary/binary>> = Stream,
	
	%% Make List of Column Types (integers)
	ColumnTypes = binary_to_list(ColumnTypeBinaries),
	RowCount = ColumnTypes,

	%% Make List of Column Names
	ColumnNames = [ Name || <<Size:32, Name:Size/binary>> <= ColumnNamesBinary ],
	RowCount = length(ColumnNames),
	% TODO: cache this globally
	
	%% Scan Rows
	RowBinaries = [ RowBinary || <<Size:32, RowBinary:Size/binary>> <= RowsBinary ],
	RowCount = length(RowBinaries),
	
	%% Scan Rows
	Rows = erl_table_rows(ColumnTypes, ColumnNames, RowBinaries).

	
erl_table_rows(_, _, []) ->

	[];

erl_table_rows(Types, Names, [ RowBinary | RowBinaryTail ]) ->

	[ erl_table_fields(Types, Names, RowBinary) | erl_table_rows(Types, Names, RowBinaryTail ) ].


%% 
erl_table_fields([ Type | TypesTail ], [ Name | NamesTail ], RowBinaryStream ) ->

	{ Value, RowBinaryRest } = erl_any(Type, RowBinaryStream),

	[ Value | erl_table_fields(TypesTail, NamesTail, RowBinaryRest ) ].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                CALLBACKS                                    % 
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%   This is a list of callbacks that are kept to be called on execution and   %
%   return of results to the client, from the server.                         %
%                                                                             %
%******************************************************************************
%                                                                             %
%    Test using test3.erl.                                                    %
%                                                                             %
%******************************************************************************

create_callback_table() ->

	ets:new(callback_table, [ set, public, named_table, { keypos, 1 }, { write_concurrency, true  } ] ).
	

create_callback_id() ->
	
	{ callback_id, { node(), self(), erlang:now() }}.

	
% @doc Add a callback function to the callback list
% @spec add_callback(function()) -> Id::any()

add_callback(Fun) when is_function(Fun, 1) ->

	Id = create_callback_id(),
	case ets:insert_new( callback_table, { Id, Fun } ) of
		false -> erlang:error(double_id);
		_ -> Id
	end.
	
	
get_callback({ callback_id, _} = Id) ->

	[ { Id, Fun } ] = ets:lookup(callback_table, Id),
	Fun.

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

execute_callback(Id, Param) ->

	Fun = get_callback(Id),
	Fun(Param).

resolve_callback({ callback_id, _} = Id, Param) ->

	Fun = get_callback(Id),
	ets:delete(callback_table, Id),
	Fun(Param).

delete_callback({ callback_id, _} = Id) ->

	ets:delete(callback_table, Id).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                 UTILITY                                     % 
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% Banner
%%%----------------------------------------------------------------------------
%%% @doc Prints a banner with library name and version, and a note.
banner() ->
	    io:format("~s ~s - ~s~n",[?LIBRARY, ?VERSION, ?EXPLAIN]).




