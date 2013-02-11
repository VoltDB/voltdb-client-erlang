%%%-------------------------------------------------------------------------%%%
%%% File        : erlvolt_wire.erl                                          %%%
%%% Version     : 0.3/beta                                                  %%%
%%% Description : Erlang VoltDB driver wire protocol handling               %%%
%%% Copyright   : VoltDB, LLC - http://www.voltdb.com                       %%%
%%% Production  : Eonblast Corporation - http://www.eonblast.com            %%%
%%% Author      : H. Diedrich <hd2012@eonblast.com>                         %%%
%%% License     : MIT                                                       %%%
%%% Created     : 17 Apr 2010                                               %%%
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
%%% You can run a sample using the 'Hello' tutorial-server discussed        %%%
%%% in the VoltDB manual and present in every VoltDB distribution.          %%%
%%%                                                                         %%%
%%% Start that server from your voltdb installation with:                   %%%
%%%                                                                         %%%
%%%     $ cd voltdb/doc/tutorial/helloworld                                 %%%
%%%     $ ./run.sh                                                          %%%
%%%                                                                         %%%
%%% Then run the hello world example, using make from the driver root:      %%%
%%%                                                                         %%%
%%%     $ make hello                                                        %%%
%%% or                                                                      %%%
%%%     $ make                                                              %%%
%%%     $ cd examples                                                       %%%
%%%     $ erlc -I ../include -o ../ebin +debug_info hello_plus.erl          %%%
%%%     $ erl -pa ../ebin -s hello_plus run -s init stop -noshell           %%%
%%%                                                                         %%%
%%% You will see this response, 'Hello, world!' in Swedish:                 %%%
%%%                                                                         %%%
%%%     Hej världen!                                                        %%%
%%%                                                                         %%%
%%% The hello world source is found in examples/hello_plus.erl              %%%
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

-module(erlvolt_wire).

-vsn("0.3/beta").
-author("H. Diedrich <hd2010@eonblast.com>").
-license("MIT - http://www.opensource.org/licenses/mit-license.php").
-copyright("(c) 2010-12 VoltDB, LLC - http://www.voltdb.com").

-define(VERSION, "0.3/beta").
-define(LIBRARY, "Erlvolt").
-define(EXPLAIN, "Erlang VoltDB driver").

-define(V, false). % verbosity

%%%-------------------------------------------------------------------------%%%

-include("erlvolt.hrl").
-include("erlvolt_internal.hrl").
-include("erlvolt_wire.hrl").

-export([
            banner/0,
            banner/1,
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
            help/0,
            micro_epoch/1,
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

            login/4,
            login/5,
            connect/3,
            connect/6,
            create_connection/0,
            create_connection/1,
            create_connection/4,
            create_connection/10,
            call_procedure/5,
            volt_login/3,
            volt_hash/1,
            volt_header/0,
            volt_invoke/3,
            volt_parameters/1,
            volt_parameter/1,

            get_table/2,
            get_table_list/1,
            get_row/2,
            get_row_list/1,
            get_record/3,
            get_field/2,
            get_integer/2,
            get_integer/3,
            get_integer_or_null/2,
            get_integer_or_null/3,
            get_string/2,
            get_string/3,
            get_string_or_null/2,
            get_string_or_null/3,
            list_ord/2,
            roundtrip/1,
            get_status/1,
            get_statusstring/1,

			close/1,

			dump/1,
			brief/1,
			show/1
            ]).


% 

%%%****************************************************************************
%%% HELP
%%%****************************************************************************
%%% @doc Displays a brief pointer about were to get more help.

%% @spec help() -> ok
help() ->
    banner(),
    io:format("See sample*.erl and test.erl for more information.~n").
    % 


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                              BASIC TYPES                                    %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%*****************************************************************************%
%                        Floating Point Numbers                               %
%*****************************************************************************%
%                                                                             %
% See  erlvolt.hrl for the official Java bit patterns for NaN and Infinities. %
% IEEE 754 does not define 'the' bit pattern for,e.g. NaN. Java does. In any  %
% case there are many bit patterns that are NaN,  meaning, they don't have a  %
% meaningful value as float numbers.But again, Java defines one as 'the' NaN. %
% This probably needs be used for communicating NaN to a Stored Procedure.    %
%                                                                             %
% -- Wikipedia on IEEE 754: [http://en.wikipedia.org/wiki/IEEE_754-1985] --   %
%                                                                             %
% "Only 8-Byte Double types  are supported using the  byte representation in  %
% IEEE 754 "double format."  Positive and negative infinity,  as well as NaN, %
% are supported on the wire, but not guaranteed to work as SQL values."       %
%                                                                             %
% -- pg. 1, VoltDB Client Wire Protocol Version 0, 12/11/09 --                %
%                                                                             %
% "When most people talk of  IEEE-754,  they are referring to  IEEE-754-1985  %
% which which encompasses single and double precision floating points, which  %
% are both binary FP formats.  Erlang's  floating point type  is the  double  %
% precision binary."                                                          %
%                                                                             %
% -- D. Smith [http://www.erlang.org/cgi-bin/ezmlm-cgi/4/47070] --            %
%                                                                             %
% "... you can construct invalid floating-point bit patterns [..], and since  %
% Erlang doesn't include "not-a-numbers" in the float data type,  an exit is  %
% signalled. If you expect there to be non-numbers in the binary,you can use  %
% 'catch' to catch the exit:                                                  %
%                                                                             %
% bytes_to_float(A,B,C,D) ->                                                  %
%     <<Float:32/signed-float>> = <<A, B, C, D>>,                             %
%     Float.                                                                  %
%                                                                             %
% convert(A,B,C,D) ->                                                         %
%     case catch bytes_to_float(A,B,C,D) of                                   %
%   {'EXIT', _} ->                                                            %
%       not_a_number;                                                         %
%   F ->                                                                      %
%       F                                                                     %
%     end.                                                                    %
%                                                                             %
% -- D. Walling, http://www.erlang.org/cgi-bin/ezmlm-cgi/4/3254 --            %
%                                                                             %
% The Zuse Z3, the world's first working computer,implemented floating point  %
% with infinities and undefined values  which were passed through operations. %
% These were not implemented properly in hardware in any other machine until  %
% the Intel 8087, which was announced in 1980.                                %
%                                                                             %
% -- Wikipedia on IEEE 754: [http://en.wikipedia.org/wiki/IEEE_754-1985] --   %
%                                                                             %
%%%-------------------------------------------------------------------------%%%

% Needs to be defined here because of the erl_integer() defintion for decimals.
-define(VOLT_DECIMALS_SCALESHIFT, 1000000000000).

%%% @type wiretype() = volttype() + binary()
%%% @type date() = {{integer(),integer(),integer()},{integer(),integer(),integer()}}
%%% @type time() = {{integer(),integer(),integer()}}
%%% @type element() = 'nan' | 'negative_infinity' | 'nil' | 'null' | 'positive_infinity' | binary() | number() | time().
%%% @type host() = inet:ip_address() | inet:hostname().
%%% @type portnumber() = inet:port_number().
%%% @type hosts() = [{host(),portnumber()}].
%%% @type lobstring() = binary() | list().
%%% @type connection() = inet:socket().

%%%-encode---------------------------------------------------------------------

%%%----------------------------------------------------------------------------
%%% @doc Erlang value to VoltDB float.
%% @spec volt_float('nan' | 'negative_infinity' | 'positive_infinity' | float()) -> <<_:32>>

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
%%% @doc VoltDB float wire code to Erlang float - throws on NaN/Infinities
%%% @see volt_float/1.

%% @spec erl_float(<<_:32>>) -> float()
erl_float(<<V:32/signed-float>>) ->

    V.

%%%----------------------------------------------------------------------------
%%% @doc VoltDB float wire code to Erlang float - with NaN/Infinities to atoms - anything 32bit.
%%% @see volt_float/1.
-spec erl_float_or_atom(<<_:32>>) -> float() | atom().

%% @spec erl_float_or_atom(<<_:32>>) -> float() | atom()
erl_float_or_atom(?VOLT_NAN) ->

    nan;

erl_float_or_atom(?VOLT_POSITIVE_INFINITY) ->

    positive_infinity;

erl_float_or_atom(?VOLT_NEGATIVE_INFINITY) ->

    negative_infinity;

erl_float_or_atom(<<V:32/signed-float>>) ->

    V.

%*****************************************************************************%
%                                 Integers                                    %
%*****************************************************************************%
%                                                                             %
%   All integer types are signed, twos-compliment and big-endian.             %
%   * Byte - 1 Byte                                                           %
%   * Short - 2 Bytes                                                         %
%   * Integer - 4 Bytes                                                       %
%   * Long - 8 Bytes                                                          %
%                                                                             %
%   --- pg. 1, VoltDB Client Wire Protocol Version 0, 12/11/09 ---            %
%                                                                             %
%   Also see http://en.wikipedia.org/wiki/Integer_%28computer_science%29      %
%                                                                             %
%*****************************************************************************%

%%%-encode------------------------------------------------------------integers-

%%%----------------------------------------------------------------------------
%%% @doc Erlang integer to VoltDB TINYINT (8 bit) wire code.

%% @spec volt_byte(integer()) -> <<_:8>>
volt_byte(E) when is_integer(E), E >= -16#80, E =< 16#7f ->

    <<E:8>>.

%%%----------------------------------------------------------------------------
%%% @doc Erlang integer to VoltDB SHORTINT (16 bit) wire code.

%% @spec volt_short(integer()) -> <<_:16>>
volt_short(E) when is_integer(E), E >= -16#8000, E =< 16#7fff ->

    <<E:16>>.

%%%----------------------------------------------------------------------------
%%% @doc Erlang integer to VoltDB SHORTINT (16 bit) wire code.

%% @spec volt_small(integer()) -> <<_:16>>
volt_small(E) when is_integer(E), E >= -16#8000, E =< 16#7fff ->

    <<E:16>>.

%%%----------------------------------------------------------------------------
%%% @doc Erlang integer to VoltDB INTEGER (32 bit) wire code.

%% @spec volt_integer(integer()) -> <<_:32>>
volt_integer(E) when is_integer(E), E >= -16#80000000, E =< 16#7fffffff ->

    <<E:32>>.

%%%----------------------------------------------------------------------------
%%% @doc Erlang integer to VoltDB INTEGER (32 bit) wire code.

%% @spec volt_intint(integer()) -> <<_:32>>
volt_intint(E) when is_integer(E), E >= -16#80000000, E =< 16#7fffffff ->

    <<E:32>>.

%%%----------------------------------------------------------------------------
%%% @doc Erlang integer to VoltDB BIGINT (64 bit) wire code.

%% @spec volt_long(integer()) -> <<_:64>>
volt_long(E) when is_integer(E), E >= -16#8000000000000000, E =< 16#7fffffffffffffff ->

    <<E:64>>.

%%%----------------------------------------------------------------------------
%%% @doc Erlang integer to VoltDB BIGINT (64 bit) wire code.

%% @spec volt_bigint(integer()) -> <<_:64>>
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
% 

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
        _ -> throw(loosing_precision)
    end.

%%%-decode-stream--------------------------------------------------------------


%*****************************************************************************%
%                                 Strings                                     %
%*****************************************************************************%
%                                                                             %
%   VoltDB: Strings begin with a 4-byte integer storing the number of bytes   %
%   of character data,  followed by the  character data.  UTF-8 is the only   %
%   supported character encoding. Note: Strings are artificially limited to   %
%   1 megabyte. The NULL string has a length preceded value of -1 (negative   %
%   one) followed by 0 (zero) bytes of string data. The empty string is re-   %
%   presented with a length preceding value of 0.                             %
%   --- Pg. 2, VoltDB Client Wire Protocol Version 0, 05/05/10 ---            %
%                                                                             %
%   
%                                                                             %
%   Erlang default is big endian.                                             %
%                                                                             %
%******************************************************************************

% Hint: erlvolt.htl ... -define(VOLT_STRING_SIZE_BINARY_TYPE, 32/big-signed) ...

%%%-encode---------------------------------------------------------------------


%%%----------------------------------------------------------------------------
%%% @doc VoltDB string wire code for NULL

%% @spec volt_string('null' | binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | byte(),binary() | [])) -> <<_:32,_:_*8>>
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

% 
%
%   null.

%%%-decode---------------------------------------------------------------------

%%%----------------------------------------------------------------------------
% VoltDB string code to Erlang string (list)
% 
%
%   null.

%%%----------------------------------------------------------------------------
% VoltDB string code to Erlang binary (list)
% 
%
%   null.

%%%----------------------------------------------------------------------------
% VoltDB string code to Erlang binary (list)
%% @spec erl_string_or_null(<<_:32,_:_*8>>) -> 'null' | binary()
erl_string_or_null(<<?VOLT_STRING_NULL>>) ->

    null;

erl_string_or_null(<<?VOLT_STRING_BINARY(String)>>) ->

    String.


%*****************************************************************************%
%                                    Time                                     %
%*****************************************************************************%
%                                                                             %
%   Volt: All dates are represented on the wire as Long values.  This signed  %
%   number represents the number of microseconds (not the usual milliseconds  %
%   before or after Jan. 1 1970 00:00:00 GMT, the Unix epoch.                 %
%                                                                             %
%   --- pg. 2, VoltDB Client Wire Protocol Version 0, 06/02/10 ---            %
%                                                                             %
%  Erlang Standard: {Date,Time} = {{Hour,Minutes,Seconds}, {Year,Month,Day}}. %
%  Unique Timestamps: {Megaseconds,Seconds,Microseconds} = erlang:now() from  %
%  1/1/19070 0:00.  erlang:now()  is  guaranteed  to deliver  unique results. %
%  Note that  erlang:now()  and  erlang:universal_time()  can be out of sync, %
%  as an Erlang 'feature': now() is guaranteed deliver a *unique* timestamps  %
%  on every call, and is used for timers,  so is only slowly altered even in  %
%  cases where universal_time() is abruptly changed by external action.       %
%                                                                             %
%  Samples: erlang:now() -> {1272,805301,939371}                              %
%  calendar:now_to_datetime({1272,805301,939371}) -> {{2010,5,2},{13,1,41}}.  %
%                                                                             %
%  1 megasec == 1 billion millisec == 1 trillion microsec.                    %
%                                                                             %
%%%-------------------------------------------------------------------------%%%

-define(VOLT_TIME_BINARY_TYPE(V), <<V:64/signed-big>>).

-define(VOLT_TIME_MIN, ?VOLT_LONG_MIN).
-define(VOLT_TIME_MAX, ?VOLT_LONG_MAX).

-define(TRILLION, 1000000000000).
-define(BILLION,  1000000000).
-define(MILLION,  1000000).

%%%-encode----------------------------------------------------------------time-


%%%----------------------------------------------------------------------------
%%% @doc Erlang 'now format' time to VoltDB wire code time binary.

%% @spec volt_time(integer() | {{non_neg_integer(),1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12,1.255},{byte(),byte(),byte()}} | {integer(),integer(),integer()}) -> <<_:64>>
volt_time({Mega, Sec, Micro}) when is_integer(Mega), is_integer(Sec),
    is_integer(Micro) ->

    volt_time_binary(micro_epoch({Mega, Sec, Micro}));


%%%----------------------------------------------------------------------------
%%% Erlang 'datetime' format to VoltDB wire code time binary

volt_time({Date,Time}) ->

    UnixEpoch = {{1970,1,1},{0,0,0}},
    MicroEpoch = (calendar:datetime_to_gregorian_seconds({Date,Time})
        - calendar:datetime_to_gregorian_seconds(UnixEpoch))
        * ?MILLION,
    volt_time_binary(MicroEpoch);


%%%----------------------------------------------------------------------------
%%% UTC time (seconds since epoch) to VoltDB wire code time binary

volt_time(UTC) when is_integer(UTC) ->

    volt_time_binary(UTC * ?MILLION).


%%%----------------------------------------------------------------------------
%%% @doc Over/Underrun guards and cast to binary

%% @spec volt_time_binary(integer()) -> <<_:64>>
volt_time_binary(MicroEpoch) when MicroEpoch < ?VOLT_TIME_MIN ->

    erlang:error(time_underrun);

volt_time_binary(MicroEpoch) when MicroEpoch > ?VOLT_TIME_MAX ->

    erlang:error(time_overrun);

volt_time_binary(MicroEpoch) when is_integer(MicroEpoch) ->

    ?VOLT_TIME_BINARY_TYPE(MicroEpoch).


%%%----------------------------------------------------------------------------
%%% @doc actual calculation 'now' format to VoltDB wire format

%% @spec micro_epoch({number(),number(),number()}) -> number()
micro_epoch({Mega, Sec, Micro}) ->

    Mega * ?TRILLION + Sec * ?MILLION + Micro.


%%%-decode----------------------------------------------------------------time-


%%%----------------------------------------------------------------------------
%%% @doc VoltDB wire code time /as binary/ to Erlang 'DateTime' format

%% @spec erl_datetime(<<_:64>>) -> {{1..1114111,1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12,1.255},{byte(),byte(),byte()}}
erl_datetime(?VOLT_TIME_BINARY_TYPE(Int)=V) when is_binary(V) ->

    calendar:now_to_universal_time(erl_nowtime(Int)). % -> {Date,Time}


%%%----------------------------------------------------------------------------
%%% @doc VoltDB wire code time from binary to Erlang 'Now' format.
%%% Note: the 'now' format in Erlang means {Megasecs, Secs, Microsecs}.
%%% Because the term 'now' may be confusing to read for, say, devs coming from,
%%% Java there are synomyms introduced, leaving the 'now' out: erl_time().
%%%
%%% Alternate Variant:
%%%
%%% VoltDB wire code time from integer to Erlang 'Now' format.
%%% Integer input represents an intermediary step but might come in handy
%%% someplace.
%%%
%%% @spec erl_nowtime(wire() | integer()) -> {Megasecs, Secs, Microsecs}
%%% 	Megasecs = non_neg_integer()
%%% 	Secs = non_neg_integer()
%%% 	Microsecs = non_neg_integer()
erl_nowtime(?VOLT_TIME_BINARY_TYPE(VInt)=V) when is_binary(V) ->

    erl_nowtime(VInt); % -> {Megasecs, Secs, Microsecs};


erl_nowtime(V) when is_integer(V) -> % 

    Mega  = trunc(V / ?TRILLION),
    Sec   = trunc((V - Mega * ?TRILLION) / ?MILLION),
    Micro = (V - Mega * ?TRILLION - Sec * ?MILLION),
    {Mega, Sec, Micro}.


%%%----------------------------------------------------------------------------
%%% @doc synonyms of erl_nowtime/1.

%% @spec erl_time(<<_:64>> | integer()) -> {integer(),integer(),integer()}
erl_time(?VOLT_TIME_BINARY_TYPE(VInt)=V) when is_binary(V) ->

    erl_nowtime(VInt);

erl_time(V) when is_integer(V) ->

    erl_nowtime(V).


%%%----------------------------------------------------------------------------
%%% @doc Unix epoch seconds from VoltDB wire code time binary.

%% @spec erl_unixtime(<<_:64>> | integer()) -> integer()
erl_unixtime(?VOLT_TIME_BINARY_TYPE(VInt)=V) when is_binary(V) ->

    erl_unixtime(VInt); % -> int() Seconds since 1/1/1970 0:00 GMT;

%%%----------------------------------------------------------------------------
%%% @doc Unix epoch seconds from VoltDB wire code time long integer.
%%% @spec erl_unixtime(epoch_microseconds::int()) -> epoch_seconds::int()

erl_unixtime(V) when is_integer(V) ->

    trunc(V / ?MILLION).  % -> int() Seconds since 1/1/1970 0:00 GMT;


%*****************************************************************************%
%                                Decimals                                     %
%*****************************************************************************%
%                                                                             %
%   "VoltDB implements a fixed precision and scale DECIMAL(38,12) type.This   %
%   type is serialized as a  128 bit signed twos complement  integer repre-   %
%   senting the unscaled decimal value.  The integer must be in  big-endian   %
%   byte order with the most significant bytes first. Null is serialized as   %
%   the smallest representable value which is                                 %
%   -170141183460469231731687303715884105728. Serializing values (Null        %
%   excluded) that are greater than  99999999999999999999999999999999999999   %
%   or less than -99999999999999999999999999999999999999" will result in an   %
%   error response."                                                          %
%                                                                             %
%   Erlang works with limited float precision and will therefor not be able   %
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
%   This also makes for ambiguity  when converting back that can even mask    %
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
%   
%                                                                             %
%   header defines:                                                           %
%   -define(VOLT_DECIMAL_BINARY_TYPE(E), E:128/signed-big).                   %
%   -define(VOLT_DECIMAL_MAX,      99999999999999999999999999999999999999).   %
%   -define(VOLT_DECIMAL_MIN,     -99999999999999999999999999999999999999).   %
%   -define(VOLT_DECIMAL_NULL,   -170141183460469231731687303715884105728).   %
%                                                                             %
%%%------------------------------------------------------------------decimals-%

% (needs to be defined above, as erl_integer is defined above:)
% -define(VOLT_DECIMALS_SCALESHIFT, 1000000000000).


%%%----------------------------------------------------------------------------
%%% @doc Erlang float to VoltDB binary decimals wire code.

%% @spec volt_decimal('null' | number()) -> <<_:128>>
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

%% @spec erl_number(<<_:128>>) -> number()
erl_number(?VOLT_DECIMAL_BINARY_TYPE(?VOLT_DECIMAL_NULL)=V) when is_binary(V) ->

    throw('this is the null value');

erl_number(?VOLT_DECIMAL_BINARY_TYPE(E)=V) when is_binary(V) ->

    case E rem ?VOLT_DECIMALS_SCALESHIFT of
        0 -> E div ?VOLT_DECIMALS_SCALESHIFT;
        _ -> E / ?VOLT_DECIMALS_SCALESHIFT
    end.

%%%----------------------------------------------------------------------------
%%% @doc VoltDB binary decimals wire code to Erlang integer, or null atom.

%% @spec erl_number_or_null(<<_:128>>) -> 'null' | number()
erl_number_or_null(?VOLT_DECIMAL_BINARY_TYPE(?VOLT_DECIMAL_NULL)=V)
    when is_binary(V) ->

    null;

erl_number_or_null(V) when is_binary(V) ->

    erl_number(V).


%%%----------------------------------------------------------------------------
%%% @doc VoltDB binary decimals wire code to Erlang float, failing on NULL.

%% @spec erl_float_from_decimal(<<_:128>>) -> float()
erl_float_from_decimal(?VOLT_DECIMAL_BINARY_TYPE(E)=V) when is_binary(V) ->

    E / ?VOLT_DECIMALS_SCALESHIFT.

%%%----------------------------------------------------------------------------
%%% @doc VoltDB binary decimals wire code to Erlang float, or null atom.

%% @spec erl_float_or_null_from_decimal(<<_:128>>) -> 'null' | float()
erl_float_or_null_from_decimal(?VOLT_DECIMAL_BINARY_TYPE(?VOLT_DECIMAL_NULL)=V)
    when is_binary(V) ->

    null;

erl_float_or_null_from_decimal(V) when is_binary(V) ->

    erl_float_from_decimal(V).

%%%----------------------------------------------------------------------------
%%% @doc VoltDB binary decimals wire code to Erlang integer, failing on NULL.

%% @spec erl_integer_from_decimal(<<_:128>>) -> integer()
erl_integer_from_decimal(?VOLT_DECIMAL_BINARY_TYPE(?VOLT_DECIMAL_NULL)=V)
    when is_binary(V) ->

    throw('this is the null value');

erl_integer_from_decimal(?VOLT_DECIMAL_BINARY_TYPE(E)=V) when is_binary(V) ->

    E div ?VOLT_DECIMALS_SCALESHIFT.

%%%----------------------------------------------------------------------------
%%% @doc VoltDB binary decimals wire code to Erlang integer, or null atom.

%% @spec erl_integer_or_null_from_decimal(binary()) -> 'null' | integer()
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
%   
%                                                                             %
%******************************************************************************

%%%-encode--------------------------------------------------------------arrays-

%%%----------------------------------------------------------------------------
%%% @doc Make Erlang List to VoltDB wire binary array, explicit type.
%%% @spec volt_array({voltarray, volttype(), list()}) -> <<_:24,_:_*8>>
volt_array({ voltarray, Type, List }) ->

    volt_array(Type, List);

%%%----------------------------------------------------------------------------
%%% @doc Make Erlang List to VoltDB wire binary array, guessing the type.
%%% @spec volt_array({voltarray, list()}) -> <<_:24,_:_*8>>
volt_array({ voltarray, List }) ->

    volt_array(List);

volt_array([H|_]=List) ->

    volt_array(volt_type(H), List);

volt_array([]) ->

    volt_array(?VOLT_INTEGER, []).


%%%----------------------------------------------------------------------------
%%% @doc Make Erlang List to VoltDB wire binary array, explicit type.
%%% @spec volt_array(volttype(), {voltarray, list()}) -> <<_:24,_:_*8>>
volt_array(Type, { voltarray, List }) ->

    volt_array(Type, List);

volt_array(Type, List) when is_list(List)->

    L = [ volt_any(Type, X) || X <- List ],
    S = length(L),
    B = list_to_binary(L),
    <<Type:8, S:16, B/binary>>.

%%%----------------------------------------------------------------------------
%%% @doc  Guessing types.

%% @spec volt_type('null' | binary() | list() | number() | date()) -> -99 | 1 | 5 | 8 | 9 | 11
volt_type(null)                       -> ?VOLT_NULL;
volt_type(X) when is_binary(X)        -> ?VOLT_STRING;
volt_type(X) when is_integer(X)       -> ?VOLT_INTEGER;
volt_type([H|_]) when is_integer(H)   -> ?VOLT_STRING; % sic
volt_type(X) when is_list(X)          -> ?VOLT_ARRAY;  % sic
volt_type(X) when is_float(X)         -> ?VOLT_FLOAT;

volt_type({{A,B,C},{D,E,F}}) when is_integer(A), is_integer(B),
    is_integer(C), is_integer(D), is_integer(E), is_integer(F) ->

    ?VOLT_TIMESTAMP.


%%%-decode--------------------------------------------------------------arrays-

%%% NOTE: Array need not be decoded in regular use.

%%%----------------------------------------------------------------------------
%%% @private not tested nor used
% % @doc EXCEPTION: TINY INT
-spec erl_binary_array(binary()) -> { binary(), binary() }.
%% @spec erl_binary_array(binary()) -> {binary(),binary()}
erl_binary_array(<<?VOLT_TINYINT:8, Count:32, Stream/binary>>=Bin) when is_binary(Bin) ->

    <<Binary:Count/binary, Rest/binary>> = Stream,
    { Binary, Rest }.

%%%----------------------------------------------------------------------------
%%% @private not tested nor used

%% @spec erl_array(<<_:32,_:_*16>>) -> ['nan' | 'negative_infinity' | 'nil' | 'null' | 'positive_infinity' | binary() | number() | {integer(),integer(),integer()}]
erl_array(<<?VOLT_TINYINT:8, Count:32, Stream>>=Bin) when is_binary(Bin) ->

    erl_elements(?VOLT_TINYINT, Count, Stream, Stream);

%%%----------------------------------------------------------------------------
%%% @private not tested nor used
% % @doc ALL OTHER

erl_array(<<Type:8, Count:16, Stream>>=Bin) when is_binary(Bin) ->

    erl_elements(Type, Count, Stream, Stream).

%%%----------------------------------------------------------------------------
%%% @private not tested nor used

%% @spec erl_elements(Type, Left, Parse, Full) -> ['nan' | 'negative_infinity' | 'nil' | 'null' | 'positive_infinity' | binary() | number() | time()]
%%	Type = volttype()
%% 	Left = non_neg_integer()
%% 	Parse = binary()
%% 	Full = binary()

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
%% @spec erl_binary_array_feed(binary()) -> {binary(),binary()}
erl_binary_array_feed(<<?VOLT_TINYINT:8, Count:32, Stream/binary>>=Bin) when is_binary(Bin) ->

    <<Binary:Count/binary, Rest/binary>> = Stream,
    { Binary, Rest }.

%%%----------------------------------------------------------------------------
%%% @private not tested nor used

%% @spec erl_array_feed(binary()) -> {list(),binary()}
erl_array_feed(<<?VOLT_TINYINT:8, Count:32, Stream/binary>>=Bin) when is_binary(Bin) ->

    <<Binary:Count/binary, Rest/binary>> = Stream,
    { binary_to_list(Binary), Rest };

%%%----------------------------------------------------------------------------
%%% @private not tested nor used
% % @doc ALL OTHER
erl_array_feed(<<Type:8, Count:16, Stream/binary>>=Bin) when is_binary(Bin) ->

    erl_elements_feed(Type, [], Count, Stream, Stream).

%%%----------------------------------------------------------------------------
%%% @private not tested nor used

%% @spec erl_elements_feed(Type, Result, Left, Stream, Full) -> [element()]
%%		Type = volttype()
%% 		Result = non_neg_integer()
%% 		Left = non_neg_integer()
%%  	Stream = binary()
%% 		Full = binary()

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
%% 
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
% 
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
% 

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

erl_float_feed    (<<Binary:?VOLT_FLOAT_BINARY,     Rest/binary>>) -> { erl_float_or_atom(Binary), Rest }. % 

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
%% @spec erl_decimal_feed(wiretype() + binary()) -> { 'null' | number(), Rest::binary() }

erl_decimal_feed  (<<Binary:?VOLT_DECIMAL_BINARY,   Rest/binary>>) -> { erl_number_or_null(Binary), Rest }. % 

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
%      values are still UTF-8 encoded. (
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

%%% 

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
%%% @type  id()           = { atom(), {{integer(),integer(),integer()},{integer(),integer(),integer()}}}.

%%%-encode-----------------------------------------------------------volttable-

%%%----------------------------------------------------------------------------
%%% @doc Encode Erlang terms to VoltTable rows, with status code 0.

%% @spec volt_table(volttable()) -> wire()
volt_table({ volttable, ColumnNames, ColumnTypes, Rows }) ->

    volt_table({ volttable, ColumnNames, ColumnTypes, Rows }, 0).

%%%----------------------------------------------------------------------------
%%% @doc Encode Erlang terms to VoltTable rows, with explicit status code.

%% @spec volt_table(volttable(), integer()) -> wire()
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

%% @spec volt_rows([volttype()],[voltrow()]) -> [wire()]
volt_rows(_, []) -> [];

volt_rows(ColumnTypes, [ { voltrow, Fields } | Rows ]) ->

    Row = list_to_binary(volt_fields(ColumnTypes, Fields)),
    Size = size(Row),
    [ <<Size:32, Row/binary>>  | volt_rows(ColumnTypes, Rows) ].


%%%----------------------------------------------------------------------------
%% @doc Converting two lists (Types, Contents) into a list of voltdb wire encoded binaries.
%% E.g.  [?VOLT_STRING , ?VOLT_STRING], ["Test", "Test"] ->  `[<<4:32,"Test">>,<<4:32,"Test">>]'
%% This returns a list of binaries. Converting it to one binary in the end is thought to be faster.

%% @spec volt_fields([volttype()], [any()]) -> wire()
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
%%%                     [ColumnName, ColumnName, ...]
%%%                     [ColumnType, ColumnType, ...]
%%%                     [ { voltrow, [Value, Value, ...] },
%%%                       { voltrow, [Value, Value, ...] },
%%%                       ...
%%%                     ]
%%%        }
%%%
%%% E.g.:
%%%
%%%        { volttable, [<<"TestCol1">>,<<"TestCol2">>,<<"TestCol3">>],
%%%                     [?VOLT_BIGINT,?VOLT_BIGINT,?VOLT_STRING],
%%%
%%%                     [ { voltrow, [42, 7, <<"volt">>] },
%%%                       { voltrow, [28,13, <<"vorlt">>] },
%%%                       { voltrow, [11, 1, <<"world">>] } ] },                         '''
%%%
%%% 

%%% @spec  erl_table(binary()) -> volttable()
erl_table(<<Length:32, MetaLength:32, _Status:8, ColumnCount:16, Stream/binary>>=Bin)
    when Length > 0, is_binary(Bin) ->

    % 

    % assert length of binary
    try Length = size(Bin) - 4
    catch
        What:Why -> throw({maybe_bad_total_size, Length + 4, size(Bin), What, Why})
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
    % 

    %% Scan Rows
    RowBinaries = [ RowBinary || <<Size:32, RowBinary:Size/binary>> <= RowsBinary ],
    try RowCount = length(RowBinaries) catch error:{badmatch,_} -> throw({bad_rowcount, 'could also be too small row length value', RowCount, length(RowBinaries), RowBinaries, RowsBinary}) end,

    %% Scan Rows
    Rows = try erl_table_rows(ColumnTypes, RowBinaries)
           catch error:E -> throw({row_parse_failure,E})
           end,

    { volttable, ColumnNames, ColumnTypes, Rows }.


%%% @spec  erl_table_feed(binary()) -> volttable()
erl_table_feed(<<Length:32, _/binary>>=Bin) when Length > 0, is_binary(Bin) ->

    UseLength = Length + 4,
    <<UseBin:UseLength/binary, Rest/binary>> = Bin,

    % debug io:format("erl_table_feed ~n~w~n~w", [UseBin, Rest]),

    { erl_table(UseBin), Rest }.

    % 


%%% @doc   Parse a VoltTable from VoltDB wire protocol data, return a once-nested list.
%%% @spec  erl_plaintable(binary()) -> volttable()
erl_plaintable(<<Length:32, MetaLength:32, _Status:8, ColumnCount:16, Stream/binary>>=Bin)
    when Length > 0, is_binary(Bin) ->

    % 

    % assert length of binary
    try Length = size(Bin) - 4
    catch
        What:Why -> throw({maybe_bad_total_size, Length + 4, size(Bin), What, Why})
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

%% @spec erl_table_rows([byte()],[binary()]) -> [{'voltrow',[any()]}]
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

%% @spec erl_plaintable_rows([volttype()], [binary()]) -> [['nan' | 'negative_infinity' | 'nil' | 'null' | 'positive_infinity' | binary() | number() | date()]]
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


%% @spec erl_table_fields([byte()],binary()) -> ['nan' | 'negative_infinity' | 'nil' | 'null' | 'positive_infinity' | binary() | number() | date()]
erl_table_fields([], <<>>) ->

    [];

erl_table_fields([ Type | TypesTail ], RowBinaryStream ) ->

    % debug: io:format("~n~p~n", [RowBinaryStream]),

    { Value, RowBinaryRest } = erl_any(Type, RowBinaryStream),

    [ Value | erl_table_fields(TypesTail, RowBinaryRest ) ].


%*****************************************************************************%
%                           Result Access Function                            %
%*****************************************************************************%
%%% @doc Get the status number from the result.
%% @spec get_status(voltresponse() | {result,voltresponse()}) -> integer()

get_status({result, Response}) ->

    get_status(Response);

get_status({ voltresponse, {_,_,Status,_,_,_,_,_}, _ }) ->

    Status.

%%% @doc Get the status string from the result.
%% @spec get_statusstring(voltresponse() | {result,voltresponse()}) -> list()

get_statusstring({result, Response}) ->

    get_statusstring(Response);

get_statusstring({ voltresponse, {_,_,_,StatusString,_,_,_,_}, _ }) ->

    StatusString.


%%%----------------------------------------------------------------------------
%%% @doc Get a table from the result list by list position. First == 1.
%% @spec get_table(voltresponse() | {result,voltresponse()}, pos_integer()) -> volttable()

get_table({result, Response}, Pos) ->

    get_table(Response, Pos);

get_table({ voltresponse, _, Tables }, Pos) ->

    lists:nth(Pos, Tables).

%%% @doc Get all tables out of a given response.
%%% @spec get_table_list(volttable()) -> [voltrow()]

get_table_list({result, { voltresponse, _, Tables }}) ->

    Tables;

get_table_list({ voltresponse, _, Tables }) ->

    Tables.


%%%----------------------------------------------------------------------------
%%% @doc Get a row out of a given table, by index number. First == 1.
%% @spec get_row(volttable(),any()) -> throw(empty_result_table)

get_row({ volttable, _, _, []}, _) ->

    throw(empty_result_table);

%% @spec get_row(volttable(),pos_integer()) -> voltrow()

get_row({ volttable, _, _, List}, Pos) when is_list(List) ->

    lists:nth(Pos, List).

%%% @doc Get all rows out of a given table.
%%% @spec get_row_list(volttable()) -> [voltrow()]

get_row_list({ volttable, _, _, List}) when is_list(List) ->

    List.

%%%----------------------------------------------------------------------------
%%% @doc Get a row out of a given table as record, by index number. First == 1.
%%% @spec get_record(volttable(), RecordTag, Pos::pos_integer()) -> record()

get_record({ volttable, _, _, List}, RecordTag, Pos) when is_list(List) ->

    { voltrow, FieldList } = lists:nth(Pos, List),
    list_to_tuple([ RecordTag | FieldList ]).


%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row, by index number. First == 1.
%%% @spec get_field(voltrow(), Pos::pos_integer()) -> term()

get_field({ voltrow, List }, Pos) ->

    lists:nth(Pos, List).


%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as string, by index number. First == 1.
%%% @spec get_string(voltrow(), Pos::pos_integer()) -> list()

get_string({ voltrow, _ }=VoltRow, Pos) when is_integer(Pos), Pos < 1 ->

    throw({out_of_range, Pos, lt_one, VoltRow });

get_string({ voltrow, List }, Pos) when is_integer(Pos), is_list(List), Pos > length(List) ->

    throw({out_of_range, Pos, length(List), List});

get_string({ voltrow, List }, Pos) when is_integer(Pos), is_list(List) ->

    vecho(?V, "get_string( { voltrow, ~w }, ~w )", [List, Pos]),

    to_list(lists:nth(Pos, List));

    % 

% Note: RecPos can be expected to be = Pos+1, as it observes the preceding tag atom.
% 

get_string(Record, RecPos) when is_integer(RecPos), is_tuple(Record), RecPos < 2 ->

    throw({out_of_range, RecPos, lt_two, Record });

get_string(Record, RecPos) when is_integer(RecPos), is_tuple(Record), RecPos > tuple_size(Record) ->

    throw({out_of_range, RecPos, tuple_size(Record) -1, Record});

get_string(Record, RecPos) when is_integer(RecPos), is_tuple(Record) ->

    vecho(?V, "get_string( ~w, ~w )", [Record, RecPos]),

    to_list(element(RecPos, Record)).

    % 

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as string, by index number; null when Pos bad.
%%% @spec get_string_or_null(voltrow(), Pos::pos_integer()) -> 'null' | list()

get_string_or_null({ voltrow, _ }, Pos) when is_integer(Pos), Pos < 1 ->

    null;

get_string_or_null({ voltrow, List }, Pos) when is_integer(Pos), is_list(List), Pos > length(List) ->

    null;

get_string_or_null({ voltrow, List }, Pos) when is_integer(Pos), is_list(List) ->

    vecho(?V, "get_string_or_null( { voltrow, ~w }, ~w )", [List, Pos]),

    try
        to_list(lists:nth(Pos, List))
    catch
        _:_ -> null
    end;

    % 

% Note: RecPos can be expected to be = Pos+1, as it observes the preceding tag atom.
% 

get_string_or_null(Record, RecPos) when is_integer(RecPos), is_tuple(Record), RecPos < 2 ->

    null;

get_string_or_null(Record, RecPos) when is_integer(RecPos), is_tuple(Record), RecPos > tuple_size(Record) ->

    null;

get_string_or_null(Record, RecPos) when is_integer(RecPos), is_tuple(Record) ->

    vecho(?V, "get_string_or_null( ~w, ~w )", [Record, RecPos]),

    try
        to_list(element(RecPos, Record))
    catch
        _:_ -> null
    end.

    % 

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as string, by column name; error when not found.
%%% The complete VoltTable is used to exract column names from it.
%%% @spec get_string(voltrow(), volttable(), Pos::pos_integer()) -> binary()

get_string(VoltRow, VoltTable, Name) when is_list(Name) ->

    get_string(VoltRow, VoltTable, list_to_binary(Name));

get_string({ voltrow, _ }=VoltRow, VoltTable, Name) when is_binary(Name)->

    { volttable, ColumnNames, _ColumnTypes, _Rows } = VoltTable,

    Index = list_ord(Name, ColumnNames),
    Index /= nil orelse erlang:error({bad_column_name, Name, ColumnNames}),

    get_string(VoltRow, Index).


%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as string, by column name; null when not found.
%%% The complete VoltTable is used to exract column names from it.
%%% @spec get_string_or_null(voltrow(), volttable(), Pos::pos_integer()) -> binary()

get_string_or_null(VoltRow, VoltTable, Name) when is_list(Name) ->

    get_string_or_null(VoltRow, VoltTable, list_to_binary(Name));

get_string_or_null({ voltrow, _ }=VoltRow, VoltTable, Name) when is_binary(Name)->

    { volttable, ColumnNames, _ColumnTypes, _Rows } = VoltTable,

    case Index = list_ord(Name, ColumnNames) of
        nil -> null;
        _ -> get_string_or_null(VoltRow, Index)
    end.

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as integer, by index number. First == 1.
%%% @spec get_integer(voltrow(), Pos::pos_integer()) -> list()

get_integer({ voltrow, _ }=VoltRow, Pos) when is_integer(Pos), Pos < 1 ->

    throw({out_of_range, Pos, lt_one, VoltRow });

get_integer({ voltrow, List }, Pos) when is_integer(Pos), is_list(List), Pos > length(List) ->

    throw({out_of_range, Pos, length(List), List});

get_integer({ voltrow, List }, Pos) when is_integer(Pos), is_list(List) ->

    vecho(?V, "get_integer( { voltrow, ~w }, ~w )", [List, Pos]),

    to_integer(lists:nth(Pos, List));

% Note: RecPos can be expected to be = Pos+1, as it observes the preceding tag atom.
% 

get_integer(Record, RecPos) when is_integer(RecPos), is_tuple(Record), RecPos < 2 ->

    throw({out_of_range, RecPos, lt_two, Record });

get_integer(Record, RecPos) when is_integer(RecPos), is_tuple(Record), RecPos > tuple_size(Record) ->

    throw({out_of_range, RecPos, tuple_size(Record) -1, Record});

get_integer(Record, RecPos) when is_integer(RecPos), is_tuple(Record) ->

    vecho(?V, "get_integer( ~w, ~w )", [Record, RecPos]),

    to_integer(element(RecPos, Record)).

    % 

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as integer, by index number; null when Pos bad.
%%% @spec get_integer_or_null(voltrow(), Pos::pos_integer()) -> 'null' | list()

get_integer_or_null({ voltrow, _ }, Pos) when is_integer(Pos), Pos < 1 ->

    null;

get_integer_or_null({ voltrow, List }, Pos) when is_integer(Pos), is_list(List), Pos > length(List) ->

    null;

get_integer_or_null({ voltrow, List }, Pos) when is_integer(Pos), is_list(List) ->

    vecho(?V, "get_integer_or_null( { voltrow, ~w }, ~w )", [List, Pos]),

    try
        to_integer(lists:nth(Pos, List))
    catch
        _:_ -> null
    end;

    % 

% Note: RecPos can be expected to be = Pos+1, as it observes the preceding tag atom.
% 

get_integer_or_null(Record, RecPos) when is_integer(RecPos), is_tuple(Record), RecPos < 2 ->

    null;

get_integer_or_null(Record, RecPos) when is_integer(RecPos), is_tuple(Record), RecPos > tuple_size(Record) ->

    null;

get_integer_or_null(Record, RecPos) when is_integer(RecPos), is_tuple(Record) ->

    vecho(?V, "get_integer_or_null( ~w, ~w )", [Record, RecPos]),

    try
        to_integer(element(RecPos, Record))
    catch
        _:_ -> null
    end.

    % 

%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as integer, by column name; error when not found.
%%% The complete VoltTable is used to exract column names from it.
%%% @spec get_integer(voltrow(), volttable(), Pos::pos_integer()) -> binary()

get_integer(VoltRow, VoltTable, Name) when is_list(Name) ->

    get_integer(VoltRow, VoltTable, list_to_binary(Name));

get_integer({ voltrow, _ }=VoltRow, VoltTable, Name) when is_binary(Name)->

    { volttable, ColumnNames, _ColumnTypes, _Rows } = VoltTable,

    Index = list_ord(Name, ColumnNames),
    Index /= nil orelse erlang:error({bad_column_name, Name, ColumnNames}),

    get_integer(VoltRow, Index).


%%%----------------------------------------------------------------------------
%%% @doc Get a field out of a row as integer, by column name; null when not found.
%%% The complete VoltTable is used to exract column names from it.
%%% @spec get_integer_or_null(voltrow(), volttable(), Pos::pos_integer()) -> binary()

get_integer_or_null(VoltRow, VoltTable, Name) when is_list(Name) ->

    get_integer_or_null(VoltRow, VoltTable, list_to_binary(Name));

get_integer_or_null({ voltrow, _ }=VoltRow, VoltTable, Name) when is_binary(Name)->

    { volttable, ColumnNames, _ColumnTypes, _Rows } = VoltTable,

    case Index = list_ord(Name, ColumnNames) of
        nil -> null;
        _ -> get_integer_or_null(VoltRow, Index)
    end.


%%%----------------------------------------------------------------------------
%%% @doc Return index number of a given list element.

%% @spec list_ord(any(),maybe_improper_list()) -> 'nil' | pos_integer()
list_ord(Searched, List) when is_list(List) ->

    list_ord(Searched, List, 1).

%% @spec list_ord(any(),maybe_improper_list(),pos_integer()) -> 'nil' | pos_integer()
list_ord(_, [], _) -> nil;

list_ord(Searched, [ Searched | _ ], Count) ->

    Count;

list_ord(Searched, [ _ | Tail ], Count) ->

    list_ord(Searched, Tail, Count + 1).

%%%----------------------------------------------------------------------------
%%% @doc Convert binaries, integers or floats to 'strings'.

%% @spec to_list(binary() | maybe_improper_list() | number()) -> maybe_improper_list()
to_list(L) when is_list(L) -> L;
to_list(L) when is_binary(L) -> binary_to_list(L);
to_list(L) when is_integer(L) -> integer_to_list(L);
to_list(L) when is_float(L) -> float_to_list(L).

%%%----------------------------------------------------------------------------
%%% @doc Convert binaries, strings (lists) or floats to integer.

%% @spec to_integer(binary() | maybe_improper_list() | number()) -> integer() | error()
to_integer(L) when is_list(L) -> list_to_integer(L);
to_integer(L) when is_binary(L) -> list_to_integer(binary_to_list(L));
to_integer(L) when is_integer(L) -> L;
to_integer(L) when is_float(L) -> trunc(L).

% 

%%%----------------------------------------------------------------------------
%%% @doc get roundtrip time in milliseconds of a response according to server

roundtrip({ result, { voltresponse, {_,_,_,_,_,_,_, RoundTripTime }, _ }}) ->

    RoundTripTime;

roundtrip({ voltresponse, {_,_,_,_,_,_,_, RoundTripTime }, _ }) ->

    RoundTripTime.


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
%%% Connection record structure
%%%----------------------------------------------------------------------------

%%%----------------------------------------------------------------------------
%%% @doc  Client opens connection and logs in to the VoltDB server cluster.
%%% Use defaults: localhost, port 21212, client name "program" and password "password".
%%% Login name and password can be irrelevant, check the VoltDB docs.

-define(PORT, 21212).

%% @spec create_connection() -> inet:socket()
create_connection() ->

    create_connection("localhost", ?PORT, "program", "password").

% 

%%%----------------------------------------------------------------------------
%%% @doc  Client opens connection and logs in to the VoltDB server cluster.
%%% Use specified host and defaults: port 21212, client name "program" and password "password".
%%% Login name and password can be irrelevant, check the VoltDB docs.

%% @spec create_connection(host()) -> inet:socket()
create_connection(Host) ->

    create_connection(Host, ?PORT, "program", "password").

%%%----------------------------------------------------------------------------
%%% @doc  Client opens connection and logs in to the VoltDB server cluster.
%%% Use specified host, login name and password, default port 21212.
%%% Login name and password can be irrelevant, check the VoltDB docs.

%% @spec create_connection(host(),lobstring(),lobstring(),lobstring()) -> inet:socket()
create_connection(Host, ?PORT, Login, Password) ->

    create_connection(Host, ?PORT, Login, Password, "database", true, 100000, 100000, 10000, blocking).
    % 

%%%----------------------------------------------------------------------------
%%% @doc  Client opens connection and logs in to the VoltDB server cluster.
%%% Specify host, port, login name and password.
%%% Login name and password can be irrelevant, check the VoltDB docs.

%% @spec create_connection(host(),portnumber(),lobstring(),lobstring(),lobstring(),true|false,integer(),integer(),integer(),atom()) ->inet:socket()
create_connection(Host, Port, User, Password, Database, Nagle, SendBuffer, ReceiveBuffer, SendTimeout, Blocking) ->

    {ok, Socket} = connect(Host, Port, Nagle, SendBuffer, ReceiveBuffer, SendTimeout),
    {ok, _ } = login(Socket, User, Password, Database, Blocking),
    Socket.

%%%----------------------------------------------------------------------------
%%% @private  Workhorse: Client opens connection to the VoltDB server cluster.
%:%----------------------------------------------------------------------------
%:% Erlang docs:
%:%
%:% send_timeout: Specifies a longest time to wait for a send operation
%:% to be accepted by the underlying TCP stack. When the limit is
%:% exceeded, the send operation will return {error,timeout}. How much
%:% of a packet that actually got sent is unknown, why the socket should
%:% be closed whenever a timeout has occurred (see send_timeout_close).
%:% Default is infinity.
%:%
%:% buffer size: Determines the size of the user-level software
%:% buffer used by the driver. Not to be confused with sndbuf and recbuf
%:% options which correspond to the kernel socket buffers. It is
%:% recommended to have val(buffer) >= max(val(sndbuf),val(recbuf)). In
%:% fact, the val(buffer) is automatically set to the above maximum when
%:% sndbuf or recbuf values are set.
%:%
%:% recbuf: Gives the size of the receive buffer to use for the socket.
%:%
%:% sndbuf: Gives the size of the send buffer to use for the socket.
%:%
%:% nodelay: If true, the TCP_NODELAY option is turned on for the
%:% socket, which means that even small amounts of data will be sent
%:% immediately.
%:%
%:% delay_send: Normally, when an Erlang process sends to a socket, the
%:% driver will try to immediately send the data. If that fails, the
%:% driver will use any means available to queue up the message to be
%:% sent whenever the operating system says it can handle it. Setting
%:% {delay_send, true} will make all messages queue up. This makes the
%:% messages actually sent onto the network be larger but fewer. The
%:% option actually affects the scheduling of send requests versus
%:% Erlang processes instead of changing any real property of the
%:% socket. Needless to say it is an implementation specific option.
%:% Default is false.
%:%
%:% send_timeout_close: Used together with send_timeout to specify
%:% whether the socket will be automatically closed when the send
%:% operation returns {error,timeout}. The recommended setting is true
%:% which will automatically close the socket. Default is false due to
%:% backward compatibility.
%:%
%:% From: http://www.erlang.org/doc/man/inet.html
%:%----------------------------------------------------------------------------
%% @spec connect(host(), portnumber(), true|false, integer(), integer(), integer()) -> {'ok',inet:socket()} | no_return()
connect(Host, Port, Nagle, SendBuffer, ReceiveBuffer, SendTimeout) ->

	Opts = [ {delay_send, Nagle},
			 {nodelay, not Nagle},
			 {buffer, max(SendBuffer, ReceiveBuffer)},
			 {sndbuf, SendBuffer},
			 {recbuf, ReceiveBuffer},
			 {send_timeout, SendTimeout} ], % Dialyzer does not accept infinity. 

    connect(Host, Port, Opts).

connect(Host, Port, Opts) ->

    SockOpts = [ binary, {packet, 4} ] ++ Opts,
    

    {ok, Socket} =
        try
            case gen_tcp:connect(Host, Port, SockOpts) of
                {error, econnrefused}=E ->
                    erlerror("Connection refused, server down?", [], notrace),
                    throw(E);
                {ok, Sock}
                    -> {ok, Sock};
                Else ->
                    throw(Else)
            end
        catch
            throw:{error, econnrefused}=Reason ->
                throw({erlvolt_tcp_connection_refused, Reason});
            Type:Reason ->
                erlerror("TCP connect in failed (2):~n~p.", [Reason]),
                throw({erlvolt_tcp_connection_failed, Type, Reason})
	    end,
    {ok, Socket}.

%%%----------------------------------------------------------------------------
%%% @doc  Close connection.
%%% May throw {error, Reason}

%% @spec close(inet:socket()) -> 'ok'
close(Socket) ->

	gen_tcp:close(Socket).


%*****************************************************************************%
%                                                                             %
%                             Message Header                                  %
%                                                                             %
%*****************************************************************************%
%
%    @doc VoltDB message header bytes - actually protocol version only.     ```
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
%    
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
%%% @spec erl_header(binary()) -> { ProtocolVersion::integer(), Size::non_neg_integer() }
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


%% @spec login(port(),lobstring(),lobstring(),lobstring()) -> {'ok','async'} | string()
login(Socket, Name, Password, Database) ->

    login(Socket, Name, Password, Database, blocking).


%%%----------------------------------------------------------------------------
%%% @doc Client log in to the VoltDB server cluster, blocking or non-blocking.

%% @spec login(port(), lobstring(), lobstring(), lobstring(), blocking | nonblocking) -> {'ok','async'} | string()
login(Socket, Name, Password, Database, Mode) ->

    gen_tcp:send(Socket, L=volt_login(Name, Password, Database)),
    vecho(?V, "Login: ~w", [L]),

    case Mode of

        blocking ->

            %% NOTE: there can be {call..} messages from
            %% the call_procedure functions piling up in the message
            %% in box while we are waiting.
            receive

                {tcp,Socket,ResultBin} ->

                    erl_login_response(ResultBin);

                {tcp_closed,_}=D ->

                    erlang:error({ erlvolt_server_denied, D })

            end;

        nonblocking -> { ok, async }

    end.


%%%----------------------------------------------------------------------------
%%% @doc  Make VoltDB wire binary for login message from name and password.

%% @spec volt_login('null' | binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | byte(),binary() | []),binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | byte(),binary() | [])) -> binary()
volt_login(Name, Password, Database) ->

    HeaderBin   = volt_header(),          % all return binaries
    DatabaseBin = volt_string(Database),
    NameBin     = volt_string(Name),
    HashBin     = volt_hash(Password),

    <<HeaderBin/binary, DatabaseBin/binary, NameBin/binary, HashBin/binary>>.


%%%----------------------------------------------------------------------------
%%% @doc  Make SHA-1 hash for client log in to the VoltDB server cluster.

%% @spec volt_hash(binary() | maybe_improper_list(binary() | maybe_improper_list(any(),binary() | []) | byte(),binary() | [])) -> binary()
volt_hash(Secret) when is_list(Secret) ->

    volt_hash(list_to_binary(Secret));

volt_hash(Secret) when is_binary(Secret) ->

    crypto:start(),
    crypto:sha(Secret).


%%%

%% @spec erl_login_response(<<_:64,_:_*8>>) -> {'ok',{0,byte(),non_neg_integer(),non_neg_integer(),non_neg_integer(),non_neg_integer(),binary()}}
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

    erltrace("#7b  Logged in. Server buildstring: ~s", [BuildString]),

    { ok, { Protocol, AuthCode, HostID, ConnectionID, ClusterStart, LeaderIP, BuildString }}.


%*****************************************************************************%
%                                                                             %
%                         Invoke Stored Procedures                            %
%                                                                             %
%*****************************************************************************%
%                                                                             %
%    A request to invoke a stored procedure identifies the procedure to in-   %
%    voke by name, the parameters to pass to the procedure,  and an 8 byte    %
%    piece of client data  that will be returned  with the response to the    %
%    invocation request.  A client does not need to wait for a response to    %
%    a request to continue sending requests. The server will use TCP back-    %
%    pressure to avoid running out of memory  when a client sends too many    %
%    invocations for the server to handle.                                    %
%                                                                             %
%    --------------------------------------------------------------------+    %
%    |                    ... Message Header ...                         |    %
%    ---------------------------------------------------------------------    %
%    + Procedure name:        String                                     |    %
%    + Client data:           Binary                                     |    %
%    + Parameters:            Parameter Set                              |    %
%    ---------------------------------------------------------------------    %
%                                                                             %
%    --- pg. 13, VoltDB Client Wire Protocol Version 0, 05/05/10 ---          %
%                                                                             %
%                                                                             %
%*****************************************************************************%

%%%----------------------------------------------------------------------------
%%% Synchronous / blocking
%%%----------------------------------------------------------------------------

%%%----------------------------------------------------------------------------
%%% @ doc  Send a stored procedure call to the VoltDB server.

-define(TIMEOUT, 1000). % ms
-define(DEFAULT_CLIENT_TAG, <<1:(8*8)>>).

%%%----------------------------------------------------------------------------
%%% @ doc  Send a stored procedure call to the VoltDB server.

%% returns -> ok | {error, Reason} directly from gen_tcp:send() or re-throws exception
%% @spec call_procedure(atom(),port(),lobstring(),list(any()),binary()) -> 'ok' | {'error',atom()}
call_procedure(ConnId, Socket, ProcedureName, Parameters, CallId) ->

    erltrace("#16  erlvolt_wire:call_procedure/5"),
	try
        Wire = erlvolt_wire:volt_invoke(ProcedureName, Parameters, CallId),
        erltrace("#16a  binary: ~w", [Wire]),
	    gen_tcp:send(Socket, Wire)
	catch
		What:Why ->
           erlerror("Procedure call failed: ~w, ~w~nCall: ~p ~p~nCall ID: ~w~nConn id: ~w~nSocket: ~w.", [What, Why, ProcedureName, Parameters, CallId, ConnId, Socket]),
		    throw({ erlvolt_asynch_proc_call_failed,
		            { What, Why, ConnId, Socket, ProcedureName, Parameters, CallId }})
	end.

%% @spec volt_invoke(lobstring(), list(), binary()) -> binary()
volt_invoke(ProcedureName, Parameters, <<ClientData:8/binary>>) when is_binary(ClientData), is_list(Parameters) ->

    H  = volt_header(),           % all return binaries
    N  = volt_string(ProcedureName),
    P  = volt_parameters(Parameters),

    Bin = <<H/binary, N/binary, ClientData:8/binary, P/binary>>,

    vecho(?V, "~nSend invoke: ~w~n", [Bin]),

    Bin.

%*****************************************************************************%
%                                                                             %
%                             Client Parameters                               %
%                                                                             %
%*****************************************************************************%
%                                                                             %
%    A  parameter set  contains all the parameters to be passed to a stored   %
%    procedure and it is one of the structures bundled inside a stored pro-   %
%    cedure invocation request.  The  first value of a  parameter set  is a   %
%    Short indicating  the number of parameters that follow.  The following   %
%    values are  a series of  <wire type, value> pairs.  Each value is pre-   %
%    ceded by its  wire type represented as  a Byte.  NULL is a  valid wire   %
%    type and value and it is not followed by any additional value.           %
%                                                                             %
%    Arrays are preceded by the wire type -99  and the array value contains   %
%    the type of the array elements as well as the number of elements  (see   %
%    Array type).  A parameter set  cannot  contain  a nested parameter set   %
%    (there is no wire type for parameter set).                               %
%                                                                             %
%                                                                             %
%    Parameter Set                                                            %
%                                                                             %
%    ---------------------------------------------------------------------    %
%    + Parameter Count:       Short     2                                |    %
%    --------------------------------------------------------------------+    %
%    |                    ... Parameters ...                             |    %
%    ---------------------------------------------------------------------    %
%                                                                             %
%                                                                             %
%    Parameter                                                                %
%                                                                             %
%    ---------------------------------------------------------------------    %
%    + Parameter Type:       Byte       1                                |    %
%    --------------------------------------------------------------------+    %
%    + Parameter [Value]:    Binary                                      |    %
%    --------------------------------------------------------------------+    %
%                                                                             %
%    --- pg. 7, VoltDB Client Wire Protocol Version 0, 05/05/10 ---           %
%                                                                             %
%                                                                             %
%*****************************************************************************%

%%%----------------------------------------------------------------------------
%%% @doc  Encode parameters for Stored Procedure Calls

%% @spec volt_parameters({ voltparameters, Parameters::list(any()) }) -> binary()
volt_parameters({ voltparameters, Parameters }) when is_list(Parameters) ->

    volt_parameters(Parameters);

%% @spec volt_parameters(Parameters::list(any())) -> binary()
volt_parameters(Parameters) when is_list(Parameters) ->

    List = [ volt_parameter(Parameter) || Parameter <- Parameters ],
    % debug io:format("~p",[List]),
    Count = length(List),
    list_to_binary([<<Count:16>> | List ]).

%%%----------------------------------------------------------------------------
%%% @doc  Encode, with type, one parameter for Stored Procedure Calls

%% @spec volt_parameter(any() | {volttype(),any()}) -> wire()
volt_parameter({ Type, Value }) ->

    V = volt_any(Type, Value),

    <<Type:8, V/binary>>;

volt_parameter(Value) when is_integer(Value) ->

    volt_parameter({?VOLT_INTINT, Value});

volt_parameter(Value) when is_list(Value) ->

    volt_parameter({?VOLT_STRING, Value});

volt_parameter(Value) when is_binary(Value) ->

    volt_parameter({?VOLT_STRING, Value});

volt_parameter(Value) ->

    erlang:error({missing_parameter_type, Value}).

    % debug io:format("Trying to make volt parameter string from: ~w~n~n~p~n",[Value,erlang:get_stacktrace()]),

    % was: volt_parameter({?VOLT_STRING, Value}).


%*****************************************************************************%
%                                                                             %
%                         Decode Invocation Result                            %
%                                                                             %
%*****************************************************************************%
%                                                                             %
%   An invocation response contains the results  of the server's attempt to   %
%   execute the stored procedure. The response includes optional fields and   %
%   the  first byte  after the header  is used to indicate,  which optional   %
%   fields are present.  The status string,  application status string, and   %
%   serializable exception  are all  optional fields.  Bit 7  indicates the   %
%   presence of a serializable exception, bit 6 indicates the presence of a   %
%   status string, and bit 8 indicates the presence of an app status string   %
%   The  serializable exception  that can be included  in some responses is   %
%   currently  not a part of the wire protocol.  The exception length value   %
%   should  be used  to skip  exceptions  if  they are present.  The status   %
%   string is used to return any  human readable information  the server or   %
%   stored procedure wants to return with the response. The app status code   %
%   and app status string can be set by application code from within stored   %
%   procedures and is returned with the response.                             %
%                                                                             %
%    --- pg. 15, VoltDB Client Wire Protocol Version 0, 05/05/10 ---          %
%                                                                             %
%                                                                             %
%   .. there is a new 4 byte latency field in the client response message     %
%   after the status byte. This field measures the roundtrip latency mea-     %
%   sured by the initiating node in the cluster measuring when the trans-     %
%   action was initiated and when the response was received and forwarded     %
%   to the client.                                                            %
%                                                                             %
%    --- Ariel Weisberg, VoltDB, email 04/28/10                               %
%                                                                             %
%                                                                             %
%   ClientResponseImpl.java Revision 495 of 05/24/10:                         %
%                                                                             %
%   181  public void writeExternal(FastSerializer out) throws IOException {   %
%   182         assert setProperly;                                           %
%   183         out.writeByte(0);//version                                    %
%   184         out.writeLong(clientHandle);                                  %
%   185         byte presentFields = 0;                                       %
%   186         if (appStatusString != null) {                                %
%   187             presentFields |= 1 << 7;                                  %
%   188         }                                                             %
%   189         if (m_exception != null) {                                    %
%   190             presentFields |= 1 << 6;                                  %
%   191         }                                                             %
%   192         if (statusString != null) {                                   %
%   193             presentFields |= 1 << 5;                                  %
%   194         }                                                             %
%   195         out.writeByte(presentFields);                                 %
%   196         out.write(status);                                            %
%   197         if (statusString != null) {                                   %
%   198             out.writeString(statusString);                            %
%   199         }                                                             %
%   200         out.write(appStatus);                                         %
%   201         if (appStatusString != null) {                                %
%   202             out.writeString(appStatusString);                         %
%   203         }                                                             %
%   204         out.writeInt(clusterRoundTripTime);                           %
%   205         if (m_exception != null) {                                    %
%   206             final ByteBuffer b =                                      %
%   206               ByteBuffer.allocate(m_exception.getSerializedSize());   %
%   207             m_exception.serializeToBuffer(b);                         %
%   208             out.write(b.array());                                     %
%   209         }                                                             %
%   210         out.writeArray(results);                                      %
%   211  }                                                                    %
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
%%%     { voltresponse,
%%%
%%%       {
%%%         Protocol,
%%%         ClientData,
%%%         Status,
%%%         StatusString,
%%%         AppStatus,
%%%         AppStatusString,
%%%         SerializedException,
%%%         RoundTripTime
%%%       },
%%%
%%%       [ volttable() ]
%%%
%%%     }.
%%% @end
%%%----------------------------------------------------------------------------
%%% @doc Parse VoltDB server response  to stored procedure invocation.
%%% @spec erl_response(wire()) -> voltresponse()

erl_response(W) ->

    % erltrace("Response received: ~w", [W])
    % vecho(?V, "~nResponse received: ~w~n", [W])

    { Protocol, _ } = erl_header(W),

    <<_:?VOLT_HEADER_TYPE, AfterHeader/binary>> = W, % 

    <<ClientData:8/binary,
      HasAppStatusString:1/integer,
      HasSerializedException:1/integer,
      HasStatusString:1/integer,
      _:5/integer,
      Status:8/signed,
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

    case HasStatusString of
        1 -> erltrace("Status: ~s", [StatusString]);
        _ -> ok
    end,
    case HasAppStatusString of
        1 -> erltrace("App Status: ~s", [AppStatusString]);
        _ -> ok
    end,

    {{ voltresponse,

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

    },<<>>}.

%%% @doc Parse VoltDB server response  to stored procedure invocation.
%%% @spec brief(wire()) -> ok.
brief( {result, {voltresponse,_,_}=V}) ->

    brief(V);

%%% @spec brief(voltresponse()) -> ok.
brief( {voltresponse,_,_}=V) ->

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

    } = V,

    TableCount = length(Tables),

    io:format(
        "Result                 ~n" ++
        "-----------------------~n" ++
        "Protocol:            ~p~n" ++
        "ClientData:          ~p~n" ++
        "Status:              ~p~n" ++
        "StatusString:        ~p~n" ++
        "AppStatus:           ~p~n" ++
        "AppStatusString:     ~p~n" ++
        "SerializedException: ~p~n" ++
        "RoundTripTime:       ~p~n" ++
        "Table Count:         ~p~n",
        [Protocol,
         ClientData,
         Status,
         StatusString,
         AppStatus,
         AppStatusString,
         SerializedException,
         RoundTripTime,
         TableCount]),

    [ io:format(
        "Table                 ~n" ++
        "......................~n" ++
        "Column Names:       ~p~n" ++
        "Column Types:       ~p~n" ++
        "Rows:               ~p~n",
        [ColumnNames,
         ColumnTypes,
         Rows])
    ||
        { volttable, ColumnNames, ColumnTypes, Rows } <- Tables
    ].

%%% @spec dump(voltresult()) -> ok.
dump( {result, {voltresponse,_,_}=V}) ->

    dump(V);

%%% @spec dump(voltresponse()) -> ok.
dump( {voltresponse,_,_}=V) ->

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

    } = V,

    TableCount = length(Tables),

    io:format(
        "Result                 ~n" ++
        "-----------------------~n" ++
        "Protocol:            ~p~n" ++
        "ClientData:          ~p~n" ++
        "Status:              ~p~n" ++
        "StatusString:        ~p~n" ++
        "AppStatus:           ~p~n" ++
        "AppStatusString:     ~p~n" ++
        "SerializedException: ~p~n" ++
        "RoundTripTime:       ~p~n" ++
        "Table Count:         ~p~n",
        [Protocol,
         ClientData,
         Status,
         StatusString,
         AppStatus,
         AppStatusString,
         SerializedException,
         RoundTripTime,
         TableCount]),

    [ dump(T) || T <- Tables ];

dump({ volttable, ColumnNames, ColumnTypes, Rows }) ->

    io:format(
        "Table                 ~n" ++
        "::::::::::::::::::::::~n" ++
        "Column Names:       ~p~n" ++
        "Column Types:       ~p~n" ++
        "Rows:               ~p~n" ++
        "......................~n",
        [ColumnNames,
         ColumnTypes,
         Rows]),

    [ dump(ColumnNames, ColumnTypes, Row) || Row <- Rows ].

dump(ColumnNames, ColumnTypes, { voltrow, Fields }) ->

    [ dump(N,T,V) || {N,T,V} <- lists:zip3(ColumnNames, ColumnTypes, Fields) ];

dump(Name, _, Value) ->

    io:format("~s: ~p~n", [Name, Value]).


show( {result, {voltresponse,_,_}=V}) ->

    show(V);

%%% @spec show(voltresponse()) -> ok.
show( {voltresponse,_,Tables}) ->

    [ show(T) || T <- Tables ];

show({ volttable, ColumnNames, ColumnTypes, Rows }) ->

    [ show(ColumnNames, ColumnTypes, Row) || Row <- Rows ].

show(ColumnNames, ColumnTypes, { voltrow, Fields }) ->

    [ show(N,T,V) || {N,T,V} <- lists:zip3(ColumnNames, ColumnTypes, Fields) ];

show(Name, _, Value) ->

    io:format("~s: ~p~n", [Name, Value]).



%%%----------------------------------------------------------------------------
%%% @private return an empty binary, and the whole input. Or a string and rest.
%%% @spec erl_cond_str(0 | 1, binary()) -> { String::'null' | binary(), Remainder::binary() }

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



%   On the Java Cient and Server:                                             %
%                                                                             %
%   "... there is an  AtomicLong used to generate the client data and a map   %
%   from  Long values  to the provided  callbacks  for each connection to a   %
%   VoltDB node.  Synchronous  invocations  use an a utomatically generated   %
%   SynchronousCallback.  To  the server the client data  is opaque and not   %
%   necessarily unique  so it generates  a transaction id  and uses that to   %
%   map to the client connection and client data for a response."             %
%   --- A. Weisberg, VoltDB, https://community.voltdb.com/node/83 ---         %
%                                                                             %

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                             %
%                                 UTILITY                                     %
%                                                                             %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%----------------------------------------------------------------------------
%%% Verbose Echo
%%%----------------------------------------------------------------------------
%%% @doc Conditionally print String x Format to standard out.

%% @spec vecho(boolean(),string(),list(any())) -> boolean()
vecho(Condition, String, Format) ->

    case Condition of
        true ->
            io:format("~nerlvolt wire: " ++ String ++ "~n", Format), true;
        _ -> false
    end.

%%%----------------------------------------------------------------------------
%%% Error out
%%%----------------------------------------------------------------------------
%%% @doc Try to hand to ?TRACE. Else print to screen

%% @spec erltrace(string()) -> 'ok' | 'void'
erltrace(S) ->
    erltrace(S, []).

%% @spec erltrace(string(),[any()]) -> 'ok' | 'void'
erltrace(Format, Args) ->
    try
        ?TRACE("[wire] " ++ Format, Args)
    catch _:_ ->
        io:format("Erlvolt Wire: " ++ Format, Args)
    end.

%% @spec erlerror(string(),[any()]) -> 'ok'
erlerror(Format, Args) ->
    erlerror(Format, Args, trace).

%% @spec erlerror(string(),[any()]) -> 'ok'
erlerror(Format, Args, Trace) ->
    try
        erlvolt:error("[wire level] " ++ Format, Args, Trace)
    catch _:_ ->
        io:format("Erlvolt Wire: " ++ Format, Args)
    end.

%%%----------------------------------------------------------------------------
%%% Banner
%%%----------------------------------------------------------------------------
%%% @doc Prints a banner with library name and version, and a note.

%% @spec banner() -> 'ok'
banner() -> banner(?EXPLAIN).

%%%----------------------------------------------------------------------------
%%% @doc Prints a banner with library name and version, and a note, plus message.

%% @spec banner(string()) -> 'ok'
banner(Message) ->
    io:format("---------------------------------------------------------------------------~n"),
    io:format("                                             %                             ~n"),
    io:format("             %%%%% %%%%%  °%%  °%%   %%°  °%%%   °%%   %%%%%%              ~n"),
    io:format("             %%%   %%  %%  %%    %%  %  %%   %%%  %%     %%                ~n"),
    io:format("             %%    %%%%%°  %%  %  %%%   %%%   %%  %%  %  %%                ~n"),
    io:format("             %%%%% %%  %%  %%%%%   %      %%% °   %%%%%  %%                ~n"),
    io:format("                                          %                                ~n"),
    io:format("---------------------------------------------------------------------------~n"),
    io:format("~s ~s - ~s~n",[?LIBRARY, ?VERSION, Message]),
    ok.


%%%-----------------------------------°%°-----------------------------------%%%
