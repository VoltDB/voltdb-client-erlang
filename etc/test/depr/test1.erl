%%%-------------------------------------------------------------------------%%%
%%% File        : test1.erl                                                 %%%
%%% Description : Unit Tests #1 for Erlang-VoltDB client API erlvolt.erl    %%% 
%%% Copyright   : VoltDB, LLC - http://www.voltdb.com                       %%%
%%% Production  : Eonblast Corporation - http://www.eonblast.com            %%%
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%
%%% License     : GPLv3                                                     %%%
%%% Created     : 25 Apr 2010                                               %%%
%%% Changed     : 07 Jul 2012                                               %%%
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
%%%                                                                         
%%%   @doc                                                                  
%%%                                                                         
%%%   This is a unit test program to test the Erlvolt module ervolt.erl.    
%%%                                                                         
%%%   === Usage ===                                                                
%%%   ```                                                                
%%%   $ erlc erlvolt.erl; erlc erlunit/erlunit.erl; erlc test1.erl;         
%%%   $ erl -s test1 run -s init stop -noshell                              
%%%   '''                                                                         
%%%   === Prerequisite ===                                                         
%%%                                                            
%%%   Erlunit in subdirectory erlunit.                                      
%%%   Get it from: [http://github.com/Eonblast/Erlunit/tarball/master]        
%%%                                                                         
%%%   === Notes ===                                                                
%%%                                                                   
%%%   Use of unit test functions and macros in this source is mixed for no
%%%   good reason.    
%%%                                                                            
%%%   @end
%%%
%%%----------------------------------------------------------------------------


-module(test1).
-include("../include/erlvolt.hrl").

-import(erlunit). % http://github.com/Eonblast/Erlunit/tarball/master
-include("../etc/erlunit/erlunit.hrl").

-export([run/0]).
-import(unicode).

-define(TRILLION, 1000000000000). % for clarity and to prevent typos.
-define(BILLION, 1000000000). % for clarity and to prevent typos.
-define(MILLION, 1000000   ).

%%%----------------------------------------------------------------------------
%%%
%%%    run/1
%%%
%%%    @doc
%%%
%%%    This is the only function in this package, it runs a unit test on
%%%    erlvolt.erl.
%%%
%%%    @end
%%%
%%%----------------------------------------------------------------------------

run() ->

    erlunit:start([colors ]),
    erlvolt:banner(),
    erlunit:banner("Test #1: Basic Types"),

    
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %                                                                         %
    %                              BASIC TYPES                                % 
    %                                                                         %
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    
    %*************************************************************************%
    %                                                                         %
    %                        Floating Point Numbers                           % 
    %                                                                         %
    %*************************************************************************%
    
    erlunit:suite("Floats"),
    
    ?ERLUNIT_EQUAL(erlvolt:volt_float( 0.0), << 0:32>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_float(-1.0), <<-1:32/float>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_float( 1.0), << 1:32/float>>),

    ?ERLUNIT_EQUAL(erlvolt:volt_float( 1234567890.098765431), << 1234567890.098765431:32/float>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_float(-1234567890.098765431), <<-1234567890.098765431:32/float>>),

    ?ERLUNIT_EQUAL(erlvolt:volt_float(nan),                   ?VOLT_NAN),
    ?ERLUNIT_EQUAL(erlvolt:volt_float(positive_infinity),     ?VOLT_POSITIVE_INFINITY),
    ?ERLUNIT_EQUAL(erlvolt:volt_float(negative_infinity),     ?VOLT_NEGATIVE_INFINITY),
    
    ?ERLUNIT_FAIL (erlvolt:volt_float( 0) ),
    ?ERLUNIT_FAIL (erlvolt:volt_float(-1) ),
    ?ERLUNIT_FAIL (erlvolt:volt_float( 1) ),
%    ?ERLUNIT_FAIL (erlvolt:volt_float(nil)),

    erlunit:equal(erlvolt:volt_float(0.0),               <<0:32>>,                      "Volt float  0.0"),
    erlunit:equal(erlvolt:volt_float(-1.0),              <<-1.0:32/float>>,             "Volt float -1.0"),
    erlunit:equal(erlvolt:volt_float(1.0),               <<1.0:32/float>>,              "Volt float  1.0"),
    erlunit:equal(erlvolt:volt_float( 7654321.1234567),  << 7654321.1234567:32/float>>, "Volt float  7654321.1234567"),
    erlunit:equal(erlvolt:volt_float(-7654321.1234567),  <<-7654321.1234567:32/float>>, "Volt float -7654321.1234567"),
    erlunit:equal(erlvolt:volt_float(nan),               ?VOLT_NAN,                     "Volt float  NaN"),
    erlunit:equal(erlvolt:volt_float(positive_infinity), ?VOLT_POSITIVE_INFINITY,       "Volt float  Pos. Inf."),
    erlunit:equal(erlvolt:volt_float(negative_infinity), ?VOLT_NEGATIVE_INFINITY,       "Volt float  Neg. Inf."),

    erlunit:fail (fun() -> erlvolt:volt_float(0)   end,                                 "Volt float  0"),
    erlunit:fail (fun() -> erlvolt:volt_float(-1)  end,                                 "Volt float -1"),
    erlunit:fail (fun() -> erlvolt:volt_float(1)   end,                                 "Volt float  1"),
    erlunit:fail (fun() -> erlvolt:volt_float(nil) end,                                 "Volt float  nil"),


    %*************************************************************************%
    %                                                                         %
    %                                 Strings                                 % 
    %                                                                         %
    %*************************************************************************%

    Ouml   = unicode:characters_to_list("ö", utf8),
    Delta   = unicode:characters_to_list("∂", utf8),
    OumlSize = size(<<"ö">>),
    DeltaSize = size(<<"∂">>),
    MegaString = list_to_binary(string:chars(33, 1000000)),
    MegaOverString = list_to_binary(string:chars(33, 1000001)),
    % TODO: TEST UNICODE / LATIN-1 CHARS

    erlunit:suite("Strings"), 
    erlunit:equal(erlvolt:volt_string("foo"), <<3:?VOLT_STRING_SIZE_BINARY_TYPE,"foo">>,         "Volt string 'foo'"),
    erlunit:equal(erlvolt:volt_string("1"),   <<1:?VOLT_STRING_SIZE_BINARY_TYPE,"1">>,           "Volt string '1'"),
    erlunit:equal(erlvolt:volt_string(Ouml),  <<OumlSize:?VOLT_STRING_SIZE_BINARY_TYPE,"ö">>,   "Volt string 'ö'"),
    erlunit:equal(erlvolt:volt_string(Delta), <<DeltaSize:?VOLT_STRING_SIZE_BINARY_TYPE,"∂">>,   "Volt string '∂'"),
    erlunit:equal(erlvolt:volt_string(""),    <<0:?VOLT_STRING_SIZE_BINARY_TYPE>>,               "Volt empty string "),
    erlunit:equal(erlvolt:volt_string(null),  <<-1:?VOLT_STRING_SIZE_BINARY_TYPE>>,              "Volt string NULL"),
    ?ERLUNIT_EQUAL_MSG(erlvolt:volt_string(MegaString), <<1000000:?VOLT_STRING_SIZE_BINARY_TYPE, MegaString/binary>>, "Volt 1MB string"),
    ?ERLUNIT_FAIL_MSG(erlvolt:volt_string(MegaOverString),                                       "Volt 1MB+1 string"),


    %*************************************************************************%
    %                                                                         %
    %                               Integers                                  % 
    %                                                                         %
    %*************************************************************************%
    
    erlunit:suite("Integers"),
    erlunit:equal(erlvolt:volt_byte(0),     <<0:8>>,                             "Volt byte  0"),
    erlunit:equal(erlvolt:volt_byte(1),     <<1:8>>,                             "Volt byte  1"),
    erlunit:equal(erlvolt:volt_byte(-1),    <<-1:8>>,                            "Volt byte -1"),
    erlunit:equal(fun() -> erlvolt:volt_byte(127)  end, <<127:8/signed>>,        "Volt byte 127"),
    erlunit:fail (fun() -> erlvolt:volt_byte(128)  end,                          "Volt byte  128"),
    erlunit:equal(fun() -> erlvolt:volt_byte(-128) end, <<-128:8/signed>>,       "Volt byte -128"),
    erlunit:fail (fun() -> erlvolt:volt_byte(-129) end,                          "Volt byte -129"),

    erlunit:equal(erlvolt:volt_short(0),    <<0:16>>,                            "Volt short  0"),
    erlunit:equal(erlvolt:volt_short(1),    <<1:16>>,                            "Volt short  1"),
    erlunit:equal(erlvolt:volt_short(-1),   <<-1:16>>,                           "Volt short -1"),
    erlunit:equal(fun() -> erlvolt:volt_short( 32767) end, <<32767:16/signed>>,  "Volt short  32767"),
    erlunit:fail (fun() -> erlvolt:volt_short( 32768) end,                       "Volt short  32768"),
    erlunit:equal(fun() -> erlvolt:volt_short(-32768) end, <<-32768:16/signed>>, "Volt short -32768"),
    erlunit:fail (fun() -> erlvolt:volt_short(-32769) end,                       "Volt short -32769"),

    erlunit:equal(erlvolt:volt_integer(0),  <<0:32>>,                                        "Volt integer  0"),
    erlunit:equal(erlvolt:volt_integer(1),  <<1:32>>,                                        "Volt integer  1"),
    erlunit:equal(erlvolt:volt_integer(-1), <<-1:32>>,                                       "Volt integer -1"),
    erlunit:equal(fun() -> erlvolt:volt_integer( 2147483647) end, <<2147483647:32/signed>>,  "Volt integer  2147483647"),
    erlunit:fail (fun() -> erlvolt:volt_integer( 2147483648) end,                            "Volt integer  2147483648"),
    erlunit:equal(fun() -> erlvolt:volt_integer(-2147483648) end, <<-2147483648:32/signed>>, "Volt integer -2147483648"),
    erlunit:fail (fun() -> erlvolt:volt_integer(-2147483649) end,                            "Volt integer -2147483649"),

    erlunit:equal(erlvolt:volt_long(0),    <<0:64>>,                                         "Volt long  0"),
    erlunit:equal(erlvolt:volt_long(1),    <<1:64>>,                                         "Volt long  1"),
    erlunit:equal(erlvolt:volt_long(-1),   <<-1:64>>,                                        "Volt long -1"),
    erlunit:equal(fun() -> erlvolt:volt_long( 9223372036854775807) end, <<9223372036854775807:64/signed>>,
                                                                                             "Volt long  9223372036854775807"),
    erlunit:fail (fun() -> erlvolt:volt_long( 9223372036854775808) end,                      "Volt long  9223372036854775808"),
    erlunit:equal(fun() -> erlvolt:volt_long(-9223372036854775808) end, <<-9223372036854775808:64/signed>>,
                                                                                             "Volt long -9223372036854775808"),
    erlunit:fail (fun() -> erlvolt:volt_long(-9223372036854775809) end,                      "Volt long -9223372036854775809"),

    %%%-decode---------------------------------------------------------------------
    
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<<>>),     0),

    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<<0:8>>),  0),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<<0:16>>), 0),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<<0:32>>), 0),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<<0:64>>), 0),

    erlunit:equal(erlvolt:erl_integer(<<1: 8>>),    1,                                   "Erlang integer to <<1:8>>"),
    erlunit:equal(erlvolt:erl_integer(<<1:16>>),    1,                                   "Erlang integer to <<1:16>>"),
    erlunit:equal(erlvolt:erl_integer(<<1:32>>),    1,                                   "Erlang integer to <<1:32>>"),
    erlunit:equal(erlvolt:erl_integer(<<1:64>>),    1,                                   "Erlang integer to <<1:64>>"),

    erlunit:equal(erlvolt:erl_integer(<<-1: 8>>),  -1,                                   "Erlang integer to <<-1:8>>"),
    erlunit:equal(erlvolt:erl_integer(<<-1:16>>),  -1,                                   "Erlang integer to <<-1:16>>"),
    erlunit:equal(erlvolt:erl_integer(<<-1:32>>),  -1,                                   "Erlang integer to <<-1:32>>"),
    erlunit:equal(erlvolt:erl_integer(<<-1:64>>),  -1,                                   "Erlang integer to <<-1:64>>"),

    erlunit:equal(erlvolt:erl_integer(<<?VOLT_BYTE_MAX:8>>),       ?VOLT_BYTE_MAX,       "Erlang integer to BYTE MAX"),
    erlunit:equal(erlvolt:erl_integer(<<?VOLT_SHORT_MAX:16>>),     ?VOLT_SHORT_MAX,      "Erlang integer to SHORT MAX"),
    erlunit:equal(erlvolt:erl_integer(<<?VOLT_INTEGER_MAX:32>>),   ?VOLT_INTEGER_MAX,    "Erlang integer to INTEGER MAX"),
    erlunit:equal(erlvolt:erl_integer(<<?VOLT_LONG_MAX:64>>),      ?VOLT_LONG_MAX,       "Erlang integer to LONG MAX"),
 
    erlunit:equal(erlvolt:erl_integer(<<?VOLT_BYTE_MIN:8>>),       ?VOLT_BYTE_MIN,       "Erlang integer to BYTE MIN"),
    erlunit:equal(erlvolt:erl_integer(<<?VOLT_SHORT_MIN:16>>),     ?VOLT_SHORT_MIN,      "Erlang integer to SHORT MIN"),
    erlunit:equal(erlvolt:erl_integer(<<?VOLT_INTEGER_MIN:32>>),   ?VOLT_INTEGER_MIN,    "Erlang integer to INTEGER MIN"),
    erlunit:equal(erlvolt:erl_integer(<<?VOLT_LONG_MIN:64>>),      ?VOLT_LONG_MIN,       "Erlang integer to LONG MIN"),

    % Note: the floowing over/underflow happens right in the terms like <<(?VOLT_BYTE_MAX +1):8>> - not in the functions.
    
    ?ERLUNIT_EQUAL_MSG(erlvolt:erl_integer(<<(?VOLT_BYTE_MAX +1):8>>),     ?VOLT_BYTE_MIN,    "Erlang integer overflow of BYTE MAX +1"),
    ?ERLUNIT_EQUAL_MSG(erlvolt:erl_integer(<<(?VOLT_SHORT_MAX +1):16>>),   ?VOLT_SHORT_MIN,   "Erlang integer overflow of SHORT MAX +1"),
    ?ERLUNIT_EQUAL_MSG(erlvolt:erl_integer(<<(?VOLT_INTEGER_MAX +1):32>>), ?VOLT_INTEGER_MIN, "Erlang integer overflow of INTEGER MAX +1"),
    ?ERLUNIT_EQUAL_MSG(erlvolt:erl_integer(<<(?VOLT_LONG_MAX +1):64>>),    ?VOLT_LONG_MIN,    "Erlang integer overflow of LONG MAX +1"),

    ?ERLUNIT_EQUAL_MSG(erlvolt:erl_integer(<<(?VOLT_BYTE_MIN -1):8>>),     ?VOLT_BYTE_MAX,    "Erlang integer underflow of BYTE MIN -1"),
    ?ERLUNIT_EQUAL_MSG(erlvolt:erl_integer(<<(?VOLT_SHORT_MIN -1):16>>),   ?VOLT_SHORT_MAX,   "Erlang integer underflow of SHORT MIN -1"),
    ?ERLUNIT_EQUAL_MSG(erlvolt:erl_integer(<<(?VOLT_INTEGER_MIN -1):32>>), ?VOLT_INTEGER_MAX, "Erlang integer underflow of INTEGER MIN -1"),
    ?ERLUNIT_EQUAL_MSG(erlvolt:erl_integer(<<(?VOLT_LONG_MIN -1):64>>),    ?VOLT_LONG_MAX,    "Erlang integer underflow of LONG MIN -1"),


    ?ERLUNIT_FAIL (erlvolt:erl_integer(<<-170141183460469231731687303715884105728:128/signed>>)),
    ?ERLUNIT_FAIL (erlvolt:erl_integer(<<255,255,255,255,255,255,255,255,255,173,33,210,178,57,217,128>>)),
    ?ERLUNIT_FAIL (erlvolt:erl_integer(<<0,0,0,0,0,0,0,0,0,82,222,45,77,198,38,128>>)),

    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 1000000000000:128>>),  1  ),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 1000000000000:128>>),  1.0),
    ?ERLUNIT_FAIL (erlvolt:erl_integer(<< 999999999999:128>>)),
    ?ERLUNIT_FAIL (erlvolt:erl_integer(<< 1100000000000:128>>)),
    ?ERLUNIT_FAIL (erlvolt:erl_integer(<< 1999999999999:128>>)),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 10000000000000:128>>),  10  ),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 10000000000000:128>>),  10.0),
    ?ERLUNIT_FAIL (erlvolt:erl_integer(<< 10100000000000:128>>)),
    ?ERLUNIT_FAIL (erlvolt:erl_integer(<< 10999999999999:128>>)),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 10000000000000000000:128>>),  10000000  ),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 10000000000000000000:128>>),  10000000.0),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 10000000000000000000:128>>),  1.0e7),
    ?ERLUNIT_FAIL (erlvolt:erl_integer(<< 10000000100000000000:128>>)),

    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 999999999999998000000000000:128>>),  999999999999998),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 9999999999999980000000000000:128>>),  9999999999999980),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 99999999999999980000000000000:128>>),  99999999999999980),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 999999999999999980000000000000:128>>),  999999999999999980),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 9999999999999999910000000000000:128>>),  9999999999999999910),

    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 9999999999999999980000000000000:128>>),  9999999999999999980),

    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 999999999999999000000000000:128>>),  999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 9999999999999999000000000000:128>>),  9999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 99999999999999999000000000000:128>>),  99999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 999999999999999999000000000000:128>>),  999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 9999999999999999999000000000000:128>>),  9999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 99999999999999999999000000000000:128>>),  99999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 999999999999999999999000000000000:128>>),  999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 9999999999999999999999000000000000:128>>),  9999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 99999999999999999999999000000000000:128>>),  99999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 999999999999999999999999000000000000:128>>),  999999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 9999999999999999999999999000000000000:128>>),  9999999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 99999999999999999999999999000000000000:128>>),  99999999999999999999999999),
%    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 999999999999999999999999999000000000000:128>>),  999999999999999999999999999),
%    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 9999999999999999999999999999000000000000:128>>),  9999999999999999999999999999),
%    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 99999999999999999999999999999000000000000:128>>),  99999999999999999999999999999),
%    ?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 999999999999999999999999999999000000000000:128>>),  999999999999999999999999999999),


    % ?ERLUNIT_EQUAL(erlvolt:erl_integer(<<-99999999999999999999999999999999999999000000000000:128>>), -99999999999999999999999999999999999999),
    ?ERLUNIT_FAIL (erlvolt:erl_integer( 100000000000000000000000000000000000000)),
    ?ERLUNIT_FAIL (erlvolt:erl_integer(-100000000000000000000000000000000000000)),

    ?ERLUNIT_FAIL (erlvolt:erl_integer(<<0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,16>>)), 
    ?ERLUNIT_FAIL (erlvolt:erl_integer(<<0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,20>>)),


    % integer_or_null (same as integer but may return null).

    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<<-170141183460469231731687303715884105728:128/signed>>), null),
    ?ERLUNIT_FAIL (erlvolt:erl_integer_or_null_from_decimal(<<255,255,255,255,255,255,255,255,255,173,33,210,178,57,217,128>>)),
    ?ERLUNIT_FAIL (erlvolt:erl_integer_or_null_from_decimal(<<0,0,0,0,0,0,0,0,0,82,222,45,77,198,38,128>>)),

    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 1000000000000:128>>),  1  ),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 1000000000000:128>>),  1.0),
    ?ERLUNIT_FAIL (erlvolt:erl_integer_or_null_from_decimal(<< 999999999999:128>>)),
    ?ERLUNIT_FAIL (erlvolt:erl_integer_or_null_from_decimal(<< 1100000000000:128>>)),
    ?ERLUNIT_FAIL (erlvolt:erl_integer_or_null_from_decimal(<< 1999999999999:128>>)),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 10000000000000:128>>),  10  ),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 10000000000000:128>>),  10.0),
    ?ERLUNIT_FAIL (erlvolt:erl_integer_or_null_from_decimal(<< 10100000000000:128>>)),
    ?ERLUNIT_FAIL (erlvolt:erl_integer_or_null_from_decimal(<< 10999999999999:128>>)),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 10000000000000000000:128>>),  10000000  ),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 10000000000000000000:128>>),  10000000.0),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 10000000000000000000:128>>),  1.0e7),
    ?ERLUNIT_FAIL (erlvolt:erl_integer_or_null_from_decimal(<< 10000000100000000000:128>>)),

    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 999999999999998000000000000:128>>),  999999999999998),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 9999999999999980000000000000:128>>),  9999999999999980),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 99999999999999980000000000000:128>>),  99999999999999980),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 999999999999999980000000000000:128>>),  999999999999999980),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 9999999999999999910000000000000:128>>),  9999999999999999910),

    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 9999999999999999980000000000000:128>>),  9999999999999999980),

    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 999999999999999000000000000:128>>),  999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 9999999999999999000000000000:128>>),  9999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 99999999999999999000000000000:128>>),  99999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 999999999999999999000000000000:128>>),  999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 9999999999999999999000000000000:128>>),  9999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 99999999999999999999000000000000:128>>),  99999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 999999999999999999999000000000000:128>>),  999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 9999999999999999999999000000000000:128>>),  9999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 99999999999999999999999000000000000:128>>),  99999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 999999999999999999999999000000000000:128>>),  999999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 9999999999999999999999999000000000000:128>>),  9999999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 99999999999999999999999999000000000000:128>>),  99999999999999999999999999),
%    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 999999999999999999999999999000000000000:128>>),  999999999999999999999999999),
%    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 9999999999999999999999999999000000000000:128>>),  9999999999999999999999999999),
%    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 99999999999999999999999999999000000000000:128>>),  99999999999999999999999999999),
%    ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<< 999999999999999999999999999999000000000000:128>>),  999999999999999999999999999999),


    % ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null_from_decimal(<<-99999999999999999999999999999999999999000000000000:128>>), -99999999999999999999999999999999999999),
    ?ERLUNIT_FAIL (erlvolt:erl_integer_or_null_from_decimal(   100000000000000000000000000000000000000)),
    ?ERLUNIT_FAIL (erlvolt:erl_integer_or_null_from_decimal(  -100000000000000000000000000000000000000)),

    ?ERLUNIT_FAIL (erlvolt:erl_integer_or_null_from_decimal(<< 0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,16>>)), 
    ?ERLUNIT_FAIL (erlvolt:erl_integer_or_null_from_decimal(<< 0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,20>>)),



    %*************************************************************************%
    %                                                                         %
    %                                Decimals                                 %
    %                                                                         %
    %*************************************************************************%
    
    erlunit:suite("Decimals"),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal(null), <<-170141183460469231731687303715884105728:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal(-23325.23425), <<255,255,255,255,255,255,255,255,255,173,33,210,178,57,217,128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 23325.23425), <<0,0,0,0,0,0,0,0,0,82,222,45,77,198,38,128>>),
    erlunit:equal(erlvolt:volt_decimal(-23325.23425),  <<-1,-1,-1,-1,-1,-1,-1,-1,-1,-83,33,-46,-78,57,-39,-128>>, "Volt decimal -23325.23425"),

    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 1  ),                                     <<1000000000000:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 1.0),                                     <<1000000000000:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 0.999999999999),                          << 999999999999:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 1.1),                                     <<1100000000000:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 1.999999999999),                          <<1999999999999:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 10  ),                                    <<10000000000000:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 10.0),                                    <<10000000000000:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 10.1),                                    <<10100000000000:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 10.999999999999),                         <<10999999999999:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 10000000  ),                              <<10000000000000000000:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 10000000.0),                              <<10000000000000000000:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 10000000.1),                              <<10000000100000000000:128>>),

    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 99999999999999999999999999999999999998),  << 99999999999999999999999999999999999998000000000000:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal(-99999999999999999999999999999999999998),  <<-99999999999999999999999999999999999998000000000000:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 99999999999999999999999999999999999999),  << 99999999999999999999999999999999999999000000000000:128>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal(-99999999999999999999999999999999999999),  <<-99999999999999999999999999999999999999000000000000:128>>),
    ?ERLUNIT_FAIL (erlvolt:volt_decimal( 100000000000000000000000000000000000000)),
    ?ERLUNIT_FAIL (erlvolt:volt_decimal(-100000000000000000000000000000000000000)),

    ?ERLUNIT_EQUAL(erlvolt:volt_decimal(23325.12345678901),                        <<0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,16>>), 
    ?ERLUNIT_EQUAL(erlvolt:volt_decimal(23325.123456789012),                       <<0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,20>>),

    %%%------------------------------------------------------------------decimals-
    
    % Tests that cannot be executed:
    
    % Erlang works with the same precision as VoltDB Decimals and will therefore not be able to
    % feed floats into volt_decimal that have precision truncated. The below lines pass, because
    % the literal as you read them below are rounded already by the Erlang compiler. Otherwise,
    % of course, an error should be thrown.
    %
    % 1> 23325.123456789012345.                      
    % 23325.123456789013
    %
    % ?ERLUNIT_FAIL (erlvolt:volt_decimal(23325.1234567890123)),
    % ?ERLUNIT_FAIL (erlvolt:volt_decimal(23325.12345678901234)),

    % Erlang's floating point precision prevents the following from working, for the same reasons
    % because:
    % 1> 99999999999999999999999999999999999998.999999999999.
    % 1.0e38
    % Again, the figure as seen below is already reduced, tacitely by the compiler.
    %
    % ?ERLUNIT_EQUAL(erlvolt:volt_decimal( 99999999999999999999999999999999999998.999999999999),
    %                                                                               << 99999999999999999999999999999999999998999999999999:128>>),
    % ?ERLUNIT_EQUAL(erlvolt:volt_decimal(-99999999999999999999999999999999999998.999999999999),  
    %                                                                               <<-99999999999999999999999999999999999998999999999999:128>>),

    % For the same reasons, the below test cannot work:
    % 1> 10000000.999999999999.
    % 10000001.0
    %
    % ?ERLUNIT_EQUAL(erlvolt:volt_decimal(10000000.999999999999),                  <<10000000999999999999:128>>),
    % ?ERLUNIT_EQUAL(erlvolt:volt_decimal(10000000.100000000001),                  <<10000000100000000001:128>>),


    %*************************************************************************%
    %                                                                         %
    %                     Number: Integer / Float / Decimal                   % 
    %                                                                         %
    %*************************************************************************%

    %%%-decode------------------------------------------------------------decimals-
                                       
    ?ERLUNIT_FAIL_MSG(erlvolt:erl_number(<<-170141183460469231731687303715884105728:128/signed>>), "Don't accept binary null value"),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<255,255,255,255,255,255,255,255,255,173,33,210,178,57,217,128>>), -23325.23425),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<0,0,0,0,0,0,0,0,0,82,222,45,77,198,38,128>>),  23325.23425),

    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<1000000000000:128>>),  1  ),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<1000000000000:128>>),  1.0),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 999999999999:128>>),  0.999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<1100000000000:128>>),  1.1),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<1999999999999:128>>),  1.999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<10000000000000:128>>),  10  ),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<10000000000000:128>>),  10.0),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<10100000000000:128>>),  10.1),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<10999999999999:128>>),  10.999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<10000000000000000000:128>>),  10000000  ),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<10000000000000000000:128>>),  10000000.0),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<10000000000000000000:128>>),  1.0e7),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<10000000100000000000:128>>),  10000000.1),

    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 999999999999998000000000000:128>>),  999999999999998),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 9999999999999980000000000000:128>>),  9999999999999980),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 99999999999999980000000000000:128>>),  99999999999999980),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 999999999999999980000000000000:128>>),  999999999999999980),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 9999999999999999910000000000000:128>>),  9999999999999999910),

    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 9999999999999999980000000000000:128>>),  9999999999999999980),

    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 999999999999999000000000000:128>>),  999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 9999999999999999000000000000:128>>),  9999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 99999999999999999000000000000:128>>),  99999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 999999999999999999000000000000:128>>),  999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 9999999999999999999000000000000:128>>),  9999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 99999999999999999999000000000000:128>>),  99999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 999999999999999999999000000000000:128>>),  999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 9999999999999999999999000000000000:128>>),  9999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 99999999999999999999999000000000000:128>>),  99999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 999999999999999999999999000000000000:128>>),  999999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 9999999999999999999999999000000000000:128>>),  9999999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 99999999999999999999999999000000000000:128>>),  99999999999999999999999999),
    
    %    These are too big. There is an 'effective' threshold of integer usage here. TODO: find MAX and MIN.
    %    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 999999999999999999999999999000000000000:128>>),  999999999999999999999999999),
    %    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 9999999999999999999999999999000000000000:128>>),  9999999999999999999999999999),
    %    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 99999999999999999999999999999000000000000:128>>),  99999999999999999999999999999),
    %    ?ERLUNIT_EQUAL(erlvolt:erl_number(<< 999999999999999999999999999999000000000000:128>>),  999999999999999999999999999999),


    % ?ERLUNIT_EQUAL(erlvolt:erl_number(<<-99999999999999999999999999999999999999000000000000:128>>), -99999999999999999999999999999999999999),
    ?ERLUNIT_FAIL (erlvolt:erl_number( 100000000000000000000000000000000000000)),
    ?ERLUNIT_FAIL (erlvolt:erl_number(-100000000000000000000000000000000000000)),

    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,16>>), 23325.12345678901), 
    ?ERLUNIT_EQUAL(erlvolt:erl_number(<<0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,20>>), 23325.123456789012),
    

    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<-170141183460469231731687303715884105728:128/signed>>), null),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<255,255,255,255,255,255,255,255,255,173,33,210,178,57,217,128>>), -23325.23425),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<0,0,0,0,0,0,0,0,0,82,222,45,77,198,38,128>>),  23325.23425),

    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<1000000000000:128>>),  1  ),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<1000000000000:128>>),  1.0),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 999999999999:128>>),  0.999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<1100000000000:128>>),  1.1),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<1999999999999:128>>),  1.999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<10000000000000:128>>),  10  ),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<10000000000000:128>>),  10.0),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<10100000000000:128>>),  10.1),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<10999999999999:128>>),  10.999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<10000000000000000000:128>>),  10000000  ),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<10000000000000000000:128>>),  10000000.0),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<10000000000000000000:128>>),  1.0e7),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<10000000100000000000:128>>),  10000000.1),

    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 999999999999998000000000000:128>>),  999999999999998),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 9999999999999980000000000000:128>>),  9999999999999980),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 99999999999999980000000000000:128>>),  99999999999999980),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 999999999999999980000000000000:128>>),  999999999999999980),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 9999999999999999910000000000000:128>>),  9999999999999999910),

    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 9999999999999999980000000000000:128>>),  9999999999999999980),

    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 999999999999999000000000000:128>>),  999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 9999999999999999000000000000:128>>),  9999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 99999999999999999000000000000:128>>),  99999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 999999999999999999000000000000:128>>),  999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 9999999999999999999000000000000:128>>),  9999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 99999999999999999999000000000000:128>>),  99999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 999999999999999999999000000000000:128>>),  999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 9999999999999999999999000000000000:128>>),  9999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 99999999999999999999999000000000000:128>>),  99999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 999999999999999999999999000000000000:128>>),  999999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 9999999999999999999999999000000000000:128>>),  9999999999999999999999999),
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 99999999999999999999999999000000000000:128>>),  99999999999999999999999999),
    
    %    These are too big. There is an 'effective' threshold of integer usage here. TODO: find MAX and MIN.
    %    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 999999999999999999999999999000000000000:128>>),  999999999999999999999999999),
    %    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 9999999999999999999999999999000000000000:128>>),  9999999999999999999999999999),
    %    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 99999999999999999999999999999000000000000:128>>),  99999999999999999999999999999),
    %    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 999999999999999999999999999999000000000000:128>>),  999999999999999999999999999999),


    % ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<-99999999999999999999999999999999999999000000000000:128>>), 
    %                                              -99999999999999999999999999999999999999),
    ?ERLUNIT_FAIL (erlvolt:erl_number_or_null( 100000000000000000000000000000000000000)),
    ?ERLUNIT_FAIL (erlvolt:erl_number_or_null(-100000000000000000000000000000000000000)),

    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,16>>), 23325.12345678901), 
    ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,20>>), 23325.123456789012),


    %*************************************************************************%
    %                                                                         %
    %                                  Time                                   % 
    %                                                                         %
    %*************************************************************************%
    
    erlunit:suite("Time"),
    {Mega,Sec,Micro} = Now = erlang:now(),
    _DateTime = calendar:now_to_datetime(Now),


    % from Erlang 'now' time format
    ?ERLUNIT_EQUAL(erlvolt:volt_time({0,0,0}),      <<0:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({0,0,1}),      <<1:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({0,0,999}),    <<999:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({0,0,1000}),   <<1000:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({0,0,1001}),   <<1001:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({0,10,0}),     <<10000000:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({1,0,999}),    <<1000000000999:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({1,0,1000}),   <<1000000001000:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({1,0,1001}),   <<1000000001001:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({1,10,0}),     <<(?TRILLION+10*?MILLION):64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({1,10,1}),     <<(?TRILLION+10*?MILLION+1):64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({1,10,999}),   <<(?TRILLION+10*?MILLION+999):64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({1,10,1000}),  <<(?TRILLION+10*?MILLION+1000):64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({1183,792027,0}),  <<0,4,52,167,15,65,220,192>>), % TODO: verify
    ?ERLUNIT_EQUAL(erlvolt:volt_time(Now),          <<(Mega*?TRILLION + Sec * ?MILLION + Micro):64>>),

%   ?ERLUNIT_EQUAL_MSG(erlvolt:volt_time({0,0,-1}),      <<0,0,0,0,0,0,0,0>>, "Catch negative 'now format' input"),
%   ?ERLUNIT_EQUAL_MSG(erlvolt:volt_time({0,-1,1000}),   <<255,255,255,255,255,255,252,25>>, "Catch negative 'now format' input"),
%   ?ERLUNIT_EQUAL_MSG(erlvolt:volt_time({-1,0,-999}),   <<255,255,255,255,196,101,54,0>>, "Catch negative 'now format' input"),
%   ?ERLUNIT_EQUAL_MSG(erlvolt:volt_time({-1,1,-1001}),  <<255,255,255,255,196,101,57,231>>, "Catch negative 'now format' input"),


    % from Erlang datetime
    ?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,1,1},{0,0,0}}),      <<0:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({{1969,12,31},{23,59,59}}), <<-1000000:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,1,1},{0,0,1}}),      <<1000000:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,12,31},{23,59,59}}), <<(365*24*60*60*1000000-1000000):64>>),
%   ?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,12,31},{23,59,59}}), <<0,0,0,7,87,177,40,24>>),               % TODO: verify
%   ?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,1,10},{1,1,1}}),     <<0,0,0,0,46,145,24,200>>),              % TODO: verify
%   ?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,10,1},{1,1,1}}),     <<0,0,0,5,126,31,248,200>> ),            % TODO: verify
%   ?ERLUNIT_EQUAL(erlvolt:volt_time({{2010,3,3},{12,0,0}}),     <<0,0,1,39,35,229,146,0>>),              % TODO: verify
%   ?ERLUNIT_EQUAL(erlvolt:volt_time({{1950,2,2},{23,59,59}}),   <<255,255,255,109,182,90,156,24>>),      % TODO: verify
%   ?ERLUNIT_EQUAL(erlvolt:volt_time({{2000,10,10},{10,10,10}}), <<0,0,0,226,30,101,255,208>>),           % TODO: verify
    ?ERLUNIT_EQUAL(erlvolt:volt_time({{2007,7,7}, {7,07,07}}),   <<0,4,52,167,15,65,220,192>>),           % TODO: verify
%   ?ERLUNIT_EQUAL(erlvolt:volt_time({{0,1,1},{0,0,0}}),         <<255,255,199,117,144,251,160,0>>),      % TODO: verify

    % overrunning date and time values
    ?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,12,31},{23,59,60}}), <<(365*24*60*60*1000000):64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,12,31},{23,59,61}}), <<(365*24*60*60*1000000+1000000):64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time({{1971,1,1},{0,0,1}}),      <<(365*24*60*60*1000000+1000000):64>>),
%   ?ERLUNIT_EQUAL(erlvolt:volt_time({{1971,1,1},{0,0,-1}}),     <<0,0,0,7,87,177,40,24>>),               % TODO: verify
%   ?ERLUNIT_EQUAL(erlvolt:volt_time({{2020,1,1},{33,10,11}}),   <<0,0,1,111,101,132,249,56>> ),          % TODO: verify

    % invalid date and time values
    ?ERLUNIT_FAIL (erlvolt:volt_time({{0,0,0},{0,0,0}})),
    ?ERLUNIT_FAIL (erlvolt:volt_time({{-1,1,1},{0,0,0}})),
    ?ERLUNIT_FAIL (erlvolt:volt_time({{ 1000000,0,0},{0,0,0}})),
    ?ERLUNIT_FAIL (erlvolt:volt_time({{-1000000,0,0},{0,0,0}})),


    % from unix epoch seconds integer
    ?ERLUNIT_EQUAL(erlvolt:volt_time(   0),   <<      0:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time(   1),   <<   1000000:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time(  -1),   <<  -1000000:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time(  10),   <<  10000000:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time( -10),   << -10000000:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time( 999),   << 999000000:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time(-999),   <<-999000000:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time(1000),   <<1000000000:64>>),
    ?ERLUNIT_EQUAL(erlvolt:volt_time(1001),   <<1001000000:64>>),

    ?ERLUNIT_FAIL_MSG(erlvolt:volt_time(1.0),   "Catch float input"),
    ?ERLUNIT_FAIL_MSG(erlvolt:volt_time(-1.0),  "Catch float input"),


    %%%-decode----------------------------------------------------------------time-


    % to Erlang 'now' time format {Megasec,Sec,Microsec} from (microsec) Integer -
    % that function represents more of an intermediary step really but might come 
    % in handy someplace
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(0),                           {0,0,0}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(1),                           {0,0,1}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(-1),                          {0,0,-1}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(10000),                       {0,0,10000}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(1000000001),                  {0,1000,1}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(1000000000001),               {1,0,1}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(?TRILLION+10*?MILLION),       {1,10,0}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(?TRILLION+10*?MILLION+1),     {1,10,1}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(Mega*?TRILLION + Sec*1000000 + Micro), {Mega,Sec,Micro}),


    % to Erlang 'now' time format {Megasec,Sec,Microsec} from binary
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<0:64>>),                    {0,0,0}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<1:64>>),                    {0,0,1}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<-1:64>>),                   {0,0,-1}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<10000000:64>>),             {0,10,0}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<1000000000001:64>>),        {1,0,1}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<(?TRILLION+10*1000000):64>>),   {1,10,0}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<(?TRILLION+10*1000000+1):64>>), {1,10,1}),
    ?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<(Mega*?TRILLION + Sec*1000000 + Micro):64>>), {Mega,Sec,Micro}),

%-
    % from Erlang datetime
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<-1000000:64>>),                   {{1969,12,31},{23,59,59}}),
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<   -999:64>>),                    {{1970,1,1},{0,0,0}}),
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<-999999:64>>),                    {{1970,1,1},{0,0,0}}),
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<  -1:64>>),                       {{1970,1,1},{0,0,0}}),
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<  0:64>>),                        {{1970,1,1},{0,0,0}}),
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<  1:64>>),                        {{1970,1,1},{0,0,0}}),
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<999:64>>),                        {{1970,1,1},{0,0,0}}),
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<999999:64>>),                     {{1970,1,1},{0,0,0}}),
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<-1000000:64>>),                   {{1969,12,31},{23,59,59}}),
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<-1100000:64>>),                   {{1969,12,31},{23,59,59}}),
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<-1999999:64>>),                   {{1969,12,31},{23,59,59}}),
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<1000000:64>>),                    {{1970,1,1},{0,0,1}}),
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<1000001:64>>),                    {{1970,1,1},{0,0,1}}),
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<1999999:64>>),                    {{1970,1,1},{0,0,1}}),
    
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<(365*24*60*60*1000000+1000000):64>>),   {{1971,1,1},{0,0,1}}),
    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<(365*24*60*60*1000000-1000000):64>>),   {{1970,12,31},{23,59,59}}),
%    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<0,0,0,7,87,177,40,24>>),          {{1970,12,31},{23,59,59}}),    % TODO: verify
%    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<0,0,0,0,46,145,24,200>>),         {{1970,1,10},{1,1,1}}),        % TODO: verify
%    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<0,0,0,5,126,31,248,200>>),        {{1970,10,1},{1,1,1}} ),       % TODO: verify
%    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<0,0,1,39,35,229,146,0>>),         {{2010,3,3},{12,0,0}}),        % TODO: verify
%    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<255,255,255,109,182,90,156,24>>), {{1950,2,2},{23,59,59}}),      % TODO: verify
%    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<0,0,0,226,30,101,255,208>>),      {{2000,10,10},{10,10,10}}),    % TODO: verify
%    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<0,0,1,111,101,132,249,56>>),      {{2020,1,2},{9,10,11}}),       % TODO: verify
%    ?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<255,255,199,117,144,251,160,0>>), {{0,1,1},{0,0,0}}),            % TODO: verify


    % to unix epoch time format (seconds since 1970) from (miCROsec) Integer -
    % this function represents more of an intermediary step but might come 
    % in handy somewhere
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(0),   0),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(1),   0),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(-1),  0),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(10000),     0),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(10000000), 10),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(1000000000000),   1000000),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(1000000000001),   1000000),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(?TRILLION+10*?MILLION),   ?MILLION + 10),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(?TRILLION+10*?MILLION+1), ?MILLION + 10),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(Mega*?TRILLION + Sec * ?MILLION + Micro), Mega*?MILLION + Sec),


    % to unix epoch time format (seconds since 1970) from VoltDB binary time wire code
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<0:64>>),   0),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<1:64>>),   0),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<-1:64>>),  0),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<10000:64>>),   0),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<10000000:64>>),   10),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<1000000000001:64>>),   1000000),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<(?TRILLION+10*?MILLION):64>>),   ?MILLION + 10),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<(?TRILLION+10*?MILLION+1):64>>), ?MILLION + 10),
    ?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<(Mega*?TRILLION + Sec * ?MILLION + Micro):64>>), Mega*?MILLION + Sec),

    %%%-two-way---------------------------------------------------------------time-

    ?ERLUNIT_PASS(begin A = {{0,1,1},{0,0,0}},     B = erlvolt:volt_time(A), A = erlvolt:erl_datetime(B) end),
    ?ERLUNIT_PASS(begin A = {{1970,1,1},{13,0,0}}, B = erlvolt:volt_time(A), A = erlvolt:erl_datetime(B) end),
    ?ERLUNIT_PASS(begin A = {{2180,1,1},{20,3,2}}, B = erlvolt:volt_time(A), A = erlvolt:erl_datetime(B) end),
    ?ERLUNIT_PASS(begin A = {{50,1,1},{19,12,55}}, B = erlvolt:volt_time(A), A = erlvolt:erl_datetime(B) end),
    ?ERLUNIT_PASS(begin A = {{5000,1,1},{0,0,1}},  B = erlvolt:volt_time(A), A = erlvolt:erl_datetime(B) end),
    ?ERLUNIT_PASS(begin A = {{1,2,3},{4,5,6}},     B = erlvolt:volt_time(A), A = erlvolt:erl_datetime(B) end),



    %*************************************************************************%
    %                                                                         %
    %                                Arrays                                   % 
    %                                                                         %
    %*************************************************************************%
    
    erlunit:suite("Arrays"),
 
    %%%-encode----------------------------------------------------------arrays-

%    Array_Erl_1 = [],
%    Array_Bin_1 = <<?VOLT_STRING,0,2,0,0,0,4,102,111,111,49,0,0,0,4,102,111,111,50>>,
%    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_1), Array_Bin_1),
%    TODO: empty array

    %%% strings

    Array_Erl_1 = [ <<"foo1">>, <<"foo2">> ],
    Array_Bin_1 = <<?VOLT_STRING,0,2,0,0,0,4,102,111,111,49,0,0,0,4,102,111,111,50>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_1), Array_Bin_1),
    %%%   --- pg. 5, VoltDB Client Wire Protocol Version 0, 05/05/10 ---        

    Array_Erl_1a = [ <<"">>, <<"foo2">> ],
    Array_Bin_1a = <<?VOLT_STRING,0,2,0,0,0,0,0,0,0,4,102,111,111,50>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_1a), Array_Bin_1a),
    
    Array_Erl_1b = { voltarray, [ <<"foo1">>, <<"foo2">> ]},
    Array_Bin_1b = <<?VOLT_STRING,0,2,0,0,0,4,102,111,111,49,0,0,0,4,102,111,111,50>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_1b), Array_Bin_1b),

    Array_Erl_1c = { voltarray, ?VOLT_STRING, [ <<"foo1">>, <<"foo2">> ]},
    Array_Bin_1c = <<?VOLT_STRING,0,2,0,0,0,4,102,111,111,49,0,0,0,4,102,111,111,50>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_1c), Array_Bin_1c),

    Array_Erl_1d = { voltarray, ?VOLT_STRING, [ <<"">>, <<"foo2">> ]},
    Array_Bin_1d = <<?VOLT_STRING,0,2,0,0,0,0,0,0,0,4,102,111,111,50>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_1d), Array_Bin_1d),

    Array_Erl_1e = [ <<"">>, <<>> ],
    Array_Bin_1e = <<?VOLT_STRING,0,2,0,0,0,0,0,0,0,0>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_1e), Array_Bin_1e),

    Array_Erl_1f = { voltarray, ?VOLT_STRING, [ <<>>, <<>> ]},
    Array_Bin_1f = <<?VOLT_STRING,0,2,0,0,0,0,0,0,0,0>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_1f), Array_Bin_1f),
    
    %%% integer

    Array_Erl_2 = [ 1, 2 ],
    Array_Bin_2 = <<?VOLT_INTINT,0,2,0,0,0,1,0,0,0,2>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_2), Array_Bin_2),

    Array_Erl_2a = [ 0, 2 ],
    Array_Bin_2a = <<?VOLT_INTINT,0,2,0,0,0,0,0,0,0,2>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_2a), Array_Bin_2a),

    Array_Erl_2b = { voltarray, [ 1, 2 ]},
    Array_Bin_2b = <<?VOLT_INTINT,0,2,0,0,0,1,0,0,0,2>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_2b), Array_Bin_2b),

    Array_Erl_2c = { voltarray, ?VOLT_INTEGER, [ 1, 2 ]},
    Array_Bin_2c = <<?VOLT_INTINT,0,2,0,0,0,1,0,0,0,2>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_2c), Array_Bin_2c),

    %%% tiny ints (have different element count binary value size)

    Array_Erl_7 = [ 1, 2 ],
    Array_Bin_7 = <<?VOLT_TINYINT,0,0,0,2,1,2>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(?VOLT_TINYINT, Array_Erl_7), Array_Bin_7),

    Array_Erl_7a = [ 0, 2 ],
    Array_Bin_7a = <<?VOLT_TINYINT,0,0,0,2,0,2>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(?VOLT_TINYINT, Array_Erl_7a), Array_Bin_7a),

    % Guesses integer: Array_Erl_7b = { voltarray, [ 1, 2 ]},

    Array_Erl_7c = { voltarray, ?VOLT_TINYINT, [ 1, 2 ]},
    Array_Bin_7c = <<?VOLT_TINYINT,0,0,0,2,1,2>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_7c), Array_Bin_7c),

    Array_Erl_7d = { voltarray, ?VOLT_TINYINT, [ 1, 2 ]},
    Array_Bin_7d = <<?VOLT_TINYINT,0,2,0,0,0,1,0,0,0,2>>,
    ?ERLUNIT_NOT_EQUAL_MSG(erlvolt:volt_array(Array_Erl_7d), Array_Bin_7d,
    "Note: If this fails, by being equal, it means tiny is encoded like int."),

    %%% strings

    Array_Erl_3 = [ "1", "2" ],
    Array_Bin_3 = <<?VOLT_STRING,0,2,0,0,0,1,49,0,0,0,1,50>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_3), Array_Bin_3),

    Array_Erl_3a = [ "0", "2" ],
    Array_Bin_3a = <<?VOLT_STRING,0,2,0,0,0,1,48,0,0,0,1,50>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_3a), Array_Bin_3a),

    Array_Erl_3b = { voltarray, [ "1", "2" ]},
    Array_Bin_3b = <<?VOLT_STRING,0,2,0,0,0,1,49,0,0,0,1,50>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_3b), Array_Bin_3b),

    Array_Erl_3c = { voltarray, ?VOLT_STRING, [ "1", "2" ]},
    Array_Bin_3c = <<?VOLT_STRING,0,2,0,0,0,1,49,0,0,0,1,50>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_3c), Array_Bin_3c),
    
    Array_Erl_3d = ["Edwina Burnam","Tabatha Gehling","Kelly Clauss","Jessie Alloway","Alana Bregman","Jessie Eichman","Allie Rogalski","Nita Coster","Kurt Walser","Ericka Dieter","Loraine Nygren","Tania Mattioli"],
    Array_Bin_3d =
       <<?VOLT_STRING,0,12, 
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
		84,97,110,105,97,32,77,97,116,116,105,111,108,105>>,
    
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_3d), Array_Bin_3d),

    Array_Erl_3e = [<<"Edwina Burnam">>,<<"Tabatha Gehling">>,<<"Kelly Clauss">>,<<"Jessie Alloway">>,<<"Alana Bregman">>,<<"Jessie Eichman">>,<<"Allie Rogalski">>,<<"Nita Coster">>,<<"Kurt Walser">>,<<"Ericka Dieter">>,<<"Loraine Nygren">>,<<"Tania Mattioli">>],
    Array_Bin_3e = Array_Bin_3d,
    
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_3e), Array_Bin_3e),

    %%% floats

    Array_Erl_4 = [ 1.0, 2.0 ],
    Array_Bin_4 = <<8,0,2,63,128,0,0,64,0,0,0>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_4), Array_Bin_4),
    
    Array_Erl_4a = [ 0.0, 2.0 ],
    Array_Bin_4a = <<8,0,2,0,0,0,0,64,0,0,0>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_4a), Array_Bin_4a),
    
    Array_Erl_4b = { voltarray, [ 1.0, 2.0 ]},
    Array_Bin_4b = <<8,0,2,63,128,0,0,64,0,0,0>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_4b), Array_Bin_4b),

    Array_Erl_4c = { voltarray, ?VOLT_FLOAT, [ 1.0, 2.0 ]},
    Array_Bin_4c = <<8,0,2,63,128,0,0,64,0,0,0>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_4c), Array_Bin_4c),

    Array_Erl_4d = { voltarray, ?VOLT_FLOAT, [ -0.1, -2.0 ]},
    Array_Bin_4d = <<8,0,2,189,204,204,205,192,0,0,0>>, % TODO: verify binary (is blindly copied)
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_4d), Array_Bin_4d),

    %%% decimals

    Array_Erl_5c = { voltarray, ?VOLT_DECIMAL, [ 1.0, 2 ]},
    Array_Bin_5c = <<22,0,2,0,0,0,0,0,0,0,0,0,0,0,232,212,165,16,0,0,0,0,0,0,0,0,0,0,0,1,209,169,74,32,0>>, % TODO: verify binary (is blindly copied)
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_5c), Array_Bin_5c),

    Array_Erl_5d = { voltarray, ?VOLT_DECIMAL, [ -0.1, -2 ]},
    Array_Bin_5d = <<22,0,2,255,255,255,255,255,255,255,255,255,255,255,232,183,137,24,0,255,255,255,255,255,255,255,255,255,255,254,46,86,181,224,0>>, % TODO: verify binary (is blindly copied)
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_5d), Array_Bin_5d),

    %%% nested arrays

    Array_Erl_6 = { voltarray, ?VOLT_ARRAY, [ [ [1.0, 2.0], [1.0, 2.0] ], [ [1.0, 2.0], [1.0, 2.0] ] ]},
    Array_Bin_6 = <<?VOLT_ARRAY,0,2,?VOLT_ARRAY,0,2,8,0,2,63,128,0,0,64,0,0,0,8,0,2,63,128,0,0,64,0,0,0,?VOLT_ARRAY,0,2,8,0,2,63,128,0,0,64,0,0,0,8,0,2,63,128,0,0,64,0,0,0>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_6), Array_Bin_6),

    Array_Erl_6b = { voltarray, ?VOLT_ARRAY, [ [1.0, 2.0], [1.0, 2.0] ]},
    Array_Bin_6b = <<?VOLT_ARRAY,0,2,8,0,2,63,128,0,0,64,0,0,0,8,0,2,63,128,0,0,64,0,0,0>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_6b), Array_Bin_6b),

    Array_Erl_6c = { voltarray, ?VOLT_ARRAY, [ { voltarray, ?VOLT_DECIMAL, [1.0, 2.0] }, { voltarray, ?VOLT_DECIMAL, [1.0, 2.0] } ]},
    Array_Bin_6c = <<?VOLT_ARRAY,0,2,22,0,2,0,0,0,0,0,0,0,0,0,0,0,232,212,165,16,0,0,0,0,0,0,0,0,0,0,0,1,209,169,74,32,0, 22,0,2,0,0,0,0,0,0,0,0,0,0,0,232,212,165,16,0,0,0,0,0,0,0,0,0,0,0,1,209,169,74,32,0>>,
    ?ERLUNIT_EQUAL(erlvolt:volt_array(Array_Erl_6c), Array_Bin_6c),

    % TODO: catch type mixes

    %%%-decode----------------------------------------------------------arrays-

    %%% never used

    %%%----------------------------------------------------------------------------
    %%% Mounting of Tests Done. Execute.
    %%%----------------------------------------------------------------------------

    erlunit:execute().
    

%%%-----------------------------------�%�-----------------------------------%%%
