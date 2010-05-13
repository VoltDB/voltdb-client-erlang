%%%-------------------------------------------------------------------------%%%
%%% File        : test1.erl                                                 %%%
%%% Description : Unit Tests #1 for Erlang-VoltDB client API erlvolt.erl    %%% 
%%% Copyright   : VoltDB, LLC - http://www.voltdb.com                       %%%
%%% Production  : Eonblast Corporation - http://www.eonblast.com            %%%
%%% Author      : H. Diedrich <hd2010@eonblast.com>                         %%%
%%% Licence     : GPLv3                                                     %%%
%%% Created     : 25 Apr 2010                                               %%%
%%% Changed     : 12 May 2010                                               %%%
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
%%%    erlvolt.erl.
%%%
%%%----------------------------------------------------------------------------

run() ->

	erlunit:start([colors,nopasses]),
	
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

	?ERLUNIT_EQUAL(erlvolt:volt_float(nan),               ?VOLT_NAN),
	?ERLUNIT_EQUAL(erlvolt:volt_float(positive_infinity), ?VOLT_POSITIVE_INFINITY),
	?ERLUNIT_EQUAL(erlvolt:volt_float(negative_infinity), ?VOLT_NEGATIVE_INFINITY),
	
	?ERLUNIT_FAIL (erlvolt:volt_float( 0) ),
	?ERLUNIT_FAIL (erlvolt:volt_float(-1) ),
	?ERLUNIT_FAIL (erlvolt:volt_float( 1) ),
	?ERLUNIT_FAIL (erlvolt:volt_float(nil)),

	erlunit:equal(erlvolt:volt_float(0.0),               <<0:32>>,                      "Volt float  0.0"),
	erlunit:equal(erlvolt:volt_float(-1.0),              <<-1.0:32/float>>,             "Volt float -1.0"),
	erlunit:equal(erlvolt:volt_float(1.0),               <<1.0:32/float>>,              "Volt float  1.0"),
	erlunit:equal(erlvolt:volt_float( 7654321.1234567),  << 7654321.1234567:32/float>>, "Volt float  7654321.1234567"),
	erlunit:equal(erlvolt:volt_float(-7654321.1234567),  <<-7654321.1234567:32/float>>, "Volt float -7654321.1234567"),
	erlunit:equal(erlvolt:volt_float(nan),               ?VOLT_NAN,                     "Volt float  NaN"),
	erlunit:equal(erlvolt:volt_float(positive_infinity), ?VOLT_POSITIVE_INFINITY,       "Volt float  Pos. Inf."),
	erlunit:equal(erlvolt:volt_float(negative_infinity), ?VOLT_NEGATIVE_INFINITY,       "Volt float  Neg. Inf."),

	erlunit:fail (fun() -> erlvolt:volt_float(0)   end,                                  "Volt float  0"),
	erlunit:fail (fun() -> erlvolt:volt_float(-1)  end,                                  "Volt float -1"),
	erlunit:fail (fun() -> erlvolt:volt_float(1)   end,                                  "Volt float  1"),
	erlunit:fail (fun() -> erlvolt:volt_float(nil) end,                                  "Volt float  nil"),


	%*************************************************************************%
    %                                                                         %
	%                                 Strings                                 % 
    %                                                                         %
	%*************************************************************************%

	% Note: this very source file should be encoded in utf-8, not Erlang std latin-1.
	Ouml   = unicode:characters_to_list("ö", utf8),
	Delta   = unicode:characters_to_list("∂", utf8),
	OumlSize = size(<<"ö">>),
	DeltaSize = size(<<"∂">>),
	MegaString = string:chars(33, 1001000),

	erlunit:suite("Strings"),
	erlunit:equal(erlvolt:volt_string("foo"), <<3:?VOLT_STRING_SIZE_BINARY_TYPE,"foo">>,        "Volt string 'foo'"),
	erlunit:equal(erlvolt:volt_string("1"),   <<1:?VOLT_STRING_SIZE_BINARY_TYPE,"1">>,          "Volt string '1'"),
	erlunit:equal(erlvolt:volt_string(Ouml),  <<OumlSize:?VOLT_STRING_SIZE_BINARY_TYPE,"ö">>,   "Volt string 'ö'"),
	erlunit:equal(erlvolt:volt_string(Delta), <<DeltaSize:?VOLT_STRING_SIZE_BINARY_TYPE,"∂">>,  "Volt string '∂'"),
	erlunit:equal(erlvolt:volt_string(""),    <<0:?VOLT_STRING_SIZE_BINARY_TYPE>>,              "Volt empty string "),
	erlunit:equal(erlvolt:volt_string(null),  <<-1:?VOLT_STRING_SIZE_BINARY_TYPE>>,             "Volt string NULL"),
	?ERLUNIT_FAIL(erlvolt:volt_string(MegaString)), %,  <<1001000:?VOLT_STRING_SIZE_BINARY_TYPE, MegaString/binary>>, "Volt 1MB string"),


	%*************************************************************************%
    %                                                                         %
	%                               Integers                                  % 
    %                                                                         %
	%*************************************************************************%
	
	erlunit:suite("Integers"),
	erlunit:equal(erlvolt:volt_byte(0),    <<0:8>>,                        "Volt byte  0"),
	erlunit:equal(erlvolt:volt_byte(1),    <<1:8>>,                        "Volt byte  1"),
	erlunit:equal(erlvolt:volt_byte(-1),   <<-1:8>>,                       "Volt byte -1"),
	erlunit:equal(fun() -> erlvolt:volt_byte(127)  end, <<127:8/signed>>,  "Volt byte 127"),
	erlunit:fail (fun()  -> erlvolt:volt_byte(128)  end,                    "Volt byte  128"),
	erlunit:equal(fun() -> erlvolt:volt_byte(-128) end, <<-128:8/signed>>, "Volt byte -128"),
	erlunit:fail (fun()  -> erlvolt:volt_byte(-129) end,                    "Volt byte -129"),

	erlunit:equal(erlvolt:volt_short(0),    <<0:16>>,                            "Volt short  0"),
	erlunit:equal(erlvolt:volt_short(1),    <<1:16>>,                            "Volt short  1"),
	erlunit:equal(erlvolt:volt_short(-1),   <<-1:16>>,                           "Volt short -1"),
	erlunit:equal(fun() -> erlvolt:volt_short( 32767) end, <<32767:16/signed>>,  "Volt short  32767"),
	erlunit:fail (fun()  -> erlvolt:volt_short( 32768) end,                       "Volt short  32768"),
	erlunit:equal(fun() -> erlvolt:volt_short(-32768) end, <<-32768:16/signed>>, "Volt short -32768"),
	erlunit:fail (fun()  -> erlvolt:volt_short(-32769) end,                       "Volt short -32769"),

	erlunit:equal(erlvolt:volt_integer(0),    <<0:32>>,                                      "Volt integer  0"),
	erlunit:equal(erlvolt:volt_integer(1),    <<1:32>>,                                      "Volt integer  1"),
	erlunit:equal(erlvolt:volt_integer(-1),   <<-1:32>>,                                     "Volt integer -1"),
	erlunit:equal(fun() -> erlvolt:volt_integer( 2147483647) end, <<2147483647:32/signed>>,  "Volt integer  2147483647"),
	erlunit:fail (fun()  -> erlvolt:volt_integer( 2147483648) end,                            "Volt integer  2147483648"),
	erlunit:equal(fun() -> erlvolt:volt_integer(-2147483648) end, <<-2147483648:32/signed>>, "Volt integer -2147483648"),
	erlunit:fail (fun()  -> erlvolt:volt_integer(-2147483649) end,                            "Volt integer -2147483649"),

	erlunit:equal(erlvolt:volt_long(0),    <<0:64>>,                                         "Volt long  0"),
	erlunit:equal(erlvolt:volt_long(1),    <<1:64>>,                                         "Volt long  1"),
	erlunit:equal(erlvolt:volt_long(-1),   <<-1:64>>,                                        "Volt long -1"),
	erlunit:equal(fun() -> erlvolt:volt_long( 9223372036854775807) end, <<9223372036854775807:64/signed>>,
	                                                                                         "Volt long  9223372036854775807"),
	erlunit:fail (fun()  -> erlvolt:volt_long( 9223372036854775808) end,                      "Volt long  9223372036854775808"),
	erlunit:equal(fun() -> erlvolt:volt_long(-9223372036854775808) end, <<-9223372036854775808:64/signed>>,
	                                                                                         "Volt long -9223372036854775808"),
	erlunit:fail (fun()  -> erlvolt:volt_long(-9223372036854775809) end,                      "Volt long -9223372036854775809"),

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

	?ERLUNIT_EQUAL(erlvolt:erl_integer(<<1000000000000:128>>),  1  ),
	?ERLUNIT_EQUAL(erlvolt:erl_integer(<<1000000000000:128>>),  1.0),
	?ERLUNIT_FAIL (erlvolt:erl_integer(<< 999999999999:128>>)),
	?ERLUNIT_FAIL (erlvolt:erl_integer(<<1100000000000:128>>)),
	?ERLUNIT_FAIL (erlvolt:erl_integer(<<1999999999999:128>>)),
	?ERLUNIT_EQUAL(erlvolt:erl_integer(<<10000000000000:128>>),  10  ),
	?ERLUNIT_EQUAL(erlvolt:erl_integer(<<10000000000000:128>>),  10.0),
	?ERLUNIT_FAIL (erlvolt:erl_integer(<<10100000000000:128>>)),
	?ERLUNIT_FAIL (erlvolt:erl_integer(<<10999999999999:128>>)),
	?ERLUNIT_EQUAL(erlvolt:erl_integer(<<10000000000000000000:128>>),  10000000  ),
	?ERLUNIT_EQUAL(erlvolt:erl_integer(<<10000000000000000000:128>>),  10000000.0),
	?ERLUNIT_EQUAL(erlvolt:erl_integer(<<10000000000000000000:128>>),  1.0e7),
	?ERLUNIT_FAIL (erlvolt:erl_integer(<<10000000100000000000:128>>)),

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
%	?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 999999999999999999999999999000000000000:128>>),  999999999999999999999999999),
%	?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 9999999999999999999999999999000000000000:128>>),  9999999999999999999999999999),
%	?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 99999999999999999999999999999000000000000:128>>),  99999999999999999999999999999),
%	?ERLUNIT_EQUAL(erlvolt:erl_integer(<< 999999999999999999999999999999000000000000:128>>),  999999999999999999999999999999),


	% ?ERLUNIT_EQUAL(erlvolt:erl_integer(<<-99999999999999999999999999999999999999000000000000:128>>), -99999999999999999999999999999999999999),
	?ERLUNIT_FAIL (erlvolt:erl_integer( 100000000000000000000000000000000000000)),
	?ERLUNIT_FAIL (erlvolt:erl_integer(-100000000000000000000000000000000000000)),

	?ERLUNIT_FAIL (erlvolt:erl_integer(<<0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,16>>)), 
	?ERLUNIT_FAIL (erlvolt:erl_integer(<<0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,20>>)),


	% integer_or_null (same as integer but may return null).

	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<<-170141183460469231731687303715884105728:128/signed>>), null),
	?ERLUNIT_FAIL (erlvolt:erl_integer_or_null(<<255,255,255,255,255,255,255,255,255,173,33,210,178,57,217,128>>)),
	?ERLUNIT_FAIL (erlvolt:erl_integer_or_null(<<0,0,0,0,0,0,0,0,0,82,222,45,77,198,38,128>>)),

	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<<1000000000000:128>>),  1  ),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<<1000000000000:128>>),  1.0),
	?ERLUNIT_FAIL (erlvolt:erl_integer_or_null(<< 999999999999:128>>)),
	?ERLUNIT_FAIL (erlvolt:erl_integer_or_null(<<1100000000000:128>>)),
	?ERLUNIT_FAIL (erlvolt:erl_integer_or_null(<<1999999999999:128>>)),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<<10000000000000:128>>),  10  ),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<<10000000000000:128>>),  10.0),
	?ERLUNIT_FAIL (erlvolt:erl_integer_or_null(<<10100000000000:128>>)),
	?ERLUNIT_FAIL (erlvolt:erl_integer_or_null(<<10999999999999:128>>)),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<<10000000000000000000:128>>),  10000000  ),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<<10000000000000000000:128>>),  10000000.0),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<<10000000000000000000:128>>),  1.0e7),
	?ERLUNIT_FAIL (erlvolt:erl_integer_or_null(<<10000000100000000000:128>>)),

	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 999999999999998000000000000:128>>),  999999999999998),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 9999999999999980000000000000:128>>),  9999999999999980),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 99999999999999980000000000000:128>>),  99999999999999980),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 999999999999999980000000000000:128>>),  999999999999999980),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 9999999999999999910000000000000:128>>),  9999999999999999910),

	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 9999999999999999980000000000000:128>>),  9999999999999999980),

	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 999999999999999000000000000:128>>),  999999999999999),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 9999999999999999000000000000:128>>),  9999999999999999),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 99999999999999999000000000000:128>>),  99999999999999999),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 999999999999999999000000000000:128>>),  999999999999999999),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 9999999999999999999000000000000:128>>),  9999999999999999999),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 99999999999999999999000000000000:128>>),  99999999999999999999),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 999999999999999999999000000000000:128>>),  999999999999999999999),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 9999999999999999999999000000000000:128>>),  9999999999999999999999),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 99999999999999999999999000000000000:128>>),  99999999999999999999999),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 999999999999999999999999000000000000:128>>),  999999999999999999999999),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 9999999999999999999999999000000000000:128>>),  9999999999999999999999999),
	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 99999999999999999999999999000000000000:128>>),  99999999999999999999999999),
%	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 999999999999999999999999999000000000000:128>>),  999999999999999999999999999),
%	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 9999999999999999999999999999000000000000:128>>),  9999999999999999999999999999),
%	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 99999999999999999999999999999000000000000:128>>),  99999999999999999999999999999),
%	?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<< 999999999999999999999999999999000000000000:128>>),  999999999999999999999999999999),


	% ?ERLUNIT_EQUAL(erlvolt:erl_integer_or_null(<<-99999999999999999999999999999999999999000000000000:128>>), -99999999999999999999999999999999999999),
	?ERLUNIT_FAIL (erlvolt:erl_integer_or_null( 100000000000000000000000000000000000000)),
	?ERLUNIT_FAIL (erlvolt:erl_integer_or_null(-100000000000000000000000000000000000000)),

	?ERLUNIT_FAIL (erlvolt:erl_integer_or_null(<<0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,16>>)), 
	?ERLUNIT_FAIL (erlvolt:erl_integer_or_null(<<0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,20>>)),



	%*************************************************************************%
    %                                                                         %
	%                                Decimals                                 %
    %                                                                         %
	%*************************************************************************%
	
	erlunit:suite("Decimals"),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal(null), <<-170141183460469231731687303715884105728:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal(-23325.23425), <<255,255,255,255,255,255,255,255,255,173,33,210,178,57,217,128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 23325.23425), <<0,0,0,0,0,0,0,0,0,82,222,45,77,198,38,128>>),

	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 1  ),                            <<1000000000000:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 1.0),                            <<1000000000000:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 0.999999999999),                 << 999999999999:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 1.1),                            <<1100000000000:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 1.999999999999),                 <<1999999999999:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 10  ),                          <<10000000000000:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 10.0),                          <<10000000000000:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 10.1),                          <<10100000000000:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 10.999999999999),               <<10999999999999:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 10000000  ),                     <<10000000000000000000:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 10000000.0),                     <<10000000000000000000:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 10000000.1),                     <<10000000100000000000:128>>),

	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 99999999999999999999999999999999999998),  << 99999999999999999999999999999999999998000000000000:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal(-99999999999999999999999999999999999998),  <<-99999999999999999999999999999999999998000000000000:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal( 99999999999999999999999999999999999999),  << 99999999999999999999999999999999999999000000000000:128>>),
	?ERLUNIT_EQUAL(erlvolt:volt_decimal(-99999999999999999999999999999999999999),  <<-99999999999999999999999999999999999999000000000000:128>>),
	?ERLUNIT_FAIL (erlvolt:volt_decimal( 100000000000000000000000000000000000000)),
	?ERLUNIT_FAIL (erlvolt:volt_decimal(-100000000000000000000000000000000000000)),

	?ERLUNIT_EQUAL(erlvolt:volt_decimal(23325.12345678901), <<0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,16>>), 
	?ERLUNIT_EQUAL(erlvolt:volt_decimal(23325.123456789012), <<0,0,0,0,0,0,0,0,0,82,222,19,129,251,234,20>>),

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
	%																			   << 99999999999999999999999999999999999998999999999999:128>>),
	% ?ERLUNIT_EQUAL(erlvolt:volt_decimal(-99999999999999999999999999999999999998.999999999999),  
	%																			   <<-99999999999999999999999999999999999998999999999999:128>>),

	% For the same reasons, the below test cannot work:
	% 1> 10000000.999999999999.
	% 10000001.0
	%
	% ?ERLUNIT_EQUAL(erlvolt:volt_decimal(10000000.999999999999),          <<10000000999999999999:128>>),
	% ?ERLUNIT_EQUAL(erlvolt:volt_decimal(10000000.100000000001),          <<10000000100000000001:128>>),


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
	
	%	These are too big. There is an 'effective' threshold of integer usage here. TODO: find MAX and MIN.
	%	?ERLUNIT_EQUAL(erlvolt:erl_number(<< 999999999999999999999999999000000000000:128>>),  999999999999999999999999999),
	%	?ERLUNIT_EQUAL(erlvolt:erl_number(<< 9999999999999999999999999999000000000000:128>>),  9999999999999999999999999999),
	%	?ERLUNIT_EQUAL(erlvolt:erl_number(<< 99999999999999999999999999999000000000000:128>>),  99999999999999999999999999999),
	%	?ERLUNIT_EQUAL(erlvolt:erl_number(<< 999999999999999999999999999999000000000000:128>>),  999999999999999999999999999999),


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
	
	%	These are too big. There is an 'effective' threshold of integer usage here. TODO: find MAX and MIN.
	%	?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 999999999999999999999999999000000000000:128>>),  999999999999999999999999999),
	%	?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 9999999999999999999999999999000000000000:128>>),  9999999999999999999999999999),
	%	?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 99999999999999999999999999999000000000000:128>>),  99999999999999999999999999999),
	%	?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<< 999999999999999999999999999999000000000000:128>>),  999999999999999999999999999999),


	% ?ERLUNIT_EQUAL(erlvolt:erl_number_or_null(<<-99999999999999999999999999999999999999000000000000:128>>), 
	%											  -99999999999999999999999999999999999999),
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
	?ERLUNIT_EQUAL(erlvolt:volt_time({0,0,1}),      <<0:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({0,0,999}),    <<0:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({0,0,1000}),   <<1:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({0,0,1001}),   <<1:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({0,10,0}),     <<10000:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({1,0,999}),    <<1000000000:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({1,0,1000}),   <<1000000001:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({1,0,1001}),   <<1000000001:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({1,10,0}),     <<(?BILLION+10*1000):64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({1,10,1}),     <<(?BILLION+10*1000):64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({1,10,999}),   <<(?BILLION+10*1000):64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({1,10,1000}),  <<(?BILLION+10*1000+1):64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time(Now),          <<(Mega*?BILLION + Sec * 1000 + trunc(Micro/1000)):64>>),

	?ERLUNIT_EQUAL_MSG(erlvolt:volt_time({0,0,-1}),      <<0,0,0,0,0,0,0,0>>, "Catch negative 'now format' input"),
	?ERLUNIT_EQUAL_MSG(erlvolt:volt_time({0,-1,1000}),   <<255,255,255,255,255,255,252,25>>, "Catch negative 'now format' input"),
	?ERLUNIT_EQUAL_MSG(erlvolt:volt_time({-1,0,-999}),   <<255,255,255,255,196,101,54,0>>, "Catch negative 'now format' input"),
	?ERLUNIT_EQUAL_MSG(erlvolt:volt_time({-1,1,-1001}),  <<255,255,255,255,196,101,57,231>>, "Catch negative 'now format' input"),


	% from Erlang datetime
	?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,1,1},{0,0,0}}),      <<0:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({{1969,12,31},{23,59,59}}), <<-1000:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,1,1},{0,0,1}}),      <<1000:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,12,31},{23,59,59}}), <<(365*24*60*60*1000-1000):64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,12,31},{23,59,59}}), <<0,0,0,7,87,177,40,24>>),               % TODO: verify
	?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,1,10},{1,1,1}}),     <<0,0,0,0,46,145,24,200>>),              % TODO: verify
	?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,10,1},{1,1,1}}),     <<0,0,0,5,126,31,248,200>> ),            % TODO: verify
	?ERLUNIT_EQUAL(erlvolt:volt_time({{2010,3,3},{12,0,0}}),     <<0,0,1,39,35,229,146,0>>),              % TODO: verify
	?ERLUNIT_EQUAL(erlvolt:volt_time({{1950,2,2},{23,59,59}}),   <<255,255,255,109,182,90,156,24>>),      % TODO: verify
	?ERLUNIT_EQUAL(erlvolt:volt_time({{2000,10,10},{10,10,10}}), <<0,0,0,226,30,101,255,208>>),           % TODO: verify
	?ERLUNIT_EQUAL(erlvolt:volt_time({{0,1,1},{0,0,0}}),         <<255,255,199,117,144,251,160,0>>),      % TODO: verify

	% overrunning date and time values
	?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,12,31},{23,59,60}}), <<(365*24*60*60*1000):64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({{1970,12,31},{23,59,61}}), <<(365*24*60*60*1000+1000):64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({{1971,1,1},{0,0,1}}),      <<(365*24*60*60*1000+1000):64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time({{1971,1,1},{0,0,-1}}),     <<0,0,0,7,87,177,40,24>>),               % TODO: verify
	?ERLUNIT_EQUAL(erlvolt:volt_time({{2020,1,1},{33,10,11}}),   <<0,0,1,111,101,132,249,56>> ),          % TODO: verify

	% invalid date and time values
	?ERLUNIT_FAIL (erlvolt:volt_time({{0,0,0},{0,0,0}})),
	?ERLUNIT_FAIL (erlvolt:volt_time({{-1,1,1},{0,0,0}})),
	?ERLUNIT_FAIL (erlvolt:volt_time({{ 1000000,0,0},{0,0,0}})),
	?ERLUNIT_FAIL (erlvolt:volt_time({{-1000000,0,0},{0,0,0}})),


	% from unix epoch seconds integer
	?ERLUNIT_EQUAL(erlvolt:volt_time(   0),   <<      0:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time(   1),   <<   1000:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time(  -1),   <<  -1000:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time(  10),   <<  10000:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time( -10),   << -10000:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time( 999),   << 999000:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time(-999),   <<-999000:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time(1000),   <<1000000:64>>),
	?ERLUNIT_EQUAL(erlvolt:volt_time(1001),   <<1001000:64>>),

	?ERLUNIT_FAIL_MSG(erlvolt:volt_time(1.0),   "Catch float input"),
	?ERLUNIT_FAIL_MSG(erlvolt:volt_time(-1.0),  "Catch float input"),


	%%%-decode----------------------------------------------------------------time-


	% to Erlang 'now' time format {Megasec,Sec,Microsec} from (millisec) Integer -
	% that function represents more of an intermediary step really but might come 
	% in handy somewhere
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(0),   {0,0,0}),
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(1),    {0,0,1000}),
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(-1),    {0,0,-1000}),
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(10000),   {0,10,0}),
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(1000000001),   {1,0,1000}),
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(?BILLION+10*1000),   {1,10,0}),
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(?BILLION+10*1000+1),   {1,10,1000}),
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(Mega*?BILLION + Sec * 1000 + trunc(Micro/1000)), {Mega,Sec,trunc(Micro/1000)*1000}),


	% to Erlang 'now' time format {Megasec,Sec,Microsec} from binary
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<0:64>>),   {0,0,0}),
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<1:64>>),    {0,0,1000}),
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<-1:64>>),    {0,0,-1000}),
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<10000:64>>),   {0,10,0}),
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<1000000001:64>>),   {1,0,1000}),
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<(?BILLION+10*1000):64>>),   {1,10,0}),
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<(?BILLION+10*1000+1):64>>),   {1,10,1000}),
	?ERLUNIT_EQUAL(erlvolt:erl_nowtime(<<(Mega*?BILLION + Sec * 1000 + trunc(Micro/1000)):64>>), {Mega,Sec,trunc(Micro/1000)*1000}),


	% from Erlang datetime
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<-1000:64>>),  {{1969,12,31},{23,59,59}}),
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<-999:64>>),   {{1970,1,1},{0,0,0}}),
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<  -1:64>>),   {{1970,1,1},{0,0,0}}),
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<  0:64>>),    {{1970,1,1},{0,0,0}}),
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<  1:64>>),    {{1970,1,1},{0,0,0}}),
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<999:64>>),    {{1970,1,1},{0,0,0}}),
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<-1000:64>>),  {{1969,12,31},{23,59,59}}),
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<-1000:64>>),  {{1969,12,31},{23,59,59}}),
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<-1000:64>>),  {{1969,12,31},{23,59,59}}),
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<1000:64>>),   {{1970,1,1},{0,0,1}}),
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<1000:64>>),   {{1970,1,1},{0,0,1}}),
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<1000:64>>),   {{1970,1,1},{0,0,1}}),
	
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<(365*24*60*60*1000+1000):64>>),   {{1971,1,1},{0,0,1}}),
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<(365*24*60*60*1000-1000):64>>),   {{1970,12,31},{23,59,59}}),
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<0,0,0,7,87,177,40,24>>),          {{1970,12,31},{23,59,59}}),    % TODO: verify
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<0,0,0,0,46,145,24,200>>),         {{1970,1,10},{1,1,1}}),        % TODO: verify
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<0,0,0,5,126,31,248,200>>),        {{1970,10,1},{1,1,1}} ),       % TODO: verify
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<0,0,1,39,35,229,146,0>>),         {{2010,3,3},{12,0,0}}),        % TODO: verify
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<255,255,255,109,182,90,156,24>>), {{1950,2,2},{23,59,59}}),      % TODO: verify
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<0,0,0,226,30,101,255,208>>),      {{2000,10,10},{10,10,10}}),    % TODO: verify
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<0,0,1,111,101,132,249,56>>),      {{2020,1,2},{9,10,11}}),       % TODO: verify
	?ERLUNIT_EQUAL(erlvolt:erl_datetime(<<255,255,199,117,144,251,160,0>>), {{0,1,1},{0,0,0}}),            % TODO: verify


	% to unix epoch time format (seconds since 1970) from (millisec) Integer -
	% that function represents more of an intermediary step but might come 
	% in handy somewhere
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(0),   0),
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(1),   0),
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(-1),  0),
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(10000),   10),
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(1000000001),   1000000),
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(?BILLION+10*1000),   ?MILLION + 10),
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(?BILLION+10*1000+1), ?MILLION + 10),
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(Mega*?BILLION + Sec * 1000 + trunc(Micro/1000)), 
		trunc((Mega*?BILLION + Sec * 1000 + trunc(Micro/1000))/1000)),


	% to unix epoch time format (seconds since 1970) from VoltDB binary time wire code
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<0:64>>),   0),
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<1:64>>),   0),
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<-1:64>>),  0),
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<10000:64>>),   10),
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<1000000001:64>>),   1000000),
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<(?BILLION+10*1000):64>>),   ?MILLION + 10),
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<(?BILLION+10*1000+1):64>>), ?MILLION + 10),
	?ERLUNIT_EQUAL(erlvolt:erl_unixtime(<<(Mega*?BILLION + Sec * 1000 + trunc(Micro/1000)):64>>), 
		trunc((Mega*?BILLION + Sec * 1000 + trunc(Micro/1000))/1000)),

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

	%%%-decode----------------------------------------------------------arrays-

	%%%   --- pg. 5, VoltDB Client Wire Protocol Version 0, 12/11/09 ---        
	% ArrBin1 = <<?VOLT_STRING,0,2,0,0,0,4,102,111,111,49,0,0,0,4,102,111,111,50>>,
	% ArrErl1 = [ <<"foo1">>, <<"foo2">> ],

	% ArrBin2 = <<?VOLT_TINYINT,0,0,0,2,1,2>>,
	% ArrErl2 = [ 1, 2 ],

	% ?ERLUNIT_EQUAL(erlvolt:erl_array(ArrBin1), ArrErl1),
	% ?ERLUNIT_EQUAL(erlvolt:erl_array(ArrBin2), ArrErl2),

	%%%----------------------------------------------------------------------------
	%%% Mounting of Tests Done. Execute.
	%%%----------------------------------------------------------------------------

	erlunit:execute().
	

