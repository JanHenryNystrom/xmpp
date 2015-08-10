%%==============================================================================
%% Copyright 2014 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%% http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%==============================================================================

%%%-------------------------------------------------------------------
%%% @doc
%%%   eunit unit tests for the XMPP Stream library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2014, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(xmpp_stream_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/src/xmpp.hrl").
-include_lib("xmpp/test/xmpp_coding_defs.hrl").

%%--------
%% Defines
%%--------

%% Stanzas

-define(SIMP_STREAM_BIN, <<"<?xml version=\'1.0\'?><stream:stream>">>).
-define(STREAM_IN_BIN,
        <<"<?xml version=\'1.0\'?>"
          "<stream:stream"
          "  to='wonderland.lit' "
          "  version='1.0'"
          "  xml:lang='en' "
          "  xmlns='jabber:client' "
          "  xmlns:stream='http:\/\/etherx.jabber.org\/streams'"
          ">">>).
-define(STREAM_OUT_BIN,
        <<"<?xml version=\'1.0\'?>"
          "<stream:stream"
          "  from='wonderland.lit' "
          "  version='1.0'"
          "  id='someid' "
          "  xml:lang='en' "
          "  xmlns='jabber:client' "
          "  xmlns:stream='http:\/\/etherx.jabber.org\/streams'"
          ">">>).
-define(STREAM_END_BIN, <<"</stream:stream>">>).

%% ===================================================================
%% Tests.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

encode_stream_test_() ->
    [{"Simple",
      ?_test(?assertEqual(?SIMP_STREAM_BIN,
                          iolist_to_binary(xmpp_stream:encode(?SIMP_STREAM))))},
     {"In",
      ?_test(?assertEqual(
                normalise(?STREAM_IN_BIN),
                iolist_to_binary(
                  xmpp_stream:encode(?STREAM_IN))))},
     {"Out",
      ?_test(?assertEqual(
                normalise(?STREAM_OUT_BIN),
                iolist_to_binary(
                  xmpp_stream:encode(?STREAM_OUT))))},
     {"End",
      ?_test(?assertEqual(?STREAM_END_BIN,
                          iolist_to_binary(xmpp_stream:encode(stop))))}
    ].

normalise(Bin) -> normalise(Bin, <<>>).

normalise(<<>>, Acc) -> Acc;
normalise(<<$\n, T/binary>>, Acc) -> normalise(<<$\s, T/binary>>, Acc);
normalise(<<$\s, $\s, T/binary>>, Acc) -> normalise(<<$\s, T/binary>>, Acc);
normalise(<<H, T/binary>>, Acc) -> normalise(T, <<Acc/binary, H>>).

%% ===================================================================
%% Decoding
%% ===================================================================

decode_stream_test_() ->
    [{"Simple",
      ?_test(?assertMatch({#stream{}, <<>>},
                          xmpp_stream:decode(?SIMP_STREAM_BIN)))},
     {"In",
      ?_test(?assertMatch({#stream{to = <<"wonderland.lit">>,
                                   xmlns = <<"jabber:client">>,
                                   'xmlns:stream' = 
                                       <<"http:\/\/etherx.jabber.org\/streams">>,
                                   'xml:lang' = <<"en">>,
                                   version = <<"1.0">>},
                           <<>>},
                          xmpp_stream:decode(?STREAM_IN_BIN)))},
     {"Out",
      ?_test(?assertMatch({#stream{from = <<"wonderland.lit">>,
                                   id = <<"someid">>,
                                   xmlns = <<"jabber:client">>,
                                   'xmlns:stream' = 
                                       <<"http:\/\/etherx.jabber.org\/streams">>,
                                   'xml:lang' = <<"en">>,
                                   version = <<"1.0">>},
                           <<>>},
                          xmpp_stream:decode(?STREAM_OUT_BIN)))}
    ].

%% ===================================================================
%% Common functions.
%% ===================================================================

