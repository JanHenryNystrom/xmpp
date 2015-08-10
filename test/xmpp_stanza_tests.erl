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
%%%   eunit unit tests for the XMPP XML library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2014, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(xmpp_stanza_tests).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Includes
-include_lib("eunit/include/eunit.hrl").
-include_lib("xmpp/src/xmpp.hrl").
-include_lib("xmpp/test/xmpp_coding_defs.hrl").

%%--------
%% Defines
%%--------

%% Stanzas

%% Stream
%% -define(SIMP_STREAM_STANZA, #xml{tag = <<"stream:stream">>}).

%% Presence
-define(PRESENCE_SUBSCRIBE_STANZA,
        #presence{from = #jid{local = <<"alice">>,
                              domain = <<"wonderland.lit">>},
                  to = #jid{local = <<"sister">>,
                           domain = <<"realworld.lit">>},
                  type = subscribe}).

-define(PRESENCE_SUBSCRIBED_STANZA,
        #presence{from = #jid{local = <<"sister">>,
                              domain = <<"realworld.lit">>},
                  to = #jid{local = <<"alice">>,
                            domain = <<"wonderland.lit">>},
                  type = subscribed}).

-define(PRESENCE_NOTIFICATION_STANZA,
        #presence{from = #jid{local = <<"alice">>,
                              domain = <<"wonderland.lit">>,
                             resource = <<"rabbithole">>},
                  to = #jid{local = <<"sister">>,
                           domain = <<"realworld.lit">>}}).

-define(PRESENCE_PROBE_STANZA,
        #presence{from = #jid{local = <<"alice">>,
                              domain = <<"wonderland.lit">>,
                             resource = <<"rabbithole">>},
                  to = #jid{local = <<"sister">>,
                           domain = <<"realworld.lit">>},
                 type = probe}).

%% %%
%% %% "Delayed Delivery" XEP-0203
%% %%
%% -define(PRESENCE_DELAY_STANZA,
%%         #xml{tag = <<"presence">>,
%%              attrs = [{<<"from">>, <<"sister@realworld.lit/home">>},
%%                       {<<"to">>, <<"alice@wonderland.lit/rabbithole">>},
%%                       {<<"type">>, <<"unavailable">>}],
%%              children = [#xml{tag = <<"delay">>,
%%                               attrs = [{<<"xmlns">>, <<"urn:xmpp:delay">>},
%%                                        {<<"stamp">>,<<"2008-11-26T15:59:09Z">>}]
%%                              }
%%                         ]
%%             }).

-define(PRESENCE_SHOW_STANZA,
        #presence{from = #jid{local = <<"alice">>,
                              domain = <<"wonderland.lit">>,
                             resource = <<"rabbithole">>},
                  to = #jid{local = <<"sister">>,
                           domain = <<"realworld.lit">>},
                  show = xa,
                  status = <<"down the rabbit hole!">>}).

%% ===================================================================
%% Tests.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

%%--------------------------------------------------------------------
%% stream
%%--------------------------------------------------------------------
%% encode_stream_test_() ->
%%     [{"Simple",
%%       ?_test(?assertEqual(?SIMP_STREAM_STANZA,
%%                           xmpp_stanza:encode_stream([])))}
%%     ].

%%--------------------------------------------------------------------
%% Presence
%%--------------------------------------------------------------------
encode_presence_test_() ->
    [{"Subscribe",
      ?_test(
         ?assertEqual(?PRESENCE_SUBSCRIBE_XML,
                      xmpp_stanza:encode(?PRESENCE_SUBSCRIBE_STANZA)))},
     {"Subscribed",
      ?_test(
         ?assertEqual(?PRESENCE_SUBSCRIBED_XML,
                      xmpp_stanza:encode(?PRESENCE_SUBSCRIBED_STANZA)))},
     {"Notification",
      ?_test(
         ?assertEqual(?PRESENCE_NOTIFICATION_XML,
                      xmpp_stanza:encode(?PRESENCE_NOTIFICATION_STANZA)))},
     {"Probe",
      ?_test(
         ?assertEqual(?PRESENCE_PROBE_XML,
                      xmpp_stanza:encode(?PRESENCE_PROBE_STANZA)))},
     %% {"Delay",
     %%  ?_test(
     %%     ?assertEqual(?PRESENCE_DELAY_XML,
     %%                  xmpp_stanza:encode(?PRESENCE_DELAY_STANZA)))},
     {"Show",
      ?_test(
         ?assertEqual(?PRESENCE_SHOW_XML,
                      xmpp_stanza:encode(?PRESENCE_SHOW_STANZA)))}
    ].

%% ===================================================================
%% Decoding
%% ===================================================================

%%--------------------------------------------------------------------
%% Stream
%%--------------------------------------------------------------------
%% decode_stream_test_() ->
%%     [{"Simple",
%%       ?_test(equal_decode({?SIMP_STREAM_STANZA, <<>>},
%%                           xmpp_stanza:decode_stream(?SIMP_STREAM_XML)))}
%%     ].

%%--------------------------------------------------------------------
%% Presence
%%--------------------------------------------------------------------
decode_presence_test_() ->
    [{"Subscribe",
      ?_test(?assertEqual(?PRESENCE_SUBSCRIBE_STANZA,
                          xmpp_stanza:decode(?PRESENCE_SUBSCRIBE_XML)))},
     {"Subscribed",
      ?_test(?assertEqual(?PRESENCE_SUBSCRIBED_STANZA,
                          xmpp_stanza:decode(?PRESENCE_SUBSCRIBED_XML)))},
     {"Notification",
      ?_test(?assertEqual(?PRESENCE_NOTIFICATION_STANZA,
                          xmpp_stanza:decode(?PRESENCE_NOTIFICATION_XML)))},
     {"Probe",
      ?_test(?assertEqual(?PRESENCE_PROBE_STANZA,
                          xmpp_stanza:decode(?PRESENCE_PROBE_XML)))},
     %% {"Delay",
     %%  ?_test(equal_decode(?PRESENCE_DELAY_STANZA,
     %%                      xmpp_stanza:decode(?PRESENCE_DELAY_XML)))},
     {"Show",
      ?_test(?assertEqual(?PRESENCE_SHOW_STANZA,
                          xmpp_stanza:decode(?PRESENCE_SHOW_XML)))}
    ].

%% ===================================================================
%% Encoding/Decoding
%% ===================================================================

%% ===================================================================
%% Common functions.
%% ===================================================================

