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

%%--------
%% Defines
%%--------

%% Stanzas

%% Stream

-define(SIMP_STREAM, #stream{}).
-define(STREAM_IN,
        #stream{to = <<"wonderland.lit">>,
                version = <<"1.0">>,
                xmlns = <<"jabber:client">>,
                'xml:lang' = <<"en">>,
                'xmlns:stream' = 
                    <<"http:\/\/etherx.jabber.org\/streams">>}).
-define(STREAM_OUT,
        #stream{from = <<"wonderland.lit">>,
                version = <<"1.0">>,
                id = <<"someid">>,
                'xml:lang' = <<"en">>,
                xmlns = <<"jabber:client">>,
                'xmlns:stream' =
                    <<"http:\/\/etherx.jabber.org\/streams">>}).
-define(STREAM_END_XML, eos).

%% Escape
-define(ESCAPE_XML,
        #xml{tag = <<"test">>,
             attrs = [{<<"attr">>, <<"\"'<&>'\"">>}],
             children = [<<"Ha \"'<&>'\"">>]}).

%% Presence
-define(PRESENCE_SUBSCRIBE_XML,
        #xml{tag = <<"presence">>,
             attrs = [{<<"from">>, <<"alice@wonderland.lit">>},
                      {<<"to">>, <<"sister@realworld.lit">>},
                      {<<"type">>, <<"subscribe">>}
                     ]}).

-define(PRESENCE_SUBSCRIBED_XML,
        #xml{tag = <<"presence">>,
             attrs = [{<<"from">>, <<"sister@realworld.lit">>},
                      {<<"to">>, <<"alice@wonderland.lit">>},
                      {<<"type">>, <<"subscribed">>}
                     ]}).

-define(PRESENCE_NOTIFICATION_XML,
        #xml{tag = <<"presence">>,
             attrs = [{<<"from">>, <<"alice@wonderland.lit/rabbithole">>},
                      {<<"to">>, <<"sister@realworld.lit">>}]}).

-define(PRESENCE_PROBE_XML,
        #xml{tag = <<"presence">>,
             attrs = [{<<"from">>, <<"alice@wonderland.lit/rabbithole">>},
                      {<<"to">>, <<"sister@realworld.lit">>},
                      {<<"type">>, <<"probe">>}]}).

%%
%% "Delayed Delivery" XEP-0203
%%
-define(PRESENCE_DELAY_XML,
        #xml{tag = <<"presence">>,
             attrs = [{<<"from">>, <<"sister@realworld.lit/home">>},
                      {<<"to">>, <<"alice@wonderland.lit/rabbithole">>},
                      {<<"type">>, <<"unavailable">>}],
             children = [#xml{tag = <<"delay">>,
                              attrs = [{<<"xmlns">>, <<"urn:xmpp:delay">>},
                                       {<<"stamp">>,<<"2008-11-26T15:59:09Z">>}]
                             }
                        ]
            }).

-define(PRESENCE_SHOW_XML,
        #xml{tag = <<"presence">>,
             attrs = [{<<"from">>, <<"alice@wonderland.lit/rabbithole">>},
                      {<<"to">>, <<"sister@realworld.lit">>}],
             children = [#xml{tag = <<"show">>,
                              children = [<<"xa">>]},
                         #xml{tag = <<"status">>,
                              children = [<<"down the rabbit hole!">>]}
                        ]}).
