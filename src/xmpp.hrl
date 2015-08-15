%% -*-erlang-*-
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

%% Includes
-include_lib("xmpp/src/xml.hrl").
-include_lib("xmpp/src/stream.hrl").

%% Defines
-define(WS(C), C =< $\s).

%% Records
-record(iq,
        { %% Attributes
          from :: undefined | jid(),
          to :: undefined | jid(),
          type :: iq_type(),
          id :: binary(),
          'xml:lang' :: undefined | binary(),
          xmlns :: stanza_ns()
        }).

-record(presence,
        { %% Attributes
          from :: undefined | jid(),
          to :: undefined | jid(),
          type :: presence_type(),
          id :: undefined | binary(),
          'xml:lang' :: undefined | binary(),
          xmlns :: stanza_ns(),
          %% Children
          show :: presence_show(), 
          status :: undefined | binary(),
          priority :: undefined | integer()
        }).

-record(message,
        {%% Attributes
          from :: undefined | jid(),
          to :: undefined | jid(),
          type :: message_type(),
          id :: undefined | binary(),
          'xml:lang' :: undefined | binary(),
          xmlns :: stanza_ns()
        }).

-record(jid, {local :: binary(),
              domain :: binary(),
              resource :: binary(),
              nlocal :: binary(),
              ndomain :: binary(),
              nresource :: binary()}).

%% Types
-type iq_type() :: get | set | result | error.
-type presence_type() :: undefined | probe | subscribe | subscribed |
                         unavailable | unsubscribe | unsubscribed.
-type presence_show() :: undefined | chat | away | xa | dnd.
-type message_type() ::
        undefined | normal | chat | groupchat | headline | error.
-type jid() :: #jid{}.
-type stanza() :: #stream{} | #iq{} | #presence{} | #message{}.
-type config() :: [{atom(), _}].
