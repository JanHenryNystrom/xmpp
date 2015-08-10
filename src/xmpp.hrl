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

%% Defines
-define(WS(C), C =< $\s).

-define(XMLNS_STREAM, <<"http://etherx.jabber.org/streams">>).

%% Records
-record(xml, {tag :: atom() | binary(),
              attrs = [] :: [attr()],
              children = [] :: [xml() | cdata() | binary()]}).
-record(cdata, {data = <<>> :: binary()}).

-record(stream,
        {%% XML attributes
          encoding = false :: boolean(),
          standalone = false :: boolean(),
          %% stream stanza attributes
          from :: undefined | jid(),
          to :: undefined | jid(),
          version :: binary(),
          id :: binary(),
          'xml:lang' :: undefined | binary(),
          xmlns :: stanza_ns(),
          'xmlns:stream' :: undefined | binary()
        }).

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
-type attr() :: {atom() | binary(), binary()}.
-type xml() :: #xml{}.
-type cdata() :: #cdata{}.

-type opt() :: {atom(), _}.

-type stanza_ns() :: 'jabber:client' | 'jabber:server'.
-type iq_type() :: get | set | result | error.
-type presence_type() :: undefined | probe | subscribe | subscribed |
                         unavailable | unsubscribe | unsubscribed.
-type presence_show() :: undefined | chat | away | xa | dnd.
-type message_type() ::
        undefined | normal | chat | groupchat | headline | error.
-type jid() :: #jid{}.
-type stanza() :: #stream{} | #iq{} | #presence{} | #message{}.
-type config() :: [{atom(), _}].
