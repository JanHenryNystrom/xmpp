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
-define(XMLNS_STREAM, <<"http://etherx.jabber.org/streams">>).

%% Records
-record(stream,
        {%% XML attributes
          encoding = false :: boolean(),
          standalone = false :: boolean(),
          %% stream stanza attributes
          from :: undefined | binary(),
          to :: undefined | binary(),
          version :: binary(),
          id :: binary(),
          'xml:lang' :: undefined | binary(),
          xmlns :: stanza_ns(),
          'xmlns:stream' :: undefined | binary()
        }).

%% Types
-type stanza_ns() :: 'jabber:client' | 'jabber:server'.
