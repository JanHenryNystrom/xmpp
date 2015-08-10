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
%%% The main supervisor xmpp.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2014, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(xmpp_sup).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

-behaviour(supervisor).

%% Management API
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

%% Includes
-include_lib("xmpp/src/xmpp.hrl").

%% Types
-type init_return() :: {ok,
                        {{supervisor:strategy(), integer(), integer()},
                         [supervisor:child_spec()]}}.

%% ===================================================================
%% Management API
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: start_link() -> {ok, Pid}
%% @doc
%%   Starts the main supervisor of the xmpp application.
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> {ok, pid()} | ignore | {error, _}.
%%--------------------------------------------------------------------
start_link() -> supervisor:start_link({local, ?MODULE}, ?MODULE, no_arg).

%% ===================================================================
%% supervisor callbacks
%% ===================================================================

%%--------------------------------------------------------------------
-spec init(no_arg) -> init_return().
%%--------------------------------------------------------------------
init(no_arg) -> {ok, {{one_for_one, 2, 3600}, []}}.

%% ===================================================================
%% Internal functions.
%% ===================================================================
