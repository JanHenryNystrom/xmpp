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
%%%   The jid encoding/decoding for XMPP.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2014, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(xmpp_jid).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2]).

%% Includes
-include_lib("xmpp/src/xmpp.hrl").

%% Types

%% Exported Types
-export_types([]).

%% Records

%% Defines

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode() -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec encode(jid()) -> iodata().
%%--------------------------------------------------------------------
encode(JID) -> encode(JID, []).

%%--------------------------------------------------------------------
%% Function: encode() -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec encode(jid(), []) -> iodata().
%%--------------------------------------------------------------------
encode(JID, _) -> encode_jid(JID).

%%--------------------------------------------------------------------
%% Function: decode() -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> jid().
%%--------------------------------------------------------------------
decode(Binary) -> decode(Binary, []).

%%--------------------------------------------------------------------
%% Function: decode() -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), []) -> jid().
%%--------------------------------------------------------------------
decode(Binary, _) -> decode_jid(Binary, <<>>).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

encode_jid(#jid{local = Local, domain = Domain, resource = undefined}) ->
    <<Local/binary, "@", Domain/binary>>;
encode_jid(#jid{local = Local, domain = Domain, resource = Resource}) ->
    <<Local/binary, "@", Domain/binary, "/", Resource/binary>>.

%% ===================================================================
%% Decoding
%% ===================================================================

decode_jid(<<$@, T/binary>>, Acc) ->
    decode_jid_domain(T, #jid{local = Acc}, <<>>);
decode_jid(<<H, T/binary>>, Acc) ->
    decode_jid(T, <<Acc/binary, H>>).

decode_jid_domain(<<>>, JID, Acc) -> JID#jid{domain = Acc};
decode_jid_domain(<<$/, T/binary>>, JID, Acc) ->
    decode_jid_resource(T, JID#jid{domain = Acc}, <<>>);
decode_jid_domain(<<H, T/binary>>, JID, Acc) ->
    decode_jid_domain(T, JID, <<Acc/binary, H>>).

decode_jid_resource(<<>>, JID, Acc) when Acc /= <<>> -> JID#jid{resource = Acc};
decode_jid_resource(<<H, T/binary>>, JID, Acc) ->
    decode_jid_resource(T, JID, <<Acc/binary, H>>).    

%% ===================================================================
%% Common parts
%% ===================================================================
