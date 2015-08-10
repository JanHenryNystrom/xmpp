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
%%%   The stanza encoding/decoding for XMPP.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2014, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(xmpp_stanza).
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
-spec encode(stanza()) -> iodata().
%%--------------------------------------------------------------------
encode(Stanza) -> encode(Stanza, []).

%%--------------------------------------------------------------------
%% Function: encode() -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec encode(stanza(), []) -> iodata().
%%--------------------------------------------------------------------
encode(Stanza, Conf) -> do_encode(Stanza, Conf).

%%--------------------------------------------------------------------
%% Function: decode() -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec decode(xml()) -> stanza().
%%--------------------------------------------------------------------
decode(Stanza) -> decode(Stanza, []).

%%--------------------------------------------------------------------
%% Function: decode() -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec decode(xml(), []) -> stanza().
%%--------------------------------------------------------------------
decode(Stanza, Conf) -> do_decode(Stanza, Conf).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

do_encode(IQ = #iq{}, Conf) -> encode_iq(IQ, Conf);
do_encode(Message = #message{}, Conf) -> encode_message(Message, Conf);
do_encode(Presence = #presence{}, Conf) -> encode_presence(Presence, Conf).

encode_iq(#iq{}, _) -> #xml{}.

encode_message(#message{}, _) -> #xml{}.

encode_presence(Presence, _) ->
    #presence{from = From,
              to = To,
              type = Type,
              id = Id,
              show = Show,
              status = Status,
              priority = Priority} = Presence,
    Children1 = case Priority of
                    undefined -> [];
                    _ -> [#xml{tag = <<"priority">>,
                               children = [integer_to_binary(Priority)]}]
                end,
    Children2 =
        case Status of
            undefined -> Children1;
            _ -> [#xml{tag = <<"status">>, children = [Status]} | Children1]
        end,
    Children = case Show of
                   undefined -> Children2;
                   _ -> [#xml{tag = <<"show">>,
                              children = [atom_to_binary(Show, utf8)]} |
                         Children2]
               end,
    #xml{tag = <<"presence">>,
         attrs = encode_base_attrs(From, To, Type, Id),
         children = Children}.

encode_base_attrs(From, To, Type, Id) ->
    Attrs1 = case Id of
                 undefined -> [];
                 _ -> [{<<"id">>, Id}]
             end,
    Attrs2 = case Type of
                 undefined -> Attrs1;
                 _ -> [{<<"type">>, atom_to_binary(Type, utf8)}]
             end,
    [{<<"from">>, xmpp_jid:encode(From)}, {<<"to">>, xmpp_jid:encode(To)} |
     Attrs2].

%% ===================================================================
%% Decoding
%% ===================================================================

do_decode(XML =  #xml{tag = <<"iq">>}, Conf) -> decode_iq(XML, Conf);
do_decode(XML =  #xml{tag = <<"message">>}, Conf) -> decode_message(XML, Conf);
do_decode(XML =  #xml{tag = <<"presence">>}, Conf) ->
    decode_presence(XML, Conf).

decode_iq(_, _) -> <<>>.

decode_presence(#xml{attrs = Attrs, children = Children}, _) ->
    Presence = decode_presence_attrs(Attrs, #presence{}),
    decode_presence_children(Children, Presence).

decode_presence_attrs([], Acc) -> Acc;
decode_presence_attrs([{<<"from">>, V} | T], Acc = #presence{from=undefined}) ->
    decode_presence_attrs(T, Acc#presence{from = xmpp_jid:decode(V)});
decode_presence_attrs([{<<"to">>, V} | T], Acc = #presence{to = undefined}) ->
    decode_presence_attrs(T, Acc#presence{to = xmpp_jid:decode(V)});
decode_presence_attrs([{<<"id">>, V} | T], Acc = #presence{id = undefined}) ->
    decode_presence_attrs(T, Acc#presence{id = V});
decode_presence_attrs([{<<"type">>, V} | T], Acc = #presence{type=undefined}) ->
    Type = case V of
               <<"probe">> -> probe;
               <<"subscribe">> -> subscribe;
               <<"subscribed">> -> subscribed;
               <<"unavailable">> -> unavailable;
               <<"unsubscribe">> -> unsubscribe;
               <<"unsubscribed">> -> unsubscribed
           end,
    decode_presence_attrs(T, Acc#presence{type = Type}).

decode_presence_children([], Acc) -> Acc;
decode_presence_children([#xml{tag = <<"show">>, children = [C]} | T], Acc) ->
    Show = case C of
               <<"chat">> -> chat;
               <<"away">> -> away;
               <<"xa">> -> xa;
               <<"dnd">> -> dnd
           end,
    decode_presence_children(T, Acc#presence{show = Show});
decode_presence_children([#xml{tag = <<"status">>, children = [C]} | T], Acc)
  when is_binary(C) ->
    decode_presence_children(T, Acc#presence{status = C});
decode_presence_children([#xml{tag = <<"priority">>, children = [C]}|T], Acc) ->
    decode_presence_children(T, Acc#presence{priority = binary_to_integer(C)}).

decode_message(_, _) -> <<>>.

%% ===================================================================
%% Common parts
%% ===================================================================
