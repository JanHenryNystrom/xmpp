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
%%%   The stream encoding/decoding for XMPP.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2014, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(xmpp_stream).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, decode/1]).

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
-spec encode(#stream{} | stop) -> iodata().
%%--------------------------------------------------------------------
encode(stop) -> <<"</stream:stream>">>;
encode(Stream) -> [<<"<?xml version=\'1.0\'">>, do_encode(Stream), <<">">>].

%%--------------------------------------------------------------------
%% Function: decode_stream() -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) -> {stanza(), binary()}.
%%--------------------------------------------------------------------
decode(Binary) -> decode_prolog(Binary).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

do_encode(Stream = #stream{encoding = false, standalone = false}) ->
    [<<"?><stream:stream">>, do_encode_attrs(from, Stream)];
do_encode(Stream = #stream{encoding = true, standalone = false}) ->
    [<<"encoding='UTF-8'?><stream:stream">>, do_encode_attrs(from, Stream)];
do_encode(Stream = #stream{encoding = false, standalone = true}) ->
    [<<"standalone='yes'?><stream:stream">>, do_encode_attrs(from, Stream)];
do_encode(Stream = #stream{encoding = true, standalone = true}) ->
    [<<"encoding='UTF-8' standalone='yes'?><stream:stream">>,
     do_encode_attrs(from, Stream)].

do_encode_attrs(from, Stream = #stream{from = undefined}) ->
    do_encode_attrs(to, Stream);
do_encode_attrs(from, Stream = #stream{from = From}) ->
    [<<" from='">>, From, <<"'">>, do_encode_attrs(to, Stream)];
do_encode_attrs(to, Stream = #stream{to = undefined}) ->
    do_encode_attrs(version, Stream);
do_encode_attrs(to, Stream = #stream{to = To}) ->
    [<<" to='">>, To, <<"'">>, do_encode_attrs(version, Stream)];
do_encode_attrs(version, Stream = #stream{version = undefined}) ->
    do_encode_attrs(id, Stream);
do_encode_attrs(version, Stream = #stream{version = Version}) ->
    [<<" version='">>, Version, <<"'">>, do_encode_attrs(id, Stream)];
do_encode_attrs(id, Stream = #stream{id = undefined}) ->
    do_encode_attrs('xml:lang', Stream);
do_encode_attrs(id, Stream = #stream{id = Id}) ->
    [<<" id='">>, Id, <<"'">>, do_encode_attrs('xml:lang', Stream)];
do_encode_attrs('xml:lang', Stream = #stream{'xml:lang' = undefined}) ->
    do_encode_attrs(xmlns, Stream);
do_encode_attrs('xml:lang', Stream = #stream{'xml:lang' = XmlLang}) ->
    [<<" xml:lang='">>, XmlLang, <<"'">>, do_encode_attrs(xmlns, Stream)];
do_encode_attrs(xmlns, Stream = #stream{xmlns = undefined}) ->
    do_encode_attrs('xmlns:stream', Stream);
do_encode_attrs(xmlns, Stream = #stream{xmlns = Xmlns}) ->
    [<<" xmlns='">>, Xmlns, <<"'">>, do_encode_attrs('xmlns:stream', Stream)];
do_encode_attrs('xmlns:stream', #stream{'xmlns:stream' = undefined}) ->
    [];
do_encode_attrs('xmlns:stream', #stream{'xmlns:stream' = XmlnsStream}) ->
    [<<" xmlns:stream='">>, XmlnsStream, <<"'">>].

%% ===================================================================
%% Decoding
%% ===================================================================

decode_prolog(B) ->
    case skip_attr(<<"version">>, <<"1.0">>, skip_str(<<"<?xml">>, B)) of
        <<"?>", T/binary>> -> decode_start(skip_ws(T));
        <<"encoding", T/binary>> ->
            case skip_value(<<"UTF-8">>, T) of
                <<"?>", T1/binary>> -> decode_start(skip_ws(T1));
                <<"standalone", T1/binary>> ->
                    T2 = skip_str(<<"?>">>, skip_value(<<"yes">>, T1)),
                    decode_start(skip_ws(T2))
                end;
        <<"standalone", T/binary>> ->
            T1 = skip_str(<<"?>">>, skip_value(<<"yes">>, T)),
            decode_start(skip_ws(T1))
    end.

decode_start(Binary) ->
    decode_name(skip_str(<<"<stream:stream">>, Binary), #stream{}).

decode_name(<<$>, T/binary>>, Stream) -> {Stream, T};
decode_name(Binary, Stream) ->
    {Name, T} = get_name(Binary, <<>>),
    {Value, T1} = get_value(T),
    decode_name(T1, attribute(Name, Value, Stream)).

skip_ws(<<H, T/binary>>) when ?WS(H) -> skip_ws(T);
skip_ws(Binary) ->  Binary.

skip_eq(<<$=, T/binary>>) -> skip_ws(T).

skip_str(<<>>, T) -> skip_ws(T);
skip_str(<<H, T1/binary>>, <<H, T2/binary>>) -> skip_str(T1, T2).

skip_quoted(Value, <<$', T/binary>>) ->
    <<$', T1/binary>> = skip_str(Value, T),
    skip_ws(T1);
skip_quoted(Value, <<$\", T/binary>>) ->
    <<$\", T1/binary>> = skip_str(Value, T),
    skip_ws(T1).

skip_attr(Name, Value, Binary) ->
    skip_value(Value, skip_str(Name, Binary)).

skip_value(Value, Binary) -> skip_quoted(Value, skip_eq(Binary)).

get_name(<<$=, T/binary>>, Acc) -> {Acc, skip_ws(T)};
get_name(<<H, T/binary>>, Acc) when ?WS(H) -> {Acc, skip_ws(skip_eq(T))};
get_name(<<H, T/binary>>, Acc) -> get_name(T, <<Acc/binary, H>>).

get_value(<<$', T/binary>>) -> get_value(T, $', <<>>);
get_value(<<$", T/binary>>) -> get_value(T, $", <<>>).

get_value(<<Q, T/binary>>, Q, Acc) -> {Acc, skip_ws(T)};
get_value(<<H, T/binary>>, Q, Acc) -> get_value(T, Q, <<Acc/binary, H>>).

attribute(<<"from">>, From, Stream = #stream{from = undefined}) ->
    Stream#stream{from = From};
attribute(<<"to">>, To, Stream = #stream{to = undefined}) -> 
    Stream#stream{to = To};
attribute(<<"version">>, Version, Stream = #stream{version = undefined}) -> 
    Stream#stream{version = Version};
attribute(<<"id">>, Id, Stream = #stream{id = undefined}) -> 
    Stream#stream{id = Id};
attribute(<<"xml:lang">>, XmlLang, Stream = #stream{'xml:lang' = undefined}) -> 
    Stream#stream{'xml:lang' = XmlLang};
attribute(<<"xmlns">>, Xmlns, Stream = #stream{xmlns = undefined}) -> 
    Stream#stream{xmlns = Xmlns};
attribute(<<"xmlns:stream">>,
          XmlnsStream,
          Stream = #stream{'xmlns:stream' = undefined}) -> 
    Stream#stream{'xmlns:stream' = XmlnsStream}.
