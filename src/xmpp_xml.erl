%==============================================================================
%% Copyright 2014-2015 Jan Henry Nystrom <JanHenryNystrom@gmail.com>
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
%%%   The xml encoding/decoding for XMPP.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2014-2015, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(xmpp_xml).
-copyright('Jan Henry Nystrom <JanHenryNystrom@gmail.com>').

%% Library functions
-export([encode/1, encode/2,
         decode/1, decode/2]).

%% Includes
-include_lib("xmpp/src/xmpp.hrl").

%% Exported Types
-export_types([]).

%% Records
-record(state, {current = undefined :: xml() | cdata() | binary(),
                stack = [] :: [xml()],
                func :: func(),
                args = [] :: args()
               }).

%% Types
-type state() :: #state{}.
-type func() :: atom().
-type args() :: tuple().

%% Defines
-define(START_CHAR(C),
            C =:= 16#3A; %% :
            C >= 16#41, C =< 16#5A; %% A-Z
            C =:= 16#5F; %% _
            C >= 16#61, C =< 16#7A; %% a-z
            C >= 16#C0, C =< 16#D6; %% À-Ö
            C >= 16#D8, C =< 16#F6; %% Ø-ö
            C >= 16#F8, C =< 16#2FF; %% ø-
            C >= 16#370, C =< 16#37D;
            C >= 16#37F, C =< 16#1FFF;
            C >= 16#200C, C =< 16#200D;
            C >= 16#2070, C =< 16#218F;
            C >= 16#2C00, C =< 16#2FEF;
            C >= 16#3001, C =< 16#D7FF;
            C >= 16#F900, C =< 16#FDCF;
            C >= 16#FDF0, C =< 16#FFFD;
            C >= 16#10000, C =< 16#EFFFF).

-define(NAME_CHAR(C),
            C >= 16#2D, C =< 16#2E; %% --.
            C >= 16#30, C =< 16#39; %% 0-9
            C =:= 16#3A; %% :
            C >= 16#41, C =< 16#5A; %% A-Z
            C =:= 16#5F; %% _
            C >= 16#61, C =< 16#7A; %% a-z
            C =:= 16#B7; %% ·
            C >= 16#C0, C =< 16#D6; %% À-Ö
            C >= 16#D8, C =< 16#F6; %% Ø-ö
            C >= 16#F8, C =< 16#2FF; %% ø-
            C >= 16#300, C =< 16#37D;
            C >= 16#37F, C =< 16#1FFF;
            C >= 16#200C, C =< 16#200D;
            C >= 16#203F, C =< 16#2040;
            C >= 16#2070, C =< 16#218F;
            C >= 16#2C00, C =< 16#2FEF;
            C >= 16#3001, C =< 16#D7FF;
            C >= 16#F900, C =< 16#FDCF;
            C >= 16#FDF0, C =< 16#FFFD;
            C >= 16#10000, C =< 16#EFFFF).

-define(CHAR(C),
        C >= 16#9, C =< 16#A;
        C =:= 16#D;
        C >= 16#20, C =< 16#D7FF;
        C >= 16#E000, C =< 16#FFFD;
        C >= 16#10000, C =< 16#10FFFF).

%% ===================================================================
%% Library functions.
%% ===================================================================

%%--------------------------------------------------------------------
%% Function: encode() -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec encode(xml()) -> iodata().
%%--------------------------------------------------------------------
encode(XML) -> encode(XML, []).

%%--------------------------------------------------------------------
%% Function: encode() -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec encode(xml(), []) -> iodata().
%%--------------------------------------------------------------------
encode(XML, _) -> do_encode(XML).

%%--------------------------------------------------------------------
%% Function: decode() -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec decode(binary()) ->
                    {ok, xml(), binary()} | {more, state()} | {eos, binary()}.
%%--------------------------------------------------------------------
decode(Binary) -> decode(Binary, #state{}).

%%--------------------------------------------------------------------
%% Function: decode() -> .
%% @doc
%%   
%% @end
%%--------------------------------------------------------------------
-spec decode(binary(), state() | {more, state()}) ->
                    {ok, xml(), binary()} | {more, state()} | {eos, binary()}.
%%--------------------------------------------------------------------
decode(<<>>, State = #state{}) -> more(decode, State);
decode(Binary, {more, State = #state{func = decode}}) -> decode(Binary, State);
decode(Binary, {more, State = #state{func = decode_start, args = Acc}}) ->
    decode_start(Binary, Acc, State);
decode(Binary, {more, State = #state{func = decode_cdata, args = Acc}}) ->
    decode_cdata(Binary, Acc, State);
decode(Binary, {more, State=#state{func=decode_cdata_end, args={Acc,Term}}}) ->
    decode_cdata_end(Binary, Acc, Term, State);
decode(Binary, {more, State = #state{func = decode_name, args = Acc}}) ->
    decode_name(Binary, Acc, State);
decode(Binary, {more, State = #state{func = decode_assign, args = Name}}) ->
    decode_assign(Binary, Name, State);
decode(Binary, {more, State = #state{func = decode_value_start, args=Name}}) ->
    decode_value_start(Binary, Name, State);
decode(Binary, {more, State=#state{func=decode_value, args={Acc,Del,Name}}}) ->
    decode_value(Binary, Acc, Del, Name, State);
decode(Binary, {more,State=#state{func=decode_value_escape}}) ->
    #state{args={Acc, AccValue, Del, Name}} = State,
    decode_value_escape(Binary, Acc, AccValue, Del, Name, State);
decode(Binary, {more, State = #state{func = decode_child, args = Acc}}) ->
    decode_child(Binary, Acc, State);
decode(Binary, {more, State = #state{func = decode_end_tag, args = Acc}}) ->
    decode_end_tag(Binary, Acc, State);
decode(Binary, {more, State = #state{func = decode_empty}}) ->
    decode_empty(Binary, State);
decode(Binary, {more, State=#state{func = decode_eos, args = Acc}}) ->
    decode_eos(Binary, Acc, State);
decode(Binary, {more, State=#state{func = decode_eos_end}}) ->
    decode_eos_end(Binary, State);
decode(Binary, {more, State = #state{func = decode_text, args = Acc}}) ->
    decode_text(Binary, Acc, State);
decode(Binary, {more, State = #state{func = decode_text_escape}}) ->
    #state{args = {Acc, TextAcc}} = State,
    decode_text_escape(Binary, Acc, TextAcc, State);
decode(Binary, {more, State=#state{func = decode_text_ws, args = {Acc, WS}}}) ->
    decode_text_ws(Binary, Acc, WS, State);
decode(<<$<, T/binary>>, State = #state{}) ->
    decode_start(T, <<>>, State);
decode(<<H, T/binary>>, State = #state{}) when ?WS(H) ->
    decode(T, State);
decode(<<H, T/binary>>, State = #state{}) ->
    decode_text(T, <<H>>, State).

%% ===================================================================
%% Internal functions.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

do_encode(Text) when is_binary(Text) -> escape(Text);
do_encode(#cdata{data = Data}) -> [<<"<![CDATA[">>, clean(Data), <<"]]>">>];
do_encode(#xml{tag = Tag, attrs = Attrs, children = []}) ->
    [<<"<">>, encode_tag(Tag), encode_attrs(Attrs), <<"/>">>];
do_encode(#xml{tag = Tag, attrs = Attrs, children = Children}) ->
    Tag1 = encode_tag(Tag),
    [<<"<">>, Tag1, encode_attrs(Attrs), <<">">>,
     [do_encode(Child) || Child <- Children],
     <<"</">>, Tag1, <<">">>].

encode_attrs([]) -> <<>>;
encode_attrs(Attrs) -> [encode_attr(Attr) || Attr <- Attrs].

encode_attr({Name, Value}) when is_atom(Name) ->
    encode_attr({atom_to_binary(Name, utf8), Value});
encode_attr({Name, Value}) ->
    [<<"\s">>, Name, <<"='">>, escape(Value), <<"'">>].

encode_tag(Tag) when is_atom(Tag) -> atom_to_binary(Tag, utf8);
encode_tag(Tag) -> Tag.

clean(Value) when is_binary(Value) ->
    case needs_cleaning(Value) of
        true -> clean(Value, <<>>);
        false -> Value
    end;
clean(Value) -> clean(iolist_to_binary(Value)).

needs_cleaning(<<>>) -> false;
needs_cleaning(<<H/utf8, T/binary>>) when ?CHAR(H) -> needs_cleaning(T);
needs_cleaning(_) -> true.

clean(<<>>, Acc) -> Acc;
clean(<<H/utf8, T/binary>>,Acc) when ?CHAR(H) -> clean(T,<<Acc/binary,H/utf8>>);
clean(<<_/utf8, T/binary>>, Acc) -> clean(T, Acc).

escape(Value) ->
    Clean = clean(Value),
    case needs_escaping(Clean) of
        true -> escape(Clean, <<>>);
        false -> Clean
    end.

needs_escaping(<<>>) -> false;
needs_escaping(<<$<, _/binary>>) -> true;
needs_escaping(<<$>, _/binary>>) -> true;
needs_escaping(<<$", _/binary>>) -> true;
needs_escaping(<<$', _/binary>>) -> true;
needs_escaping(<<$&, _/binary>>) -> true;
needs_escaping(<<_, T/binary>>) -> needs_escaping(T).

escape(<<>>, Acc) -> Acc;
escape(<<$<, T/binary>>, Acc) -> escape(T, <<Acc/binary, "&lt;">>);
escape(<<$>, T/binary>>, Acc) -> escape(T, <<Acc/binary, "&gt;">>);
escape(<<$", T/binary>>, Acc) -> escape(T, <<Acc/binary, "&quot;">>);
escape(<<$', T/binary>>, Acc) -> escape(T, <<Acc/binary, "&apos;">>);
escape(<<$&, T/binary>>, Acc) -> escape(T, <<Acc/binary, "&amp;">>);
escape(<<H, T/binary>>, Acc) -> escape(T, <<Acc/binary, H>>).

%% ===================================================================
%% Decoding
%% ===================================================================

decode_start(<<>>, Acc, State) -> more(decode_start, Acc, State);
decode_start(<<"![CDATA[", T/binary>>, <<>>, State) ->
    decode_cdata(T, <<>>, State);
decode_start(<<$/, T/binary>>, Acc, State) ->
    decode_empty(T, State#state{current = #xml{tag = Acc}});
decode_start(<<$>, T/binary>>, Acc, State) ->
    decode_child(T, <<>>, State#state{current = #xml{tag = Acc}});
decode_start(<<H, T/binary>>, Acc, State) when ?WS(H) ->
    decode_name(T, <<>>, State#state{current = #xml{tag = Acc}});
decode_start(<<H/utf8, T/binary>>, <<>>, State) when ?START_CHAR(H) ->
    decode_start(T, <<H/utf8>>, State);
decode_start(<<H/utf8, T/binary>>, Acc = <<_, _/binary>>, State)
  when ?NAME_CHAR(H) ->
    decode_start(T, <<Acc/binary, H/utf8>>, State).

decode_cdata(<<>>, Acc, State) -> more(decode_cdata, Acc, State);
decode_cdata(<<$], T/binary>>, Acc, State) ->
    decode_cdata_end(T, Acc, <<$]>>, State);
decode_cdata(<<H, T/binary>>, Acc, State) ->
    decode_cdata(T, <<Acc/binary, H>>, State).

decode_cdata_end(<<>>, Acc, Term, State) ->
    more(decode_cdata_end, {Acc, Term}, State);
decode_cdata_end(<<$], T/binary>>, Acc, <<$]>>, State) ->
    decode_cdata_end(T, Acc, <<"]]">>, State);
decode_cdata_end(<<$>, T/binary>>, Acc, <<"]]">>, State) ->
    pop(T, State#state{current = #cdata{data = Acc}});
decode_cdata_end(T, Acc, Term, State) ->
    decode_cdata(T, <<Acc/binary, Term/binary>>, State).

decode_name(<<>>, Acc, State) -> more(decode_name, Acc, State);
decode_name(<<$/, T/binary>>, <<>>, State) -> decode_empty(T, State);
decode_name(<<$>, T/binary>>, <<>>, State) -> decode_child(T, <<>>, State);
decode_name(<<$=, T/binary>>, Acc, State) -> decode_value_start(T, Acc, State);
decode_name(<<H, T/binary>>, <<>>, State) when ?WS(H) ->
    decode_name(T, <<>>, State);
decode_name(<<H, T/binary>>, Acc, State) when ?WS(H) ->
    decode_assign(T, Acc, State);
decode_name(<<H/utf8, T/binary>>, <<>>, State) when ?START_CHAR(H) ->
    decode_name(T, <<H/utf8>>, State);
decode_name(<<H/utf8, T/binary>>, Acc = <<_, _/binary>>, State)
  when ?NAME_CHAR(H) ->
    decode_name(T, <<Acc/binary, H/utf8>>, State).

decode_assign(<<>>, Name, State) -> more(decode_assign, Name, State);
decode_assign(<<$=, T/binary>>, Name, State) ->
    decode_value_start(T, Name, State);
decode_assign(<<H, T/binary>>, Name, State) when ?WS(H) ->
    decode_assign(T, Name, State).
    
decode_value_start(<<>>, Name, State) -> more(decode_value_start, Name, State);
decode_value_start(<<$', T/binary>>, Name, State) ->
    decode_value(T, <<>>, $', Name, State);
decode_value_start(<<$", T/binary>>, Name, State) ->
    decode_value(T, <<>>, $", Name, State);
decode_value_start(<<H, T/binary>>, Name, State) when ?WS(H) ->
    decode_value_start(T, Name, State).

decode_value(<<>>, Acc, Del, Name, State) ->
    more(decode_value, {Acc, Del, Name}, State);
decode_value(<<$&, T/binary>>, Acc, Del, Name, State) ->
    decode_value_escape(T, <<>>, Acc, Del, Name, State);
decode_value(<<Del, T/binary>>, Acc, Del, Name, State) ->
    #state{current = XML = #xml{attrs = Attrs}} = State,
    State1 = State#state{current = XML#xml{attrs = [{Name, Acc} | Attrs]}},
    decode_name(T, <<>>, State1);
decode_value(<<H, T/binary>>, Acc, Del, Name, State) ->
    decode_value(T, <<Acc/binary, H>>, Del, Name, State).

decode_value_escape(<<>>, Acc, AccValue, Del, Name, State) ->
    more(decode_value_escape, {Acc, AccValue, Del, Name}, State);
decode_value_escape(<<";", T/binary>>, Acc, AccValue, Del, Name, State) ->
    C = unescape(Acc),
    decode_value(T, <<AccValue/binary, C>>, Del, Name, State);
decode_value_escape(<<H, T/binary>>, Acc, AccValue, Del, Name, State) ->
    decode_value_escape(T, <<Acc/binary, H>>, AccValue, Del, Name, State).

decode_child(<<>>, Acc, State) -> more(decode_child, Acc, State);
decode_child(<<$<, T/binary>>, <<>>, State) ->
    decode_child(T, <<$<>>, State);
decode_child(<<$/, T/binary>>, <<$<>>, State) ->
    decode_end_tag(T, <<>>, State);
decode_child(<<H, T/binary>>, <<>>, State) when ?WS(H) ->
    decode_child(T, <<>>, State);
decode_child(T, Acc, State) ->
    decode(<<Acc/binary, T/binary>>, push(State)).

decode_end_tag(<<>>, Acc, State) -> more(decode_end_tag, Acc, State);
decode_end_tag(<<$>, T/binary>>, A, State = #state{current = #xml{tag = A}}) ->
    pop(T, reverse_children(State));
decode_end_tag(<<H, T/binary>>, Acc, State) when ?WS(H) ->
    decode_end(T, Acc, State);
decode_end_tag(<<H, T/binary>>, Acc, State) ->
    decode_end_tag(T, <<Acc/binary, H>>, State).

decode_end(<<$>, T/binary>>, A, State = #state{current = #xml{tag = A}}) ->
    pop(T, reverse_children(State));
decode_end(<<H, T/binary>>, Acc, State) when ?WS(H) ->
    decode_end(T, Acc, State).

decode_empty(<<>>, State) -> more(decode_empty, State);
decode_empty(<<$>, T/binary>>, State) -> pop(T, State);
decode_empty(Binary, State = #state{current = #xml{tag = <<>>}, stack = []}) ->
    decode_eos(Binary, <<>>, State).

decode_eos(<<>>, Acc, State) -> more(decode_eos, Acc, State);
decode_eos(<<$>, T/binary>>, <<"stream:stream">>, _) -> {eos, T};
decode_eos(<<H, T/binary>>, <<"stream:stream">>, State) when ?WS(H) ->
    decode_eos_end(T, State);
decode_eos(<<H, T/binary>>, Acc, State) ->
    decode_eos(T, <<Acc/binary, H>>, State).

decode_eos_end(<<>>, State) -> more(decode_eos_end, State);
decode_eos_end(<<$>, T/binary>>, _) -> {eos, T};
decode_eos_end(<<H, T/binary>>, State) when ?WS(H) -> decode_eos_end(T, State).

decode_text(<<>>, Acc, State) -> more(decode_text, Acc, State);
decode_text(<<H, T/binary>>, Acc, State) when ?WS(H) ->
    decode_text_ws(T, Acc, <<H>>, State);
decode_text(T = <<$<, _/binary>>, Acc, State) ->
    pop(T, State#state{current = Acc});
decode_text(<<$&, T/binary>>, Acc, State) ->
    decode_text_escape(T, <<>>, Acc, State);
decode_text(<<H, T/binary>>, Acc, State) ->
    decode_text(T, <<Acc/binary, H>>, State).

decode_text_escape(<<>>, Acc, TextValue, State) ->
    more(decode_text_escape, {Acc, TextValue}, State);
decode_text_escape(<<";", T/binary>>, Acc, TextValue, State) ->
    C = unescape(Acc),
    decode_text(T, <<TextValue/binary, C>>, State);
decode_text_escape(<<H, T/binary>>, Acc, TextValue, State) ->
    decode_text_escape(T, <<Acc/binary, H>>, TextValue, State).

decode_text_ws(<<>>, Acc, WS, State) -> more(decode_text_ws, {Acc, WS}, State);
decode_text_ws(<<H, T/binary>>, Acc, WS, State) when ?WS(H) ->
    decode_text_ws(T, Acc, <<WS/binary, H>>, State);
decode_text_ws(T = <<$<, _/binary>>, Acc, _, State) ->
    pop(T, State#state{current = Acc});
decode_text_ws(<<$&, T/binary>>, Acc, WS, State) ->
    decode_text_escape(T, <<>>, <<Acc/binary, WS/binary>>, State);
decode_text_ws(<<H, T/binary>>, Acc, WS, State) ->
    decode_text(T, <<Acc/binary, WS/binary, H>>, State).

more(Func, State) -> more(Func, no_args, State).

more(Func, Args, State) -> {more, State#state{func = Func, args = Args}}.

push(State = #state{current = C, stack = Stack}) ->
    State#state{current = undefined, stack = [C | Stack]}.

pop(Binary, #state{stack = [], current = C}) -> {ok, C, Binary};
pop(Binary, State) ->
    #state{current = C, stack = [H = #xml{children = Cs} | T]} = State,
    State1 = State#state{current = H#xml{children = [C | Cs]}, stack = T},
    decode_child(Binary, <<>>, State1).

reverse_children(State = #state{current = XML = #xml{children = Cs}}) ->
    State#state{current = XML#xml{children = lists:reverse(Cs)}}.

unescape(<<"lt">>) -> $<;
unescape(<<"gt">>) -> $>;
unescape(<<"quot">>) -> $"; %% "
unescape(<<"apos">>) -> $'; %% '
unescape(<<"amp">>) -> $&.
