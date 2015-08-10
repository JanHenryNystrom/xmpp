%%==============================================================================
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
%%%   eunit unit tests for the XMPP XML library module.
%%% @end
%%%
%% @author Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%% @copyright (C) 2014-2015, Jan Henry Nystrom <JanHenryNystrom@gmail.com>
%%%-------------------------------------------------------------------
-module(xmpp_xml_tests).
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

-define(STREAM_END_BIN, <<"</stream:stream>">>).

%% Escape

-define(ESCAPE_BIN,
        <<"<test attr='&quot;&apos;&lt;&amp;&gt;&apos;&quot;'>"
          "Ha &quot;&apos;&lt;&amp;&gt;&apos;&quot;"
          "</test>">>).

%% Presence
-define(PRESENCE_SUBSCRIBE_BIN,
        <<"<presence from='alice@wonderland.lit' "
                     "to='sister@realworld.lit' "
                     "type='subscribe'/>">>).

-define(PRESENCE_SUBSCRIBED_BIN,
        <<"<presence from='sister@realworld.lit' "
                     "to='alice@wonderland.lit' "
                     "type='subscribed'/>">>).

-define(PRESENCE_NOTIFICATION_BIN,
        <<"<presence from='alice@wonderland.lit/rabbithole' "
                    "to='sister@realworld.lit'/>">>).

-define(PRESENCE_PROBE_BIN,
        <<"<presence from='alice@wonderland.lit/rabbithole' "
                    "to='sister@realworld.lit' "
                    "type='probe'/>">>).

%%
%% "Delayed Delivery" XEP-0203
%%
-define(PRESENCE_DELAY_BIN,
        <<"<presence from='sister@realworld.lit/home' "
                    "to='alice@wonderland.lit/rabbithole' "
                    "type='unavailable'>"
            "<delay xmlns='urn:xmpp:delay' "
                   "stamp='2008-11-26T15:59:09Z'/>"
          "</presence>">>).

-define(PRESENCE_SHOW_BIN,
        <<"<presence from='alice@wonderland.lit/rabbithole' "
                    "to='sister@realworld.lit'>"
            "<show>xa</show>"
            "<status>down the rabbit hole!</status>"
          "</presence>">>).

%% ===================================================================
%% Tests.
%% ===================================================================

%% ===================================================================
%% Encoding
%% ===================================================================

%%--------------------------------------------------------------------
%% Escape
%%--------------------------------------------------------------------

encode_escape_test_() ->
    [{"Simple",
      ?_test(
         ?assertEqual(
            ?ESCAPE_BIN,
            iolist_to_binary(xmpp_xml:encode(?ESCAPE_XML))))}
    ].

%%--------------------------------------------------------------------
%% Presence
%%--------------------------------------------------------------------
encode_presence_test_() ->
    [{"Subscribe",
      ?_test(
         ?assertEqual(
            ?PRESENCE_SUBSCRIBE_BIN,
            iolist_to_binary(xmpp_xml:encode(?PRESENCE_SUBSCRIBE_XML))))},
     {"Subscribed",
      ?_test(
         ?assertEqual(
            ?PRESENCE_SUBSCRIBED_BIN,
            iolist_to_binary(xmpp_xml:encode(?PRESENCE_SUBSCRIBED_XML))))},
     {"Notification",
      ?_test(
         ?assertEqual(
            ?PRESENCE_NOTIFICATION_BIN,
            iolist_to_binary(xmpp_xml:encode(?PRESENCE_NOTIFICATION_XML))))},
     {"Probe",
      ?_test(
         ?assertEqual(
            ?PRESENCE_PROBE_BIN,
            iolist_to_binary(xmpp_xml:encode(?PRESENCE_PROBE_XML))))},
     {"Delay",
      ?_test(
         ?assertEqual(
            ?PRESENCE_DELAY_BIN,
            iolist_to_binary(xmpp_xml:encode(?PRESENCE_DELAY_XML))))},
     {"Show",
      ?_test(
         ?assertEqual(
            ?PRESENCE_SHOW_BIN,
            iolist_to_binary(xmpp_xml:encode(?PRESENCE_SHOW_XML))))}
    ].

%% ===================================================================
%% Decoding
%% ===================================================================

%%--------------------------------------------------------------------
%% Stream
%%--------------------------------------------------------------------
decode_stream_test_() ->
    [{"End",
      ?_test(equal_decode({?STREAM_END_XML, <<>>},
                          xmpp_xml:decode(?STREAM_END_BIN)))},
     {"End step",
      ?_test(equal_decode({?STREAM_END_XML, <<>>},
                          step(?STREAM_END_BIN)))}
    ].

%%--------------------------------------------------------------------
%% Escape
%%--------------------------------------------------------------------
decode_escape_test_() ->
    [{"Simple",
          ?_test(equal_decode({?ESCAPE_XML, <<>>},
                              xmpp_xml:decode(?ESCAPE_BIN)))},
     {"Simple step",
          ?_test(equal_decode({?ESCAPE_XML, <<>>}, step(?ESCAPE_BIN)))}
    ].

%%--------------------------------------------------------------------
%% Presence
%%--------------------------------------------------------------------
decode_presence_test_() ->
    [{"Subscribe",
      ?_test(equal_decode({?PRESENCE_SUBSCRIBE_XML, <<>>},
                          xmpp_xml:decode(?PRESENCE_SUBSCRIBE_BIN)))},
     {"Subscribe step",
      ?_test(equal_decode({?PRESENCE_SUBSCRIBE_XML, <<>>},
                          step(?PRESENCE_SUBSCRIBE_BIN)))},
     {"Subscribed",
      ?_test(equal_decode({?PRESENCE_SUBSCRIBED_XML, <<>>},
                          xmpp_xml:decode(?PRESENCE_SUBSCRIBED_BIN)))},
     {"Subscribed step",
      ?_test(equal_decode({?PRESENCE_SUBSCRIBED_XML, <<>>},
                          step(?PRESENCE_SUBSCRIBED_BIN)))},
     {"Notification",
      ?_test(equal_decode({?PRESENCE_NOTIFICATION_XML, <<>>},
                          xmpp_xml:decode(?PRESENCE_NOTIFICATION_BIN)))},
     {"Notification step",
      ?_test(equal_decode({?PRESENCE_NOTIFICATION_XML, <<>>},
                          step(?PRESENCE_NOTIFICATION_BIN)))},
     {"Probe",
      ?_test(equal_decode({?PRESENCE_PROBE_XML, <<>>},
                          xmpp_xml:decode(?PRESENCE_PROBE_BIN)))},
     {"Probe step",
      ?_test(equal_decode({?PRESENCE_PROBE_XML, <<>>},
                          step(?PRESENCE_PROBE_BIN)))},
     {"Delay",
      ?_test(equal_decode({?PRESENCE_DELAY_XML, <<>>},
                          xmpp_xml:decode(?PRESENCE_DELAY_BIN)))},
     {"Delay step",
      ?_test(equal_decode({?PRESENCE_DELAY_XML, <<>>},
                          step(?PRESENCE_DELAY_BIN)))},
     {"Show",
      ?_test(equal_decode({?PRESENCE_SHOW_XML, <<>>},
                          xmpp_xml:decode(?PRESENCE_SHOW_BIN)))},
     {"Show step",
      ?_test(equal_decode({?PRESENCE_SHOW_XML, <<>>},
                          step(?PRESENCE_SHOW_BIN)))}
    ].

%% ===================================================================
%% Common functions.
%% ===================================================================

equal_decode({eos, Bin}, {eos, Bin}) -> true;
equal_decode({XML1, Bin}, {ok, XML2, Bin}) -> equal(XML1, XML2).

equal(#xml{tag = Tag, attrs = Attrs1, children = Children1},
      #xml{tag = Tag, attrs = Attrs2, children = Children2}) ->
    equal_attrs(lists:sort(Attrs1), lists:sort(Attrs2)),
    equal_children(Children1, Children2);
equal(#xml{tag = Tag1}, #xml{tag = Tag2}) ->
    erlang:error({different_tags, Tag1, Tag2});
equal(Text, Text) when is_binary(Text) ->
    true.

equal_attrs([], []) -> true;
equal_attrs([], Attrs) -> erlang:error({missing_atributes, Attrs});
equal_attrs(Attrs, []) -> erlang:error({excessive_atributes, Attrs});
equal_attrs([H1 | T1], [H2 | T2]) ->
    equal_attr(H1, H2),
    equal_attrs(T1, T2).

equal_attr({N, V}, Attr) when is_atom(N) ->
    equal_attr({atom_to_binary(N, utf8), V}, Attr);
equal_attr(Attr, {N, V}) when is_atom(N) ->
    equal_attr(Attr, {atom_to_binary(N, utf8), V});
equal_attr({N, V}, {N, V}) ->
    true;
equal_attr({N, V1}, {N, V2}) ->
    erlang:error({attribute_values, V1, V2});
equal_attr({N1, V}, {N2, V}) ->
    erlang:error({missaligned_attribute, N1, N2});
equal_attr({N1, V1}, {N2, V2}) ->
    erlang:error({missaligned_attribute, {N1, V1}, {N2, V2}}).

equal_children([], []) -> true;
equal_children([], Children) -> erlang:error({missing_children, Children});
equal_children(Children, []) -> erlang:error({excessive_children, Children});
equal_children([H1 |T1], [H2 | T2]) ->
    equal(H1, H2),
    equal_children(T1, T2).

step(<<H, T/binary>>) ->
    S = {more, _} = xmpp_xml:decode(<<H>>),
    step(T, S).

step(<<H>>, S) -> xmpp_xml:decode(<<H>>, S);
step(<<H, T/binary>>, S) ->
    S1 = {more, _} = xmpp_xml:decode(<<H>>, S),
    step(T, S1).
