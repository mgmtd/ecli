-module(ecli_pipe_tests).

-include_lib("eunit/include/eunit.hrl").

sample_tree() ->
    [{"status", {value, "ok"}},
     {"host", [{"name", {value, "box1"}},
               {"speed", {value, "1GbE"}}]}].

apply_default_curly_test() ->
    Text = pipe_bin(ecli_pipe:apply({data, sample_tree()}, [])),
    ?assertEqual(list_to_binary(ecli:format_simple_tree(sample_tree())),
                 Text).

apply_display_xml_test() ->
    Stage = [#{name => "display"},
             #{name => "xml", action => {pipe, {display, xml}}}],
    Xml = pipe_bin(ecli_pipe:apply({data, sample_tree()}, [Stage])),
    ?assertEqual(true, binary:match(Xml, <<"<config>">>) =/= nomatch),
    ?assertEqual(true, binary:match(Xml, <<"<status>ok</status>">>) =/= nomatch),
    ?assertEqual(true, binary:match(Xml, <<"<speed>1GbE</speed>">>) =/= nomatch).

apply_display_json_test() ->
    Stage = [#{name => "display"},
             #{name => "json", action => {pipe, {display, json}}}],
    Json = pipe_bin(ecli_pipe:apply({data, sample_tree()}, [Stage])),
    Decoded = decode_json(Json),
    true = is_map(Decoded),
    ?assertEqual(<<"ok">>, maps:get(<<"status">>, Decoded)),
    Host = maps:get(<<"host">>, Decoded),
    true = is_map(Host),
    ?assertEqual(<<"1GbE">>, maps:get(<<"speed">>, Host)).

apply_display_json_leaf_list_test() ->
    Tree = [{"name", {value, "box1"}},
            {"tags", {leaf_list, ["red", "green", "blue"]}}],
    Stage = [#{name => "display"},
             #{name => "json", action => {pipe, {display, json}}}],
    Json = pipe_bin(ecli_pipe:apply({data, Tree}, [Stage])),
    ?assertEqual(#{<<"name">> => <<"box1">>,
                   <<"tags">> => [<<"red">>, <<"green">>, <<"blue">>]},
                 decode_json(Json)).

apply_display_json_leaf_list_binaries_test() ->
    Tree = [{"tags", {leaf_list, [<<"red">>, <<"green">>]}}],
    Stage = [#{name => "display"},
             #{name => "json", action => {pipe, {display, json}}}],
    Json = pipe_bin(ecli_pipe:apply({data, Tree}, [Stage])),
    ?assertEqual(#{<<"tags">> => [<<"red">>, <<"green">>]}, decode_json(Json)).

apply_display_json_string_leaf_not_array_test() ->
    Tree = [{"name", {value, "box1"}}],
    Stage = [#{name => "display"},
             #{name => "json", action => {pipe, {display, json}}}],
    Json = pipe_bin(ecli_pipe:apply({data, Tree}, [Stage])),
    ?assertEqual(#{<<"name">> => <<"box1">>}, decode_json(Json)).

apply_curly_leaf_list_test() ->
    Tree = [{"tags", {leaf_list, ["red", "green"]}}],
    Text = iolist_to_binary(ecli:format_simple_tree(Tree)),
    ?assertEqual(<<"tags [ red green ];\r\n">>, Text).

apply_xml_leaf_list_test() ->
    Tree = [{"tags", {leaf_list, ["red", "green"]}}],
    Stage = [#{name => "display"},
             #{name => "xml", action => {pipe, {display, xml}}}],
    Xml = pipe_bin(ecli_pipe:apply({data, Tree}, [Stage])),
    ?assertEqual(true, binary:match(Xml, <<"<tags>red</tags>">>) =/= nomatch),
    ?assertEqual(true, binary:match(Xml, <<"<tags>green</tags>">>) =/= nomatch).

apply_set_leaf_list_test() ->
    Tree = [{"tags", {leaf_list, ["red", "green"]}}],
    Stage = [#{name => "display"},
             #{name => "set", action => {pipe, {display, set}}}],
    Set = pipe_bin(ecli_pipe:apply({data, Tree}, [Stage])),
    ?assertEqual(<<"set tags red\r\nset tags green\r\n">>, Set).

apply_display_defaults_test() ->
    Stage = [#{name => "display"},
             #{name => "defaults", action => {pipe, {display, defaults}}}],
    Text = pipe_bin(ecli_pipe:apply({data, sample_tree()}, [Stage])),
    ?assertEqual(list_to_binary(ecli:format_simple_tree(sample_tree())), Text),
    ?assertEqual(true, ecli_pipe:wants_defaults([Stage])),
    ?assertEqual(false, ecli_pipe:wants_defaults([])).

apply_compare_passthrough_test() ->
    Stage = [#{name => "compare", action => {pipe, compare}}],
    Text = "[edit]\r\n+  host {\r\n+    name box1;\r\n+  }\r\n",
    ?assertEqual(true, ecli_pipe:wants_compare([Stage])),
    ?assertEqual(false, ecli_pipe:wants_compare([])),
    ?assertEqual(session, ecli_pipe:compare_against([Stage])),
    ?assertEqual(false, ecli_pipe:compare_against([])),
    Out = pipe_bin(ecli_pipe:apply(Text, [Stage])),
    ?assertEqual(list_to_binary(Text), Out).

compare_against_rollback_leaf_test() ->
    Stage = [#{name => "compare", action => {pipe, compare}},
             #{name => "rollback", action => {pipe, {compare, rollback}},
               value => 1, type => integer}],
    ?assertEqual(true, ecli_pipe:wants_compare([Stage])),
    ?assertEqual({rollback, 1}, ecli_pipe:compare_against([Stage])).

compare_against_rollback_index_cmd_test() ->
    Stage = [#{name => "compare", action => {pipe, compare}},
             #{name => "rollback"},
             #{name => "1", action => {pipe, {compare, {rollback, 1}}}}],
    ?assertEqual({rollback, 1}, ecli_pipe:compare_against([Stage])).

set_pipes_catalog_test() ->
    Names = [maps:get(name, C) || C <- ecli_pipe:catalog(fun ecli_pipe:set_pipes/0)],
    ?assertEqual(["first", "last", "before", "after"], Names).

insert_where_first_test() ->
    Stage = [#{name => "first", action => {pipe, {insert, first}}}],
    ?assertEqual(first, ecli_pipe:insert_where([Stage])),
    ?assertEqual(undefined, ecli_pipe:insert_where([])).

insert_where_after_value_test() ->
    Stage = [#{name => "after", action => {pipe, {insert, 'after'}},
               value => "deny", type => string}],
    ?assertEqual({'after', {"deny"}}, ecli_pipe:insert_where([Stage])).

insert_where_before_value_test() ->
    Stage = [#{name => "before", action => {pipe, {insert, before}},
               value => "allow"}],
    ?assertEqual({before, {"allow"}}, ecli_pipe:insert_where([Stage])).

config_show_pipes_include_compare_test() ->
    Config = [maps:get(name, C) || C <- ecli_pipe:catalog(fun ecli_pipe:config_show_pipes/0)],
    Oper = [maps:get(name, C) || C <- ecli_pipe:catalog(fun ecli_pipe:show_pipes/0)],
    ?assertEqual(true, lists:member("compare", Config)),
    ?assertEqual(false, lists:member("compare", Oper)),
    Compare = lists:keyfind("compare", 1,
                            [{maps:get(name, C), C}
                             || C <- ecli_pipe:catalog(fun ecli_pipe:config_show_pipes/0)]),
    {_, #{children := KidsFun}} = Compare,
    KidNames = [maps:get(name, K) || K <- ecli_pipe:catalog(KidsFun)],
    ?assertEqual(true, lists:member("rollback", KidNames)).

apply_display_set_test() ->
    Stage = [#{name => "display"},
             #{name => "set", action => {pipe, {display, set}}}],
    Set = pipe_bin(ecli_pipe:apply({data, sample_tree()}, [Stage])),
    ?assertEqual(true, binary:match(Set, <<"set status ok">>) =/= nomatch),
    ?assertEqual(true, binary:match(Set, <<"set host speed 1GbE">>) =/= nomatch).

apply_match_count_test() ->
    Text = "alpha\r\nbeta\r\nalpha2\r\n",
    Match = [#{name => "match", action => {pipe, match}, value => "alpha"}],
    Count = [#{name => "count", action => {pipe, count}}],
    Out = pipe_bin(ecli_pipe:apply(Text, [Match, Count])),
    ?assertEqual(<<"Count: 2 lines\r\n">>, Out).

apply_except_test() ->
    Text = "keep\r\ndrop me\r\nkeep too\r\n",
    Except = [#{name => "except", action => {pipe, except}, value => "drop"}],
    Out = pipe_bin(ecli_pipe:apply(Text, [Except])),
    ?assertEqual(<<"keep\r\nkeep too\r\n">>, Out).

apply_display_on_text_errors_test() ->
    Stage = [#{name => "display"},
             #{name => "xml", action => {pipe, {display, xml}}}],
    ?assertEqual({error, "Command does not support this display format"},
                 ecli_pipe:apply("already text\r\n", [Stage])).

apply_invalid_regex_test() ->
    Match = [#{name => "match", action => {pipe, match}, value => "["}],
    ?assertEqual({error, "Invalid regular expression"},
                 ecli_pipe:apply("foo\r\n", [Match])).

run_show_display_xml_test() ->
    Tree = ecli_test_schema:test_tree(),
    {ok, Out, _} = ecli:run("show status | display xml", Tree, undefined, #{}),
    Bin = iolist_to_binary(Out),
    ?assertEqual(true, binary:match(Bin, <<"<status>ok</status>">>) =/= nomatch).

run_show_default_curly_test() ->
    Tree = ecli_test_schema:test_tree(),
    {ok, Out, _} = ecli:run("show status", Tree, undefined, #{}),
    ?assertEqual(list_to_binary(ecli:format_simple_tree(ecli_test_schema_tree())),
                 iolist_to_binary(Out)).

ecli_test_schema_tree() ->
    [{"status", {value, "ok"}},
     {"host", [{"name", {value, "box1"}},
               {"speed", {value, "1GbE"}}]}].

run_match_then_display_still_xml_test() ->
    %% display is a renderer choice, not a stream filter: both orders XML-then-match.
    Tree = ecli_test_schema:test_tree(),
    {ok, Out, _} = ecli:run("show status | match speed | display xml", Tree, undefined, #{}),
    Bin = iolist_to_binary(Out),
    ?assertEqual(true, binary:match(Bin, <<"<speed>1GbE</speed>">>) =/= nomatch),
    ?assertEqual(nomatch, binary:match(Bin, <<"<status>">>)).

-spec pipe_bin(iodata() | {error, string()}) -> binary().
pipe_bin({error, Reason}) ->
    error({pipe_error, Reason});
pipe_bin(Text) ->
    iolist_to_binary(Text).

%% OTP 24 has no json:decode/1. Enough of JSON for these fixtures.
decode_json(Bin) when is_binary(Bin) ->
    {Val, Rest} = json_value(json_skip(unicode:characters_to_list(Bin))),
    [] = json_skip(Rest),
    Val.

json_skip([C | T]) when C =< 32 ->
    json_skip(T);
json_skip(S) ->
    S.

json_value([${ | T]) ->
    json_object(json_skip(T), #{});
json_value([$[ | T]) ->
    json_array(json_skip(T), []);
json_value([$" | T]) ->
    json_string(T, []);
json_value([$t, $r, $u, $e | T]) ->
    {true, T};
json_value([$f, $a, $l, $s, $e | T]) ->
    {false, T};
json_value([$n, $u, $l, $l | T]) ->
    {null, T};
json_value([C | _] = T) when C =:= $-; C >= $0, C =< $9 ->
    json_number(T).

json_object([$} | T], Acc) ->
    {Acc, T};
json_object([$" | T], Acc) ->
    {Key, Rest0} = json_string(T, []),
    [$: | Rest1] = json_skip(Rest0),
    {Val, Rest2} = json_value(json_skip(Rest1)),
    case json_skip(Rest2) of
        [$, | Rest3] ->
            json_object(json_skip(Rest3), Acc#{Key => Val});
        [$} | Rest3] ->
            {Acc#{Key => Val}, Rest3}
    end.

json_array([$] | T], Acc) ->
    {lists:reverse(Acc), T};
json_array(T, Acc) ->
    {Val, Rest0} = json_value(T),
    case json_skip(Rest0) of
        [$, | Rest1] ->
            json_array(json_skip(Rest1), [Val | Acc]);
        [$] | Rest1] ->
            {lists:reverse([Val | Acc]), Rest1}
    end.

json_string([$" | T], Acc) ->
    {unicode:characters_to_binary(lists:reverse(Acc)), T};
json_string([$\\, $" | T], Acc) ->
    json_string(T, [$" | Acc]);
json_string([$\\, $\\ | T], Acc) ->
    json_string(T, [$\\ | Acc]);
json_string([$\\, $n | T], Acc) ->
    json_string(T, [$\n | Acc]);
json_string([$\\, $r | T], Acc) ->
    json_string(T, [$\r | Acc]);
json_string([$\\, $t | T], Acc) ->
    json_string(T, [$\t | Acc]);
json_string([C | T], Acc) ->
    json_string(T, [C | Acc]).

json_number(T) ->
    {Raw, Rest} = json_number_chars(T, []),
    case lists:member($., Raw) orelse lists:member($e, Raw)
         orelse lists:member($E, Raw) of
        true ->
            {list_to_float(floatish(Raw)), Rest};
        false ->
            {list_to_integer(Raw), Rest}
    end.

json_number_chars([C | T], Acc) when C >= $0, C =< $9; C =:= $-; C =:= $+;
                                     C =:= $.; C =:= $e; C =:= $E ->
    json_number_chars(T, [C | Acc]);
json_number_chars(T, Acc) ->
    {lists:reverse(Acc), T}.

floatish(Raw) ->
    case lists:member($., Raw) of
        true ->
            Raw;
        false ->
            Raw ++ ".0"
    end.
