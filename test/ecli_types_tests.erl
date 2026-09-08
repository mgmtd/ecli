-module(ecli_types_tests).

-include_lib("eunit/include/eunit.hrl").

enum_members() ->
    [{"1GbE", "1 Gigabit/s Ethernet"},
     {"10GbE", "10 Gigabit/s Ethernet"}].

parse_enum_name_desc_test() ->
    ?assertEqual({ok, "1GbE"},
                 ecli_types:parse({enum, enum_members()}, "1GbE")).

parse_enum_plain_name_test() ->
    ?assertEqual({ok, "auto"},
                 ecli_types:parse({enum, ["auto", "full"]}, "auto")).

parse_enum_map_member_test() ->
    Members = [#{name => "1GbE", desc => "1 Gigabit/s Ethernet", value => 1000}],
    ?assertEqual({ok, "1GbE"}, ecli_types:parse({enum, Members}, "1GbE")).

parse_enum_yang_alias_test() ->
    ?assertEqual({ok, "1GbE"},
                 ecli_types:parse({enumeration, enum_members()}, "1GbE")).

parse_enum_unknown_test() ->
    %% This is the crash from the example: `{enum, Members}' used to
    %% match `{Mod, Type}' and call enum:parse_value/2.
    ?assertEqual({error, "Unknown enum value"},
                 ecli_types:parse({enum, enum_members()}, "67")).

parse_union_constructor_not_module_test() ->
    ?assertEqual({error, "union types are not supported yet"},
                 ecli_types:parse({union, [string, uint8]}, "x")).

completions_enum_test() ->
    ?assertEqual([#{name => "1GbE", desc => "1 Gigabit/s Ethernet"},
                  #{name => "10GbE", desc => "10 Gigabit/s Ethernet"}],
                 ecli_types:completions({enum, enum_members()})).

completions_boolean_test() ->
    ?assertEqual([#{name => "true", desc => "True"},
                  #{name => "false", desc => "False"}],
                 ecli_types:completions(boolean)).

lookup_enum_value_test() ->
    Tree = ecli_test_schema:test_tree(),
    {ok, _Cmd, Items, []} = ecli:lookup("set host speed 1GbE", Tree, undefined),
    #{name := "speed", value := "1GbE"} = lists:last(Items).

lookup_enum_unknown_test() ->
    Tree = ecli_test_schema:test_tree(),
    ?assertEqual({error, "Unknown enum value"},
                 ecli:lookup("set host speed 67", Tree, undefined)).
