-module(ecli_expand_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ecli/include/ecli.hrl").

expand_menu_test() ->
    Tree = ecli_test_schema:test_tree(),
    Str = "sh",
    ?assertEqual({yes,"ow ",[]}, ecli_expand:expand(Str, Tree)).

expand_char1_test() ->
    Tree = ecli_test_schema:test_tree(),
    Str = "a",
    ?assertEqual({yes,"d",
                      ["\r\n",
                       [["  ",
                         ["admin"," "],
                         "Administer operational state","\r\n"],
                        ["  ",
                         ["add","   "],
                         "Add list configuration item","\r\n"]]]}, ecli_expand:expand(Str, Tree)).

expand_char2_test() ->
    Tree = ecli_test_schema:test_tree(),
    Str = "adm",
    ?assertEqual({yes,"in ",[]}, ecli_expand:expand(Str, Tree)).

expand_char3_test() ->
    Tree = ecli_test_schema:test_tree(),
    Str = "admin",
    ?assertEqual({yes," ",["\r\n",
                       [["  ",["peers"," "],"Administer peers","\r\n"]]]}, ecli_expand:expand(Str, Tree)).


expand_char4_test() ->
    Tree = ecli_test_schema:test_tree(),
    Str = "admin ",
    ?assertEqual({yes,"peers ",
                      ["\r\n",
                       [["  ",["add","     "],"Add a peer","\r\n"],
                        ["  ",["remove","  "],"Remove a peer","\r\n"],
                        ["  ",["block","   "],"Block a peer","\r\n"],
                        ["  ",["unblock"," "],"Unblock a peer","\r\n"]]]}, ecli_expand:expand(Str, Tree)).

expand_leaf_space_test() ->
    Tree = ecli_test_schema:test_tree(),
    Str = "admin peers add host ",
    ?assertEqual({yes,[],["\r\n","Hostname","\r\n"]}, ecli_expand:expand(Str, Tree)).

expand_leaf_test() ->
    Tree = ecli_test_schema:test_tree(),
    Str = "admin peers add host",
    ?assertEqual({yes," ",[]}, ecli_expand:expand(Str, Tree)).

expand_leaf_value_test() ->
    %% Expect an inserted space and to be prompted with the remaining leafs as a menu
    Tree = ecli_test_schema:test_tree(),
    Str = "admin peers add host 10.2.3.4",
    {yes," ",Menu} = ecli_expand:expand(Str, Tree),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(<<"\r\n  port    Port\r\n  pubkey  Remote node public key starting with pp_\r\n  trusted If the peer is trusted\r\n">>, MenuBin).

expand_leaf_value_space_test() ->
    %% Expect to be prompted with the remaining leafs as a menu
    Tree = ecli_test_schema:test_tree(),
    Str = "admin peers add host 10.2.3.4 ",
    {yes,"",Menu} = ecli_expand:expand(Str, Tree),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(<<"\r\n  port    Port\r\n  pubkey  Remote node public key starting with pp_\r\n  trusted If the peer is trusted\r\n">>, MenuBin).

expand_leaf_value2_space_test() ->
    %% Expect to be prompted with the remaining leafs as a menu
    Tree = ecli_test_schema:test_tree(),
    Str = "admin peers add host 10.2.3.4 po",
    ?assertEqual({yes,"rt ",[]}, ecli_expand:expand(Str, Tree)).

expand_enum_space_test() ->
    Tree = ecli_test_schema:test_tree(),
    {yes, "", Menu} = ecli_expand:expand("set host speed ", Tree),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(<<"\r\n  1GbE  1 Gigabit/s Ethernet\r\n  10GbE 10 Gigabit/s Ethernet\r\n">>,
                 MenuBin).

expand_enum_prefix_test() ->
    %% "1" is a prefix of both 1GbE and 10GbE; unique completion needs "1G".
    Tree = ecli_test_schema:test_tree(),
    ?assertEqual({yes, "bE ", []},
                 ecli_expand:expand("set host speed 1G", Tree)).

expand_pipe_menu_test() ->
    Tree = ecli_test_schema:test_tree(),
    {yes, " ", Menu} = ecli_expand:expand("show status |", Tree),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(true, binary:match(MenuBin, <<"display">>) =/= nomatch),
    ?assertEqual(true, binary:match(MenuBin, <<"match">>) =/= nomatch).

expand_pipe_trailing_space_does_not_add_space_test() ->
    Tree = ecli_test_schema:test_tree(),
    {yes, "", Menu} = ecli_expand:expand("show status | ", Tree),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(true, binary:match(MenuBin, <<"display">>) =/= nomatch),
    ?assertEqual(true, binary:match(MenuBin, <<"match">>) =/= nomatch).

expand_pipe_display_prefix_test() ->
    Tree = ecli_test_schema:test_tree(),
    ?assertEqual({yes, "splay ", []},
                 ecli_expand:expand("show status | di", Tree)).

expand_pipe_display_children_test() ->
    Tree = ecli_test_schema:test_tree(),
    {yes, "", Menu} = ecli_expand:expand("show status | display ", Tree),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(true, binary:match(MenuBin, <<"xml">>) =/= nomatch),
    ?assertEqual(true, binary:match(MenuBin, <<"json">>) =/= nomatch),
    ?assertEqual(true, binary:match(MenuBin, <<"defaults">>) =/= nomatch).

expand_compare_rollback_test() ->
    Tree = [#cmd{name = "show",
                 desc = "Show configuration",
                 action = fun(_, _, _) -> {ok, ""} end,
                 pipes = fun ecli_pipe:config_show_pipes/0}],
    {yes, "", Menu} = ecli_expand:expand("show | compare ", Tree),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(true, binary:match(MenuBin, <<"rollback">>) =/= nomatch).

expand_pipe_after_space_offers_pipe_test() ->
    Tree = ecli_test_schema:test_tree(),
    {yes, "", Menu} = ecli_expand:expand("show status ", Tree),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(true, binary:match(MenuBin, <<"|">>) =/= nomatch).

expand_set_does_not_offer_pipe_test() ->
    Tree = ecli_test_schema:test_tree(),
    Result = ecli_expand:expand("set host name x |", Tree),
    ?assertEqual(no, Result).

