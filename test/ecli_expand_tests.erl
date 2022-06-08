-module(ecli_expand_tests).

-include_lib("eunit/include/eunit.hrl").

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

