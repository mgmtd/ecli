-module(ecli_expand_tests).

-include_lib("eunit/include/eunit.hrl").

expand_menu_test() ->
    Tree = test_tree(),
    Str = "sh",
    ?assertEqual({yes,"ow ",[]}, ecli_expand:expand(Str, Tree)).

expand_char1_test() ->
    Tree = test_tree(),
    Str = "a",
    ?assertEqual({yes,"dmin ",[]}, ecli_expand:expand(Str, Tree)).

expand_char2_test() ->
    Tree = test_tree(),
    Str = "ad",
    ?assertEqual({yes,"min ",[]}, ecli_expand:expand(Str, Tree)).

expand_char3_test() ->
    Tree = test_tree(),
    Str = "admin",
    ?assertEqual({yes," ",["\r\n",
                       [["  ",["peers"," "],"Administer peers","\r\n"]]]}, ecli_expand:expand(Str, Tree)).


expand_char4_test() ->
    Tree = test_tree(),
    Str = "admin ",
    ?assertEqual({yes,"peers ",
                      ["\r\n",
                       [["  ",["add","     "],"Add a peer","\r\n"],
                        ["  ",["remove","  "],"Remove a peer","\r\n"],
                        ["  ",["block","   "],"Block a peer","\r\n"],
                        ["  ",["unblock"," "],"Unblock a peer","\r\n"]]]}, ecli_expand:expand(Str, Tree)).

expand_leaf_space_test() ->
    Tree = test_tree(),
    Str = "admin peers add host ",
    ?assertEqual(no, ecli_expand:expand(Str, Tree)).

expand_leaf_test() ->
    Tree = test_tree(),
    Str = "admin peers add host",
    ?assertEqual({yes," ",[]}, ecli_expand:expand(Str, Tree)).

expand_leaf_value_test() ->
    %% Expect an inserted space and to be prompted with the remaining leafs as a menu
    Tree = test_tree(),
    Str = "admin peers add host 10.2.3.4",
    {yes," ",Menu} = ecli_expand:expand(Str, Tree),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(<<"\r\n  port    Port\r\n  pubkey  Remote node public key starting with pp_\r\n  trusted If the peer is trusted\r\n">>, MenuBin).

expand_leaf_value_space_test() ->
    %% Expect to be prompted with the remaining leafs as a menu
    Tree = test_tree(),
    Str = "admin peers add host 10.2.3.4 ",
    {yes,"",Menu} = ecli_expand:expand(Str, Tree),
    MenuBin = list_to_binary(Menu),
    ?assertEqual(<<"\r\n  port    Port\r\n  pubkey  Remote node public key starting with pp_\r\n  trusted If the peer is trusted\r\n">>, MenuBin).

expand_leaf_value2_space_test() ->
    %% Expect to be prompted with the remaining leafs as a menu
    Tree = test_tree(),
    Str = "admin peers add host 10.2.3.4 po",
    ?assertEqual({yes,"rt ",[]}, ecli_expand:expand(Str, Tree)).

test_tree() ->
    [#{role => cmd,
       node_type => container,
       name => "show",
       desc => "Show commands",
       children => fun() -> operational_show_menu() end
      },
     #{role => cmd,
       node_type => container,
       name => "admin",
       desc => "Administer operational state",
       children => fun() -> operational_admin_menu() end
      },
     #{role => cmd,
       node_type => leaf,
       name => "exit",
       desc => "Close session",
       action => fun(_J1, _, _) -> stop end
      }
    ].

operational_show_menu() ->
    [#{role => cmd,
       node_type => container,
       name => "status",
       desc => "Status summary"
      },
     #{role => cmd,
       node_type => container,
       name => "peers",
       desc => "Show peers"
      }
    ].

operational_admin_menu() ->
    [#{role => cmd,
       node_type => container,
       name => "peers",
       desc => "Administer peers",
       children => fun() -> operational_admin_peers_menu() end
      }
    ].

operational_admin_peers_menu() ->
    [#{role => cmd,
       node_type => container,
       name => "add",
       desc => "Add a peer",
       children => fun() -> peer_schema() end
      },
     #{role => cmd,
       node_type => container,
       name => "remove",
       desc => "Remove a peer"
      },
     #{role => cmd,
       node_type => container,
       name => "block",
       desc => "Block a peer"
      },
     #{role => cmd,
       node_type => container,
       name => "unblock",
       desc => "Unblock a peer"
      }
    ].

peer_schema() ->
    [#{role => schema,
       node_type => leaf,
       name => "host",
       desc => "Hostname",
       type => string
     },
     #{role => schema,
       node_type => leaf,
       name => "port",
       desc => "Port",
       type => integer
     },
     #{role => schema,
       node_type => leaf,
       name => "pubkey",
       desc => "Remote node public key starting with pp_",
       type => string
     },
     #{role => schema,
       node_type => leaf,
       name => "trusted",
       desc => "If the peer is trusted",
       type => boolean
     }].