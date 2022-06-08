-module(ecli_test_schema).

-export([test_tree/0]).

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
       node_type => container,
       list_action => set,
       name => "set",
       desc => "Set configuration item",
       children => fun() -> configure_schema() end
      },
     #{role => cmd,
       node_type => container,
       list_action => add,  %% Special form to only add new list items (TODO)
       name => "add",
       desc => "Add list configuration item",
       children => fun() -> configure_schema() end
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

configure_schema() ->
    [#{role => schema,
       node_type => container,
       name => "host",
       desc => "Host details",
       children => fun() -> config_host_menu() end
     },
     #{role => schema,
       node_type => container,
       name => "Server",
       desc => "Server config",
       children => fun() -> config_servers_list_menu() end
     },
     #{role => schema,
       node_type => container,
       name => "client",
       desc => "Client config",
       children => fun() -> config_clients_list_menu() end
     }].

config_host_menu() ->
    [#{role => schema,
       node_type => leaf,
       name => "name",
       desc => "Name of host",
       type => string
     }].

%% Testing list item with multiple keys
config_clients_list_menu() ->
    [#{role => schema,
       node_type => list,
       name => "clients",
       desc => "List of clients",
       key_names => ["host", "port"],
        children => fun() -> config_clients_list_items() end
     }].

config_clients_list_items() ->
    [#{role => schema,
       node_type => leaf,
       name => "name",
       desc => "Client name",
       type => string
     },
    #{role => schema,
       node_type => leaf,
       name => "host",
       desc => "Remote node hostname or address",
       type => string
     },
     #{role => schema,
       node_type => leaf,
       name => "port",
       desc => "Port",
       type => uint16
     }].

%% Testing list item with single key
config_servers_list_menu() ->
    [#{role => schema,
       node_type => list,
       name => "servers",
       desc => "List of servers",
       key_names => ["name"],
        children => fun() -> config_servers_list_items() end
     }].

config_servers_list_items() ->
    [#{role => schema,
       node_type => leaf,
       name => "name",
       desc => "Client name",
       type => string
     },
    #{role => schema,
       node_type => leaf,
       name => "host",
       desc => "Local interface address",
       type => string
     },
     #{role => schema,
       node_type => leaf,
       name => "port",
       desc => "Port",
       type => uint16
     }].