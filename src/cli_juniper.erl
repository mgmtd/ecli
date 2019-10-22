%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Implementation of cli callbacks emulating a juniper like CLI
%%%
%%% @end
%%% Created : 31 Aug 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cli_juniper).

-export([init/0,
         banner/1,
         prompt/1,
         expand/2,
         execute/2
        ]).

-record(cli_juniper,
        {
         mode = operational,
         user_txn             % Transaction store for command sequences that need one
        }).

%%--------------------------------------------------------------------
%% CLI behaviour mandatory callbacks
%%--------------------------------------------------------------------
init() ->
    {ok, #cli_juniper{}}.

banner(#cli_juniper{}) ->
    {ok, "\r\nWelcome to the Juniper style CLI\r\n
Hit TAB, SPC or ? at any time to see available options\r\n\r\n"}.

prompt(#cli_juniper{mode = Mode}) ->
    Suffix = case Mode of
                 operational ->
                     "> ";
                 configuration ->
                     "# "
             end,
    case inet:gethostname() of
        {ok, Hostname} ->
            {ok, Hostname ++  Suffix};
        _ ->
            {ok, Suffix}
    end.


expand([], #cli_juniper{mode = operational} = J) ->
    {no, [], cli:format_menu(operational_menu()), J};
expand(Chars, #cli_juniper{mode = operational} = J) ->
    %% io:format("expand ~p~n",[Chars]),
    match_menu_item(Chars, operational_menu(), J);
expand([], #cli_juniper{mode = configuration} = J) ->
    {no, [], cli:format_menu(configuration_menu()), J};
expand(Chars, #cli_juniper{mode = configuration} = J) ->
    io:format("expand config ~p~n",[Chars]),
    match_menu_item(Chars, configuration_menu(), J).

execute(CmdStr, #cli_juniper{mode = operational} = J) ->
    io:format("Executing operational Command ~p~n",[CmdStr]),
    execute_menu_item(CmdStr, operational_menu(), J);
execute(CmdStr, #cli_juniper{mode = configuration} = J) ->
    io:format("Executing configuration Command ~p~n",[CmdStr]),
    execute_menu_item(CmdStr, configuration_menu(), J).



%%--------------------------------------------------------------------
%% Menu definitions
%%--------------------------------------------------------------------
operational_menu() ->
    [#{node_type => container,
       node => "show",
       desc => "Show commands",
       action => fun(J, Item, _Value) ->
                         show_operational(J, Item)
                 end,
       children => fun() -> operational_show_menu() end
      },
     #{node_type => leaf,
       node => "configure",
       desc => "Enter configuration mode",
       action => fun(J, _, _) -> enter_config_mode(J) end
      },
     #{node_type => leaf,
       node => "colose",
       desc => "Close session",
       action => fun(J) -> enter_config_mode(J) end
      }
    ].

operational_show_menu() ->
    [#{node_type => leaf,
       node => "status",
       desc => "Status summary",
       action => fun(J, Item, _) -> show_status(J, Item) end
      },
     #{node_type => leaf,
       node => "sockets",
       desc => "Open sockets",
       action => fun(J, Item, _) -> show_status(J, Item) end
      },
     #{node_type => leaf,
       node => "interface",
       desc => "Interface status",
       action => fun(J, Item, _) -> show_interface_status(J, Item) end
      }
    ].

configuration_menu() ->
    [#{node_type => container,
       node => "show",
       desc => "Show configuration",
       children => fun() -> configuration_tree() end
      },
     #{node_type => container,
       node => "set",
       desc => "Set a configuration parameter",
       action => fun(Txn, Path, Value) -> cfg_set(Txn, Path, Value) end
      },
     #{node_type => leaf,
       node => "exit",
       desc => "Exit configuration mode",
       action => fun(J, _, _) -> exit_config_mode(J) end
      }
    ].

%%--------------------------------------------------------------------
%% Action implementations
%%--------------------------------------------------------------------
enter_config_mode(#cli_juniper{} = J) ->
    Txn = transaction_id,
    {ok, "", J#cli_juniper{mode = configuration, user_txn = Txn}}.

exit_config_mode(#cli_juniper{user_txn = _Txn} = J) ->
    {ok, "", J#cli_juniper{mode = operational, user_txn = undefined}}.

show_status(#cli_juniper{} = J, _Item) ->
    {ok, "Status description\r\n", J}.

show_interface_status(#cli_juniper{} = J, _Item) ->
    {ok, "Interface statuses\r\n", J}.

show_operational(#cli_juniper{user_txn = _Txn}, Item) ->
    io:format("Executing show operational ~p~n",[Item]),
    {ok, "Operational statuses\r\n"}.

configuration_tree() ->
    [].


cfg_set(_Txn, _Path, _Value) ->
    {ok, "Set OK\r\n"}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

%% Given a string from the user and a tree of menu items match the
%% command against the tree. Several outcomes:
%%
%% 1. The string matches the prefix of a single node - Fill the
%%    remaining part of the menu item. With a space at the end if the
%%    node is a container, not if it is a leaf
%%
%% 2. The string fully matches a single container - Prompt with the
%%    next level of menu items
%%
%% 3. The String fully matches a single leaf - nothing to do
%%
%% 4. The string matches nothing - do nothing
%%
%% 5. The string matches several possible items - complete as far as
%%    we can and prompt the user with the possible matches

match_menu_item(Str, Menu, J) ->
    %% io:format("match_menu_item ~p~n",[Str]),
    %% Use the library function provided in cli to take care of the expansion
    case cli:expand(Str, Menu) of
        no ->
            {no, [], [], J};
        {yes, Extra, MenuItems} ->
            {yes, Extra, MenuItems, J}
    end.

execute_menu_item(CmdStr, Menu, J) ->
    case cli:lookup(CmdStr, Menu) of
        {error, Reason} ->
            {ok, Reason, J};
        {ok, Cmd, Path, Value} ->
            ActionNode = tl(Cmd),
            #{action := Action} = ActionNode,
            case catch Action(J, Path, Value) of
                {'EXIT', Reason} ->
                    io:format("Executing configuration exit ~p~n",[Reason]),
                    {ok, "Error executing command", J};
                {ok, Result} ->
                    {ok, Result, J};
                {ok, Result, #cli_juniper{} = J1} ->
                    {ok, Result, J1};
                {ok, Result, UserTxn} ->
                    {ok, Result, J#cli_juniper{user_txn = UserTxn}}
            end
    end.

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

full_expansion_top_level_test_() ->
    {ok, J} = init(),
    Result = expand("s", J),
    ?_assertMatch({yes, "how ", [], #cli_juniper{mode = operational}}, Result).

full_expansion_multi_chars_top_level_test_() ->
    {ok, J} = init(),
    Result = expand("sh", J),
    ?_assertMatch({yes, "ow ", [], #cli_juniper{mode = operational}}, Result).

no_match_top_level_test_() ->
    {ok, J} = init(),
    Result = expand("x", J),
    ?_assertMatch( {no, "", [], #cli_juniper{mode = operational}}, Result).

partial_match_multiple_test_() ->
    {ok, J} = init(),
    Result = expand("c", J),
    ?_assertMatch({yes, "o", ["\r\n", [_,_]], #cli_juniper{mode = operational}}, Result).

add_space_top_level_test_() ->
    {ok, J} = init(),
    Result = expand("show", J),
    ?_assertMatch({yes, " ",  ["\r\n", [_,_,_]], #cli_juniper{mode = operational}}, Result).


-endif.
