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
          mode = operational
        }).

-record(menu_item,
        {
          node_type = leaf,
          node,
          desc,
          children,
          action
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
    {no, [], cli:format_menu(operational_menu(), getters()), J};
expand(Chars, #cli_juniper{mode = operational} = J) ->
    %% io:format("expand ~p~n",[Chars]),
    match_menu_item(Chars, operational_menu(), J);
expand([], #cli_juniper{mode = configuration} = J) ->
    {no, [], cli:format_menu(configuration_menu(), getters()), J};
expand(Chars, #cli_juniper{mode = configuration} = J) ->
    io:format("expand config ~p~n",[Chars]),
    match_menu_item(Chars, configuration_menu(), J).

execute("configure", #cli_juniper{mode = operational} = J) ->
    {ok, "", J#cli_juniper{mode = configuration}};
execute("exit", #cli_juniper{mode = configuration} = J) ->
    {ok, "", J#cli_juniper{mode = operational}};
execute(Other, #cli_juniper{} = J) ->
    io:format("Executed other ~p~n",[Other]),
    {ok, "", J}.



%%--------------------------------------------------------------------
%% Menu definitions
%%--------------------------------------------------------------------
operational_menu() ->
    [#menu_item{node_type = container,
                node = "show",
                desc = "Show commands",
                children = fun() -> operational_show_menu() end
               },
     #menu_item{node_type = leaf,
                node = "configure",
                desc = "Enter configuration mode",
                action = fun(J) -> enter_config_mode(J) end
               },
     #menu_item{node_type = leaf,
                node = "colose",
                desc = "Close session",
                action = fun(J) -> enter_config_mode(J) end
               }
    ].

operational_show_menu() ->
    [#menu_item{node_type = leaf,
                node = "status",
                desc = "Status summary",
                action = fun(J) -> show_status(J) end
               },
     #menu_item{node_type = leaf,
                node = "sockets",
                desc = "Open sockets",
                action = fun(J) -> show_status(J) end
               },
     #menu_item{node_type = leaf,
                node = "interface",
                desc = "Interface status",
                action = fun(J) -> show_interface_status(J) end
               }
     ].

configuration_menu() ->
    [#menu_item{node_type = container,
                node = "show",
                desc = "Show configuration",
                children = fun() -> configuration_tree() end
               },
     #menu_item{node_type = container,
                node = "set",
                desc = "Set a configuration parameter",
                action = fun(J) -> show_status(J) end
               },
     #menu_item{node_type = leaf,
                node = "exit",
                desc = "Exit configuration mode",
                action = fun(J) -> show_interface_status(J) end
               }
     ].

%%--------------------------------------------------------------------
%% Action implementations
%%--------------------------------------------------------------------
enter_config_mode(#cli_juniper{} = J) ->
    {ok, "", J#cli_juniper{mode = configuration}}.

show_status(#cli_juniper{} = J) ->
    {ok, "Status description\r\n", J}.

show_interface_status(#cli_juniper{} = J) ->
    {ok, "Interface statuses\r\n", J}.

configuration_tree() ->
    {switch_getters, cfg:getters(), example:cfg_schema()}.

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
    case cli:expand(Str, Menu, getters()) of
        no ->
            {no, [], [], J};
        {yes, Extra, MenuItems, Getters} ->
            {yes, Extra, cli:format_menu(MenuItems, Getters), J}
    end.

%% Set up the structure needed for the generic expander to know enough about our #menu_item{}s
getters() ->
    cli:getters(fun name/1, fun desc/1, fun children/1).

name(#menu_item{node = Name}) -> Name.

desc(#menu_item{desc = Desc}) -> Desc.

children(#menu_item{children = Cs}) -> Cs.


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
