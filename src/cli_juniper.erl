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
         expand/2
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

prompt(#cli_juniper{mode = operational}) ->
    case inet:gethostname() of
        {ok, Hostname} ->
            {ok, Hostname ++  "> "};
        _ ->
            {ok, "> "}
    end.


expand([], #cli_juniper{mode = operational} = J) ->
    {no, [], format_menu(operational_menu()), J};
expand(Chars, #cli_juniper{mode = operational} = J) ->
    io:format("expand ~p~n",[Chars]),
    match_menu_item(Chars, operational_menu(), J).

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

%%--------------------------------------------------------------------
%% Action implementations
%%--------------------------------------------------------------------
enter_config_mode(#cli_juniper{} = J) ->
    {ok, "", J#cli_juniper{mode = configuration}}.

show_status(#cli_juniper{} = J) ->
    {ok, "Status description\r\n", J}.

show_interface_status(#cli_juniper{} = J) ->
    {ok, "Interface statuses\r\n", J}.


%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
format_menu([]) -> "";
format_menu(Items) ->
    MaxCmdLen = max_cmd_len(Items),
    Menu = lists:map(fun(#menu_item{node = Cmd, desc = Desc}) ->
                             [pad(Cmd, MaxCmdLen + 1), Desc, "\r\n"]
                     end, Items),
    ["\r\n", Menu].

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
    io:format("match_menu_item ~p~n",[Str]),
    case parse_cmd(Str, Menu) of
        no ->
            {no, [], [], J};
        {yes, Extra, MenuItems} ->
            {yes, Extra, format_menu(MenuItems), J}
    end.

menu_item_children(#menu_item{children = Fn}) when is_function(Fn) -> Fn();
menu_item_children(L) when is_list(L) -> L;
menu_item_children(_) -> [].


parse_cmd(Str, MenuItems) ->
    io:format("parse_cmd ~p~n",[Str]),
    parse_cmd(Str, MenuItems, [], start).

parse_cmd([$\s|Cs], MenuItems, MatchedChars, start) ->
    parse_cmd(Cs, MenuItems, MatchedChars, start);
parse_cmd([$\t|Cs], MenuItems, MatchedChars, start) ->
    parse_cmd(Cs, MenuItems, MatchedChars, start);
parse_cmd([], MenuItems, _MatchedChars, start) ->
    io:format("parse_cmd start end ~p~n",[MenuItems]),
    case MenuItems of
        [] ->
            no;
        [#menu_item{}] ->
            {yes, "", MenuItems};
        _ ->
            {yes, "", MenuItems}
    end;
parse_cmd(Str, MenuItems, MatchedChars, start) ->
    io:format("parse_cmd change state ~p~n",[cmd]),
    parse_cmd(Str, MenuItems, MatchedChars, cmd);
parse_cmd([$\s|Cs], [#menu_item{node = Node} = Item], MatchedChars, cmd) ->
    case MatchedChars of
        Node ->
            %% Matched a full level in the tree with a following space. Carry on to children
            io:format("parse_cmd matched full level ~p~p~n",[Node,Item]),
            parse_cmd(Cs, menu_item_children(Item), [], start);
        _ ->
            no
    end;
parse_cmd([C|Cs], MenuItems, Matched, cmd) ->
    io:format("parse_cmd normal chars ~p matched = ~p~n",[C, Matched]),
    SoFar = Matched ++ [C],
    Matches = match_cmds(SoFar, MenuItems),
    case Matches of
        [] ->
            no;
        Items ->
            parse_cmd(Cs, Items, SoFar, cmd)
    end;
parse_cmd([], [], _, _) ->
    no;
parse_cmd([], MenuItems, MatchedStr, cmd) ->
    %% Reached the end of the input without getting to the end of a
    %% cmd with following space
    io:format("parse_cmd end ~p matched = ~p~n",[MenuItems, MatchedStr]),
    case MenuItems of
        [#menu_item{node = Node} = Item] when Node == MatchedStr ->
            {yes, " ", menu_item_children(Item)};
        [#menu_item{node = Node}] ->
            {yes, chars_to_expand(MatchedStr, Node), []};
        MenuItems ->
            %% Still a number of matching menu items. auto fill up to the point they diverge
            Chars = expand_menus(MatchedStr, MenuItems),
            {yes, Chars, MenuItems}
    end.


match_cmds(_Str, []) ->
    [];
match_cmds(Str, Menu) ->
    lists:filter(fun(#menu_item{node = Item}) ->
                         lists:prefix(Str, Item)
                 end, Menu).

chars_to_expand(Str, Match) ->
    lists:sublist(Match, length(Str) + 1, length(Match) - 1) ++ " ".

%% Find characters to add to fill up to where the node names diverge
%% e.g. names configure and contain given an input of "c" should return "on"
expand_menus(Str, Menus) ->
    StrLen = length(Str),
    Suffixes = lists:map(fun(#menu_item{node = Node}) ->
                                        lists:nthtail(StrLen, Node)
                                end, Menus),
    io:format("expand_menus ML = ~p~n",[Suffixes]),
    longest_common_prefix(Suffixes).

longest_common_prefix(Strings) ->
    longest_common_prefix(Strings, []).

longest_common_prefix(Strings, Result) ->
    {Prefixes, Tails} = lists:unzip(lists:map(
                                      fun([C|Cs]) -> {C, Cs};
                                         ([]) -> {empty, []}
                                      end, Strings)),
    case lists:member(empty, Prefixes) of
        true ->
            lists:reverse(Result);
        false ->
            case identical(Prefixes) of
                true ->
                    longest_common_prefix(Tails, [hd(Prefixes) | Result]);
                false ->
                    lists:reverse(Result)
            end
    end.

identical([A,A|As]) ->
    identical([A|As]);
identical([_,_|_]) ->
    false;
identical([_]) ->
    true;
identical([]) ->
    true.


pad(Str, Len) ->
    Pad = lists:duplicate(Len - length(Str), $\s),
    [Str, Pad].


max_cmd_len(Items) ->
    lists:max(lists:map(fun(#menu_item{node = Cmd}) -> length(Cmd) end, Items)).

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
