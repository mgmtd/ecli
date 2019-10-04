%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc A generic command expander useful for all kinds of trees of
%%%      records assuming the records include a mandatory set of elements:
%%%      name, children, action, node_type
%%%
%%% @end
%%% Created : 17 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cli_expand).

-export([expand/3, expand/4]).

%% @doc expand a string based on a provided tree of nodes and the
%% commands to extract known elements from tree items
%%
%% Returns no if nothing at all matched, or {yes, CharsToInsert, Menu}
%% where CharsToInsert are characters to be automatically appended to
%% the user input, and Menu is a list of nodes to be presented to the
%% user as a menu of possible next items.
-spec expand(string(), list(), list()) -> {yes, string(), list()} | no.
expand(Str, Tree, Getters) ->
    expand(Str, Tree, Getters, undefined).

expand(Str, Tree, Getters, Txn) ->
    Stripped = cli_util:strip_ws(Str),
    expand(Stripped, Tree, [], Getters, Txn).

%%
%% Expand a string according to a generic tree.
%% This expansion termiates when we run out of characters, reach a
%% terminal node, or the string no longer matches (syntax error)
%%
expand([], [], _, _, _) ->
    no;
expand([$\s|Cs], [Item], MatchedChars, Gs, Txn) ->
    Node = cli_util:get_name(Gs, Item),
    case MatchedChars of
        Node ->
            %% Matched a full single level in the command tree with a
            %% following space. Carry on to children
            io:format("expand matched full level ~p~p~n",[Node,Item]),
            {Children, NewGs} = menu_item_children(Item, Txn, Gs),
            expand(Cs, Children, [], NewGs, Txn);
        _ ->
            no
    end;
expand([C|Cs], MenuItems, Matched, Gs, Txn) ->
    io:format("expand normal chars ~p matched = ~p~n",[C, Matched]),
    SoFar = Matched ++ [C],
    Matches = match_cmds(SoFar, MenuItems, Gs),
    io:format("expand normal chars ~p matches = ~p~n",[SoFar, Matches]),
    case Matches of
        [] ->
            no;
        Items ->
            expand(Cs, Items, SoFar, Gs, Txn)
    end;
expand([], MenuItems, MatchedStr, Gs, Txn) ->
    %% Reached the end of the input without getting to the end of a
    %% cmd with following space
    io:format("expand end before space ~p matched = ~p~n",[MenuItems, MatchedStr]),
    case MenuItems of
        [Item] ->
            %% We have reached a single node.
            Name = cli_util:get_name(Gs, Item),
            if Name == MatchedStr ->
                    %% And it has all the characters of that
                    %% node. Insert the space on behalf of the user
                    %% and grab the children to show as a menu.
                    {Children, Getters} = menu_item_children(Item, Txn, Gs),
                    Menu = cli:format_menu(Children, Getters),
                    {yes, " ", Menu};
               true ->
                    %% The user didn't type all the characters, but it
                    %% was enough to identify the single node. Insert the
                    %% rest of the chars plus the following
                    %% space. Don't get too carried away with showing
                    %% the children as well at this point - it would
                    %% all feel a bit too magic. Let them hit tab
                    %% again to see the menu.
                    {yes, chars_to_expand(MatchedStr, Name), []}
            end;
        MenuItems ->
            %% Still a number of matching menu items. auto fill up to
            %% the point they diverge
            Chars = expand_menus(MatchedStr, MenuItems, Gs),
            Menu = cli:format_menu(MenuItems, Gs),
            {yes, Chars, Menu}
    end.


match_cmds(_Str, [], _) ->
    [];
match_cmds(Str, Menu, Gs) ->
    lists:filter(fun(Item) ->
                         Name = cli_util:get_name(Gs, Item),
                         lists:prefix(Str, Name)
                 end, Menu).

chars_to_expand(Str, Match) ->
    lists:sublist(Match, length(Str) + 1, length(Match) - 1) ++ " ".

menu_item_children(Item, Txn, Gs) ->
    Cs = cli_util:get_children(Gs, Item),
    io:format("Children = ~p~n",[Cs]),
    case cli_util:eval_childspec(Cs, Txn) of
        {Children, Getters} ->
            {Children, Getters};
        Children ->
            {Children, Gs}
    end.

%% Find characters to add to fill up to where the node names diverge
%% e.g. names configure and contain given an input of "c" should return "on"
expand_menus(Str, Menus, Gs) ->
    StrLen = length(Str),
    Suffixes = lists:map(fun(Item) ->
                                 Name = cli_util:get_name(Gs, Item),
                                 lists:nthtail(StrLen, Name)
                         end, Menus),
    %% io:format("expand_menus ML = ~p~n",[Suffixes]),
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




