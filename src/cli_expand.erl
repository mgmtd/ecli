%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc A generic command expander useful for all kinds of trees of
%%%      records assuming the records include a mandatory set of elements:
%%%      name, children, node_type
%%%
%%% @end
%%% Created : 17 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cli_expand).

-export([expand/3, expand/4]).

%%--------------------------------------------------------------------
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
    expand(Stripped, Tree, [], Getters, false, Txn).


%% Expand a string according to a generic tree.
%% This expansion termiates when we run out of characters, reach a
%% terminal node, or the string no longer matches (syntax error)
%%
%% MenuItems here is always the current set of nodes we are trying to
%% match against
expand([], MenuItems, MatchedStr, Gs, AddListItems, Txn) ->
    io:format("expand end no space ~p~n",[length(MenuItems)]),
    %% The end of the input but no space.
    case MenuItems of
        [] ->
            %% Nothing matching
            no;
        [Item] ->
            Name = cli_util:get_name(Gs, Item),
            NodeType = cli_util:get_node_type(Gs, Item),
            if NodeType == new_list_item andalso AddListItems == true ->
                    %% Adding the first new list item. If the user has
                    %% typed some characters towards the list item add
                    %% the space to allow them to move on
                    {yes, " ", []};
               Name == MatchedStr ->
                    %% And it has all the characters of that
                    %% node. Insert the space on behalf of the user
                    %% and grab the children to show as a menu.
                    {Children, NewGs, NewAddListItems} =
                        menu_item_children(Item, Txn, Gs, AddListItems),
                    Menu = cli:format_menu(Children, NewGs),
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
    end;
expand([$\s], [], _, _, _, _) ->
    %% End of input after a space, but no matchable children
    no;
expand([$\s], Items, MatchedChars, Gs, AddListItems, Txn) ->
    io:format("expand space at end Items = ~p Matched = ~p~n",[length(Items), MatchedChars]),
    %% Its the end of the input after a space, we want to show menus
    case Items of
        [Item] ->
            %% There's only one
            Name = cli_util:get_name(Gs, Item),
            NodeType = cli_util:get_node_type(Gs, Item),
            if NodeType == new_list_item andalso AddListItems == true ->
                    %% Adding the first new list item
                    %% io:format("NEW LIST ITEM ~p~n", [Item]),
                    Menu = cli:format_menu(Items, Gs),
                    {yes, "", Menu};
               Name == MatchedChars ->
                    %% The current node is the one, show its children
                    %% in a menu
                    {Children, NewGs, NewAddListItems} =
                        menu_item_children(Item, Txn, Gs, AddListItems),
                    %% io:format("cli_expand: after spc children = ~p~n",[Children]),
                    case Children of
                        [One] when NodeType /= list ->
                            %% Only one - just fill it in for the user
                            OneName = cli_util:get_name(NewGs, One),
                            {yes, OneName ++ " ", ""};
                        _ ->
                            Menu = cli:format_menu(Children, NewGs),
                            {yes, "", Menu}
                    end;
               true ->
                    %% The user didn't type all the characters, but it
                    %% was enough to identify the single node. Insert the
                    %% rest of the chars plus the following
                    %% space. Don't get too carried away with showing
                    %% the children as well at this point - it would
                    %% all feel a bit too magic. Let them hit tab
                    %% again to see the menu.
                    {yes, chars_to_expand(MatchedChars, Name), []}
            end;
        [Item|_] ->
            NodeType = cli_util:get_node_type(Gs, Item),
            if NodeType == new_list_item andalso AddListItems == true ->
                    %% This is the first list item
                    {Children, NewGs, NewAddListItems} =
                        menu_item_children(Item, Txn, Gs, AddListItems),
                    %% io:format("Children new list = ~p of item ~p~n",[Children, Item]),
                    Menu = cli:format_menu(Children, NewGs),
                    {yes, "", Menu};
               true ->
                    %% Still a number of matching menu items. auto fill up to
                    %% the point they diverge
                    Chars = expand_menus(MatchedChars, Items, Gs),
                    Menu = cli:format_menu(Items, Gs),
                    {yes, Chars, Menu}
            end
    end;
expand([$\s|Cs], [Item], MatchedChars, Gs, AddListItems, Txn) ->
    io:format("expand space ~p matched = ~p~n",[1, MatchedChars]),
    Node = cli_util:get_name(Gs, Item),
    case MatchedChars of
        Node ->
            %% Matched a full single level in the command tree with a
            %% following space. Carry on to children
            %% io:format("expand matched full level ~p~p~n",[Node,Item]),
            {Children, NewGs, NewAddListItems} =
                menu_item_children(Item, Txn, Gs, AddListItems),
            expand(Cs, Children, [], NewGs, NewAddListItems, Txn);
        _ ->
            no
    end;
expand([$\s|Cs], [Item | _] = MenuItems, MatchedChars, Gs, AddListItems, Txn) ->
    NodeType = cli_util:get_node_type(Gs, Item),
    if  NodeType == new_list_item andalso AddListItems == true ->
            %% Reached a space in the middle. If we are adding a list
            %% item use it and move on
            {Children, NewGs, NewAddListItems} =
                menu_item_children(Item, Txn, Gs, AddListItems),
            expand(Cs, Children, [], NewGs, NewAddListItems, Txn);
        true ->
            no
    end;
expand([C|Cs], MenuItems, Matched, Gs, AddListItems, Txn) ->
    io:format("expand normal chars ~p matched = ~p~n",[C, Matched]),
    SoFar = Matched ++ [C],
    Matches = match_cmds(SoFar, MenuItems, Gs),
    case Matches of
        [] ->
            %% But we could be adding a new list item, so would not have matches
            [Item|_] = MenuItems,
            NodeType = cli_util:get_node_type(Gs, Item),
            if NodeType == new_list_item andalso AddListItems == true ->
                    expand(Cs, MenuItems, SoFar, Gs, AddListItems, Txn);
               true ->
                    no
            end;
        Items ->
            expand(Cs, Items, SoFar, Gs, AddListItems, Txn)
    end.

match_cmds(_Str, [], _) ->
    [];
match_cmds(Str, Menu, Gs) ->
    lists:filter(fun(Item) ->
                         Name = cli_util:get_name(Gs, Item),
                         lists:prefix(Str, Name)
                 end, Menu).

chars_to_expand("", Match) -> Match ++ " ";
chars_to_expand(Str, Match) ->
    lists:sublist(Match, length(Str) + 1, length(Match) - 1) ++ " ".

%% Return the children of the current node.
%%
%% For container nodes this is a simple get of the schema children
%%
%% Leaf type nodes should not have children
%%
%% list nodes will by default have the elements that make up the list
%% entry as children, but we need to handle the list key(s) first.  So
%% children of the list item are existing keys, plus the potential to
%% add a new list item. list items with multiple keys need to handle
%% each key in sequence. Once we have all list keys children become
%% the items inside the list item, minus the list key names.
menu_item_children(Item, Txn, Gs, AddListItems) ->
    Cs = cli_util:get_children(Gs, Item, Txn, AddListItems),
    case cli_util:eval_childspec(Cs, Txn) of
        {Children, Getters} ->
            {Children, Getters, AddListItems};
        {Children, Getters, NewAddListItems} ->
            {Children, Getters, NewAddListItems};
        Children ->
            {Children, Gs, AddListItems}
    end.


%% Find characters to add to fill up to where the node names diverge
%% e.g. names configure and contain given an input of "c" should return "on"
expand_menus(Str, Menus, Gs) ->
    io:format("cli_expand:expand_menus ~p~n ~p~n ~p ~n",[Str, Menus, Gs]),
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




