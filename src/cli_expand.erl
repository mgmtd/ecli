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

-include("debug.hrl").

-export([expand/2, expand/3]).

%%--------------------------------------------------------------------
%% @doc expand a string based on a provided tree of nodes and the
%% commands to extract known elements from tree items
%%
%% Returns no if nothing at all matched, or {yes, CharsToInsert, Menu}
%% where CharsToInsert are characters to be automatically appended to
%% the user input, and Menu is a list of nodes to be presented to the
%% user as a menu of possible next items.
-spec expand(string(), list(), list()) -> {yes, string(), list()} | no.
expand(Str, Tree) ->
    expand(Str, Tree, undefined).

expand(Str, Tree, Txn) ->
    Stripped = cli_util:strip_ws(Str),
    expand(Stripped, Tree, [], Txn, undefined).


%% Expand a string according to a generic tree.
%% This expansion termiates when we run out of characters, reach a
%% terminal node, or the string no longer matches (syntax error)
%%
%% MenuItems here is always the current set of nodes we are trying to
%% match against
expand([], MenuItems, MatchedStr, Txn, CmdType) ->
    ?DBG("expand end no space ~p~n",[length(MenuItems)]),
    %% The end of the input but no space.
    case MenuItems of
        [] ->
            %% Nothing matching
            no;
        [#{name := Name, node_type := NodeType} = Item] ->
            if NodeType == new_list_item ->
                    %% Adding a new list item. If the user has
                    %% typed some characters towards the list item add
                    %% the space to allow them to move on
                    {yes, " ", []};
               Name == MatchedStr ->
                    %% And it has all the characters of that
                    %% node. Insert the space on behalf of the user
                    %% and grab the children to show as a menu.
                    Children = menu_item_children(Item, Txn, CmdType),
                    Menu = cli:format_menu(Children),
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
        [#{node_type := new_list_item} | _] ->
            %% Adding a new list item when there are exsiting items
            {yes, " ", []};
        MenuItems ->
            %% Still a number of matching menu items. auto fill up to
            %% the point they diverge
            Chars = expand_menus(MatchedStr, MenuItems),
            Menu = cli:format_menu(MenuItems),
            {yes, Chars, Menu}
    end;
expand([$\s], [], _, _, _) ->
    %% End of input after a space, but no matchable children
    no;
expand([$\s], Items, MatchedChars, Txn, CmdType) ->
    ?DBG("expand space at end Items = ~p Matched = ~p~n",[length(Items), MatchedChars]),
    ?DBG("Items ~p~n",[Items]),
    %% Its the end of the input after a space, we want to show menus
    case Items of
        [#{name := Name, node_type := NodeType} = Item] ->
            %% There's only one
            if NodeType == new_list_item; NodeType == list_key ->
                    %% Adding the first new list item
                    ?DBG("Appennd Key Val ~p~n", [MatchedChars]),
                    #{key_values := KVs} = Item,
                    Item1 = Item#{key_values => KVs ++ [MatchedChars]},
                    Children = menu_item_children(Item1, Txn, CmdType),
                    Menu = cli:format_menu(Children),
                    {yes, "", Menu};
               Name == MatchedChars ->
                    %% The current node is the one, show its children
                    %% in a menu
                    Cmd = cmd_type(CmdType, Item),
                    Children = menu_item_children(Item, Txn, Cmd),
                    ?DBG("cli_expand: after spc children = ~p~n",[Children]),
                    case Children of
                        [#{name := OneName}] when NodeType /= list ->
                            %% Only one - just fill it in for the user
                            {yes, OneName ++ " ", ""};
                        _ ->
                            Menu = cli:format_menu(Children),
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
        [#{node_type := NodeType} = Item|_] ->
            if NodeType == new_list_item ->
                    %% This is a new list item
                    #{key_values := KVs} = Item,
                    ?DBG("Appennd Key Val ~p~n", [MatchedChars]),
                    Item1 = Item#{key_values => KVs ++ [MatchedChars]},
                    Children = menu_item_children(Item1, Txn, CmdType),
                    ?DBG("Children new list ~p of item ~p~n",[Children, Item1]),
                    Menu = cli:format_menu(Children),
                    {yes, "", Menu};
               true ->
                    %% Still a number of matching menu items. auto fill up to
                    %% the point they diverge
                    Chars = expand_menus(MatchedChars, Items),
                    Menu = cli:format_menu(Items),
                    {yes, Chars, Menu}
            end
    end;
expand([$\s|Cs], [#{name := Node, node_type := new_list_item} = Item], MatchedChars, Txn, CmdType) ->
    %% We got down to just the special new list item. It must be a new
    %% entry in our list as any existing entries have been discared
    %% already
    #{key_values := KVs} = Item,
    ?DBG("Appennd Key Val ~p~n", [MatchedChars]),
    Item1 = Item#{key_values => KVs ++ [MatchedChars]},
    Children = menu_item_children(Item1, Txn, CmdType),
    ?DBG("expand new list space Childrten ~p~n",[Children]),
    expand(Cs, Children, [], Txn, CmdType);
expand([$\s|Cs], [#{name := Node, node_type := NodeType} = Item], MatchedChars, Txn, CmdType) ->
    ?DBG("expand space ~p matched = ~p~n",[1, MatchedChars]),
    ?DBG("expand space Item ~p~n",[Item]),
    case MatchedChars of
        Node when NodeType == list_key ->
            %% Matched an existing list item. Add it to the list keys
            %% and carry on expanding
            #{key_values := KVs} = Item,
            ?DBG("Appennd Key Val ~p~n", [MatchedChars]),
            Item1 = Item#{key_values => KVs ++ [MatchedChars]},
            Children = menu_item_children(Item1, Txn, CmdType),
            ?DBG("expand space Childrten ~p~n",[Children]),
            expand(Cs, Children, [], Txn, CmdType);
        Node ->
            %% Matched a full single level in the command tree with a
            %% following space. Carry on to children
            %% ?DBG("expand matched full level ~p~p~n",[Node,Item]),
            Cmd = cmd_type(CmdType, Item),
            Children = menu_item_children(Item, Txn, Cmd),
            ?DBG("expand space Childrten ~p~n",[Children]),
            expand(Cs, Children, [], Txn, Cmd);
        _ ->
            no
    end;
expand([$\s|Cs], [#{node_type := NodeType} = Item | _] = MenuItems, MatchedChars, Txn, CmdType) ->
    if  NodeType == new_list_item ->
            %% Reached a space in the middle. If we are adding a list
            %% item use it and move on
            #{key_values := KVs} = Item,
            ?DBG("Appennd Key Val ~p~n", [MatchedChars]),
            Item1 = Item#{key_values => KVs ++ [MatchedChars]},
            Children = menu_item_children(Item1, Txn, CmdType),
            expand(Cs, Children, [], Txn, CmdType);
        true ->
            no
    end;
expand([C|Cs], [#{node_type := new_list_item} = NLI |Items] = MenuItems, Matched, Txn, CmdType) ->
    %% When we are adding a new list item only try to expand against existing items, we want the special new_list_item entry to stay around
    SoFar = Matched ++ [C],
    Matches = filter_by_prefix(SoFar, Items),
    case Matches of
        [] ->
            expand(Cs, [NLI], SoFar, Txn, CmdType);
        KeptItems ->
            expand(Cs, KeptItems, SoFar, Txn, CmdType)
    end;
expand([C|Cs], MenuItems, Matched, Txn, CmdType) ->
    ?DBG("expand normal chars ~p matched = ~p~n",[[C], Matched]),
    ?DBG("expand normal chars Items ~p~n",[MenuItems]),
    SoFar = Matched ++ [C],
    Matches = filter_by_prefix(SoFar, MenuItems),
    case Matches of
        [] ->
            ?DBG("expand normal chars no matches true ~n",[]),
            no;
        Items ->
            expand(Cs, Items, SoFar, Txn, CmdType)
    end.

cmd_type(undefined, #{name := "set"}) ->
    set;
cmd_type(undefined, #{}) ->
    show;
cmd_type(Type, _) ->
    Type.

filter_by_prefix(_Str, []) ->
    [];
filter_by_prefix(Str, Menu) ->
    lists:filter(fun(#{name := Name}) ->
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
menu_item_children(Item, Txn, CmdType) ->
     cli_util:children(Item, Txn, CmdType).

%% Find characters to add to fill up to where the node names diverge
%% e.g. names configure and contain given an input of "c" should return "on"
expand_menus(Str, Menus) ->
    ?DBG("cli_expand:expand_menus ~p~n ~p ~n",[Str, Menus]),
    StrLen = length(Str),
    Suffixes = lists:map(fun(#{name := Name}) ->
                                 lists:nthtail(StrLen, Name)
                         end, Menus),
    %% ?DBG("expand_menus ML = ~p~n",[Suffixes]),
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
            case identical_prefixes(Prefixes) of
                true ->
                    longest_common_prefix(Tails, [hd(Prefixes) | Result]);
                false ->
                    lists:reverse(Result)
            end
    end.

identical_prefixes([A,A|As]) ->
    identical_prefixes([A|As]);
identical_prefixes([_,_|_]) ->
    false;
identical_prefixes([_]) ->
    true;
identical_prefixes([]) ->
    true.




