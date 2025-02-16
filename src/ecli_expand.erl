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
-module(ecli_expand).

-include("ecli_internal.hrl").
-include("../include/ecli.hrl").

-export([expand/2, expand/3]).

-export([parse/2, parse/3]).

-spec expand(string(), list()) -> {yes, string(), list()} | no.
expand(Str, Tree) ->
    expand(Str, Tree, undefined).

-spec expand(string(), list(), any()) -> {yes, string(), list()} | no.
expand(Str, Tree, Txn) ->
    case ecli_tokenise:string(Str) of
        {ok, Tokens} ->
            ?DBG("Tokens ~p~n", [Tokens]),
            parse(Tokens, Tree, Txn);
        no ->
            no
    end.

%% Tree directed parsing of the command line tokens
parse(Tokens, Tree) ->
    parse(Tokens, Tree, [], no_txn, no_cmd).

parse(Tokens, Tree, Txn) ->
    parse(Tokens, Tree, [], Txn, no_cmd).

parse([], Tree, _Acc, _Txn, _Cmd) ->
    %% No input yet, show the first menu
    Menu = ecli:format_menu(Tree),
    {yes, "", Menu};
parse([{token, Tok}], MenuItems, Acc, Txn, Cmd) ->
    ?DBG("Expanding after Token ~p = ~p~nAcc = ~p~n", [Tok, MenuItems, Acc]),
    expand_after_token(Tok, MenuItems, Acc, Txn, Cmd);
parse([space], MenuItems, Acc, Txn, Cmd) ->
    ?DBG("Expanding after space = ~p~n", [Acc]),
    ?DBG("Expanding after space menuitems = ~p~n", [MenuItems]),
    expand_after_space(MenuItems, Acc, Txn, Cmd);
parse([{part_string, Str}], _Tree, Acc, _Txn, _Cmd) ->
    %% Nothing we can do here, the user need to carry on typing their string
    %% (Or, maybe this is part of a list key we could complete... future)
    ?DBG("Expanding part string = ~p when Acc = ~n", [Str, Acc]),
    no;
parse([{token, Tok} | Ts], Tree, [], Txn, Cmd) ->
    %% A token at the start. Expect it to match a container in the initial Tree.
    %% ecli also understands #cmd{} records as its native container type
    case lookup(Tok, Tree) of
        {ok, #{node_type := container} = Item} ->
            NewCmd = maps:get(cmd_type, Item, Cmd),
            Children = menu_item_children(Item, Txn, NewCmd),
            %% io:format(user, "Starting with Item = ~p~n", [Item]),
            parse(Ts, Children, [Item], Txn, Cmd);
        {ok, #{node_type := leaf} = Item} ->
            %% io:format(user, "Starting with Item = ~p~n", [Item]),
            parse(Ts, [], [Item], Txn, Cmd);
        false ->
            %% Oops, end of the line
            no
    end;
parse([{token, Tok} | Ts], Tree, [#{node_type := NodeType} | _] = Acc, Txn, Cmd) when NodeType == container; NodeType == list ->
    %% A token after a container. Expect it to match an entry in the Tree
    case lookup(Tok, Tree) of
        {ok, #{node_type := container} = Item} ->
            NewCmd = maps:get(cmd_type, Item, Cmd),
            Children = menu_item_children(Item, Txn, NewCmd),
            ?DBG("Adding Container Item in container = ~p children ~p~n", [Item, Children]),
            parse(Ts, Children, [Item | Acc], Txn, NewCmd);
        {ok, #{node_type := Leaf, type := Type} = Item} when Leaf == leaf; Leaf == leaf_list ->
            %% Expecting a leaf value, possibly followed by more entries in the same
            %% container. If it's an enum type keep going, otherwise keep the same possible future tree, but without this node
            EnumTree = enum_values(Type),
            Tree1 = remove(Tok, Tree),
            ?DBG("Adding Leaf Item in container = ~p~n", [Item]),
            parse_leaf(Ts, EnumTree, Tree1, Item, Acc, Txn, Cmd);
        {ok, #{node_type := list} = Item} ->
            %% Expecting a key value next. Children could be all entries in the list.
            %% so accept the value given and deal with whether it exists etc if we need completion
            ?DBG("Adding List Item in container = ~p~n", [Item]),
            parse_list_keys(Ts, Item, Acc, Txn, Cmd);
        false ->
            %% Oops, end of the line
            no
    end;
parse([{token, _Tok} | _] = Ts, Tree, [#{node_type := leaf, value := _Value}, #{node_type := container} = Container | Acc], Txn, Cmd) ->
    %% Token after a leaf after a container when we also have the value
    %% Backtrack to to fetch something else from the remaining children of this container
    ?DBG("Skipping leaf value = ~p~n", [Container]),
    parse(Ts, Tree, [Container | Acc], Txn, Cmd);
parse([{token, _Tok} | Ts], Tree, [#{node_type := leaf, type := Type} = Leaf, #{node_type := container} = Container | Acc], Txn, Cmd) ->
    %% Token after a leaf after a container. It's the value, we can discard it for completion purposes
    %% Go back to to fetch something else from the remaining children of this container
    ?DBG("Setting leaf value in container = ~p ~p ~n", [Leaf, Container]),
    case enum_values(Type) of
        false ->
            parse(Ts, Tree, [Container | Acc], Txn, Cmd);
        EnumValues ->
            parse(Ts, EnumValues, [Leaf, Container | Acc], Txn, Cmd)
    end;
parse([{token, Tok} | Ts], Tree, [#{node_type := leaf} = Leaf | Acc], Txn, Cmd) ->
    %% Token after a leaf. This is the value. Just put it it in the leaf
    %% Can this really happen - leaf not inside a container??
    ?DBG("Setting leaf value = ~p~n", [Leaf]),
    Leaf1 = Leaf#{value => Tok},
    parse(Ts, Tree, [Leaf1 | Acc], Txn, Cmd);
parse([space | Ts], Tree, Acc, Txn, Cmd) ->
    %% We only track spaces for the last one, skip
    ?DBG("Skipping space~n", []),
    parse(Ts, Tree, Acc, Txn, Cmd).

%% Parsing a leaf is a side excursion. We need to juggle two menus/trees:
%% 1. Possible values of the leaf for enum alikes
%% 2. The remaining menu items of a parent container that contains
%%    multiple leafs.
parse_leaf([{token, Tok}], [#{name := Tok} = Item], Tree, Item, _Acc, _Txn, _Cmd) ->
    ?DBG("Expanding leaf value after Token ~p = ~p~nAcc = ~p~n", [Tok, [Item], _Acc]),
    Menu = ecli:format_menu(Tree),
    {yes, " ", Menu};
parse_leaf([{token, _Tok}], [], Tree, _Item, _Acc, _Txn, _Cmd) ->
    ?DBG("Expanding leaf value after Token ~p = ~p~nAcc = ~p~n", [_Tok, [_Item], _Acc]),
    Menu = ecli:format_menu(Tree),
    {yes, " ", Menu};
parse_leaf([{token, Tok}], EnumValues, Tree, _Item, _Acc, _Txn, _Cmd) ->
    Matches = filter_by_prefix(Tok, EnumValues),
    ?DBG("Expanding leaf value after Token ~p = ~p Matches = ~p~n", [Tok, EnumValues, Matches]),
    case Matches of
        [] ->
            no;
        [#{name := Name}] when Name == Tok ->
            Menu = ecli:format_menu(Tree),
            {yes, " ", Menu};
        [#{name := Name}] ->
            {yes, chars_to_expand(Tok, Name), []};
        _ ->
            Chars = expand_menus(Tok, Matches),
            Menu = ecli:format_menu(Matches),
            {yes, Chars, Menu}
    end;
parse_leaf([space], [], _Tree, #{node_type := leaf, desc := Desc}, _Acc, _Txn, _Cmd) ->
    %% Leaf with no value or type. User needs to start putting in some effort here!
    %% Help them out by showing the help text or enumerated values
    {yes, "", ["\r\n", Desc, "\r\n"]};
parse_leaf([space], EnumValues, _Tree, #{node_type := leaf}, _Acc, _Txn, _Cmd) ->
    Menu = ecli:format_menu(EnumValues),
    {yes, "", Menu};
parse_leaf([space], [], _Tree, _Item, _Acc, _Txn, _Cmd) ->
    no;
parse_leaf([space], EnumValues, _Tree, _Item, _Acc, _Txn, _Cmd) ->
    Menu = ecli:format_menu(EnumValues),
    {yes, "", Menu};
parse_leaf([{token, _Tok} | Ts], _, Tree, _Item, Acc, Txn, Cmd) ->
    %% Value is the token, drop the leaf and carry on from parent container
    ?DBG("Parsing leaf value after Token ~p = ~p~nAcc = ~p~n", [_Tok, [], Acc]),
    parse(Ts, Tree, Acc, Txn, Cmd);
parse_leaf([space | Ts], EnumValues, Tree, Item, Acc, Txn, Cmd) ->
    %% We only track spaces for the last one, skip
    ?DBG("Skipping space when expecting leaf ???~n", []),
    parse_leaf(Ts, EnumValues, Tree, Item, Acc, Txn, Cmd).


parse_list_keys([space], Item, _Acc, Txn, Cmd) ->
    %% Ended at space with incomplete list keys
    Children = menu_item_children(Item, Txn, Cmd),
    Menu = ecli:format_menu(Children),
    {yes, "", Menu};
parse_list_keys([space | Ts], Item, Acc, Txn, Cmd) ->
    parse_list_keys(Ts, Item, Acc, Txn, Cmd);
parse_list_keys([{token, Tok}], Item, _Acc, Txn, Cmd) ->
    %% Ended at end of a token, Could be a whole list key,
    %% part of the only one that matches, or not a match at all.
    ?DBG("Reached end of list key ~p~n", [Tok]),
    Children = menu_item_children(Item, Txn, Cmd),
    ?DBG("Reached end of list key got children ~p~n", [Children]),
    Matches = filter_by_prefix(Tok, Children),
    case Matches of
        [] ->
            no;
        [#cmd{name = Name} = Item] when Name == Tok ->
            Children = menu_item_children(Item, Txn, Cmd),
            Menu = ecli:format_menu(Children),
            {yes, " ", Menu};
        [#{name := Name} = Item] when Name == Tok ->
            Children = menu_item_children(Item, Txn, Cmd),
            Menu = ecli:format_menu(Children),
            {yes, " ", Menu};
        [#cmd{name = Name}] ->
            {yes, chars_to_expand(Tok, Name), []};
        [#{name := Name}] ->
            {yes, chars_to_expand(Tok, Name), []};
        _ ->
            Chars = expand_menus(Tok, Matches),
            Menu = ecli:format_menu(Matches),
            {yes, Chars, Menu}
    end;
parse_list_keys([{token, Tok} | Ts], #{key_names := KeyNames, key_values := KeyValues} = Item, Acc, Txn, Cmd) ->
    KeyValues1 = KeyValues ++ [Tok],
    Item1 = Item#{key_values => KeyValues1},
    if length(KeyNames) == length(KeyValues1) ->
            %% Got the list keys, carry on in the main parser
            Children = menu_item_children(Item1, Txn, Cmd),
            parse(Ts, Children, [Item1 | Acc], Txn, Cmd);
       true ->
            parse_list_keys(Ts, Item1, Acc, Txn, Cmd)
    end.


expand_after_token(_Tok, [], _Acc, _Txn, _Cmd) ->
    %% No possible matches for our token
    no;
expand_after_token(_Tok, _Menu, [#{node_type := leaf, value := _Val} | _Acc], _Txn, _Cmd) ->
    {yes, " ", []};
expand_after_token(_Tok, _Menu, [#{node_type := leaf} | _Acc], _Txn, _Cmd) ->
    {yes, " ", []};
expand_after_token(Tok, MenuItems, _Acc, Txn, Cmd) ->
    %% There are a number of possibly matching menu items.
    Matches = filter_by_prefix(Tok, MenuItems),
    case Matches of
        [] ->
            no;
        [#cmd{name = Name} = Item] when Name == Tok ->
            Children = menu_item_children(Item, Txn, Cmd),
            Menu = ecli:format_menu(Children),
            {yes, " ", Menu};
        [#{name := Name} = Item] when Name == Tok ->
            Children = menu_item_children(Item, Txn, Cmd),
            Menu = ecli:format_menu(Children),
            {yes, " ", Menu};
        [#cmd{name = Name}] ->
            {yes, chars_to_expand(Tok, Name), []};
        [#{name := Name}] ->
            {yes, chars_to_expand(Tok, Name), []};
        _ ->
            Chars = expand_menus(Tok, Matches),
            Menu = ecli:format_menu(Matches),
            {yes, Chars, Menu}
    end.

expand_after_space([], [#{node_type := leaf, value := _Val} | _Acc], _Txn, _Cmd) ->
    %% This leaf has a value, but no peer items. End of the command
    no;
expand_after_space(Tree, [#{node_type := leaf, value := _Val} | _Acc], _Txn, _Cmd) ->
    %% This leaf has a value. Show the remaining Menu items
    Menu = ecli:format_menu(Tree),
    {yes, "", Menu};
expand_after_space(_Menu, [#{node_type := leaf, desc := Desc, type := Type} | _Acc], _Txn, _Cmd) ->
    %% Leaf with no value. User needs to start putting in some effort here!
    %% Help them out by showing the help text or enumerated values
    case enum_values(Type) of
        [] ->
            {yes, "", ["\r\n", Desc, "\r\n"]};
        Tree ->
            Menu = ecli:format_menu(Tree),
            {yes, "", Menu}
    end;
expand_after_space(_Menu, [#{node_type := leaf, desc := Desc} | _Acc], _Txn, _Cmd) ->
    %% Leaf with no value or type. User needs to start putting in some effort here!
    %% Help them out by showing the help text or enumerated values
    {yes, "", ["\r\n", Desc, "\r\n"]};
expand_after_space([#{name := OneName} = Item], _Acc, Txn, Cmd) ->
    %% Ended at a menu containing container with just one possible child element, skip through and show the
    %% next menu
    Children = menu_item_children(Item, Txn, Cmd),
    %% ?DBG("ecli_expand: after spc children = ~p~n",[Children]),
    %% Only one - just fill it in for the user
    Menu = ecli:format_menu(Children),
    {yes, OneName ++ " ", Menu};
expand_after_space([#cmd{name = OneName} = Item], _Acc, Txn, Cmd) ->
    %% Ended at a cmd with just one possible child element, skip through and show the
    %% next menu
    Children = menu_item_children(Item, Txn, Cmd),
    Menu = ecli:format_menu(Children),
    {yes, OneName ++ " ", Menu};
expand_after_space(Tree, _Acc, _Txn, _Cmd) ->
    %% Ending at a space, we got this far ok, show the next menu item if present
    ?DBG("ecli_expand_after_space: fallthorgh = ~p~n",[Tree]),
    Menu = ecli:format_menu(Tree),
    {yes, "", Menu}.

enum_values(boolean) ->
    [#{name => "true", desc => "True"},
     #{name => "false", desc => "False"}];
enum_values(_) ->
    [].

filter_by_prefix(_Str, []) ->
    [];
filter_by_prefix(Str, Menu) ->
    lists:filter(fun(#{name := Name}) ->
                         lists:prefix(Str, Name);
                    (#cmd{name = Name}) ->
                         lists:prefix(Str, Name)
                 end, Menu).

chars_to_expand("", Match) -> Match ++ " ";
chars_to_expand(Str, Match) ->
    lists:sublist(Match, length(Str) + 1, length(Match) - 1) ++ " ".

lookup(Name, [#{name := Name} = Item | _Tree]) ->
    {ok, Item};
lookup(Name, [#cmd{name = Name} = Item | _Tree]) ->
    {ok, ecli_util:cmd_to_map(Item)};
lookup(Name, [_ | Tree]) ->
    lookup(Name, Tree);
lookup(_, []) ->
    false.

remove(Tok, Tree) ->
    lists:filter(fun(#{name := Name}) -> Name /= Tok end, Tree).

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
    ecli_util:children(Item, Txn, CmdType).

%% Find characters to add to fill up to where the node names diverge
%% e.g. names configure and contain given an input of "c" should return "on"
expand_menus(Str, Menus) ->
    %% ?DBG("ecli_expand:expand_menus ~p~n ~p ~n",[Str, Menus]),
    StrLen = length(Str),
    Suffixes = lists:map(fun(#{name := Name}) ->
                                 lists:nthtail(StrLen, Name);
                            (#cmd{name = Name}) ->
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




