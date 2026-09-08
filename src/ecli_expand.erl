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
            parse(normalise(Tokens), Tree, Txn);
        no ->
            no
    end.

%% Tree directed parsing of the command line tokens
parse(Tokens, Tree) ->
    parse(Tokens, Tree, [], no_txn, no_cmd, undefined).

parse(Tokens, Tree, Txn) ->
    parse(Tokens, Tree, [], Txn, no_cmd, undefined).

parse([], Tree, Acc, _Txn, _Cmd, Pipes) ->
    %% No input yet, show the first menu
    Menu = ecli:format_menu(with_pipe(Tree, Acc, Pipes)),
    {yes, "", Menu};
parse([pipe | Ts], _Tree, _Acc, Txn, Cmd, Pipes) ->
    case has_pipes(Pipes) of
        false ->
            no;
        true when Ts =:= [] ->
            %% Completing `|` inserts a trailing space, same as a finished token.
            {yes, " ", ecli:format_menu(Pipes)};
        true ->
            %% `|` terminates the command path and restarts in the pipe catalog.
            parse(Ts, Pipes, [], Txn, Cmd, Pipes)
    end;
parse([{token, Tok}], MenuItems, Acc, Txn, Cmd, Pipes) ->
    ?DBG("Expanding after Token ~p = ~p~nAcc = ~p~n", [Tok, MenuItems, Acc]),
    expand_after_token(Tok, MenuItems, Acc, Txn, Cmd, Pipes);
parse([space], MenuItems, Acc, Txn, Cmd, Pipes) ->
    ?DBG("Expanding after space = ~p~n", [Acc]),
    ?DBG("Expanding after space menuitems = ~p~n", [MenuItems]),
    expand_after_space(MenuItems, Acc, Txn, Cmd, Pipes);
parse([{part_string, _Str}], _Tree, _Acc, _Txn, _Cmd, _Pipes) ->
    %% Nothing we can do here, the user need to carry on typing their string
    %% (Or, maybe this is part of a list key we could complete... future)
    ?DBG("Expanding part string = ~p when Acc = ~n", [_Str, _Acc]),
    no;
parse([{token, Tok} | Ts], Tree, [], Txn, Cmd, Pipes) ->
    %% A token at the start. Expect it to match a container in the initial Tree.
    %% ecli also understands #cmd{} records as its native container type
    case lookup(Tok, Tree) of
        {ok, #{node_type := container} = Item} ->
            NewCmd = maps:get(cmd_type, Item, Cmd),
            Pipes1 = update_pipes(Item, Pipes),
            Children = menu_item_children(Item, Txn, NewCmd),
            parse(Ts, Children, [Item], Txn, Cmd, Pipes1);
        {ok, #{node_type := leaf} = Item} ->
            Pipes1 = update_pipes(Item, Pipes),
            parse(Ts, [], [Item], Txn, Cmd, Pipes1);
        false ->
            no
    end;
parse([{token, Tok} | Ts], Tree, [#{node_type := NodeType} | _] = Acc, Txn, Cmd, Pipes)
  when NodeType == container; NodeType == list ->
    %% A token after a container. Expect it to match an entry in the Tree
    case lookup(Tok, Tree) of
        {ok, #{node_type := container} = Item} ->
            NewCmd = maps:get(cmd_type, Item, Cmd),
            Pipes1 = update_pipes(Item, Pipes),
            Children = menu_item_children(Item, Txn, NewCmd),
            ?DBG("Adding Container Item in container = ~p children ~p~n", [Item, Children]),
            parse(Ts, Children, [Item | Acc], Txn, NewCmd, Pipes1);
        {ok, #{node_type := Leaf, type := Type} = Item} when Leaf == leaf; Leaf == leaf_list ->
            EnumTree = ecli_types:completions(Type),
            Tree1 = remove(Tok, Tree),
            Pipes1 = update_pipes(Item, Pipes),
            ?DBG("Adding Leaf Item in container = ~p~n", [Item]),
            parse_leaf(Ts, EnumTree, Tree1, Item, Acc, Txn, Cmd, Pipes1);
        {ok, #{node_type := list} = Item} ->
            Pipes1 = update_pipes(Item, Pipes),
            ?DBG("Adding List Item in container = ~p~n", [Item]),
            parse_list_keys(Ts, Item, Acc, Txn, Cmd, Pipes1);
        false ->
            no
    end;
parse([{token, _Tok} | _] = Ts, Tree, [#{node_type := leaf, value := _Value}, #{node_type := NT} = Parent | Acc], Txn, Cmd, Pipes)
  when NT == container; NT == list ->
    ?DBG("Skipping leaf value = ~p~n", [Parent]),
    parse(Ts, Tree, [Parent | Acc], Txn, Cmd, Pipes);
parse([{token, _Tok} | Ts], Tree, [#{node_type := leaf, type := Type} = Leaf, #{node_type := NT} = Parent | Acc], Txn, Cmd, Pipes)
  when NT == container; NT == list ->
    ?DBG("Setting leaf value in container = ~p ~p ~n", [Leaf, Parent]),
    case ecli_types:completions(Type) of
        [] ->
            parse(Ts, Tree, [Parent | Acc], Txn, Cmd, Pipes);
        EnumValues ->
            parse(Ts, EnumValues, [Leaf, Parent | Acc], Txn, Cmd, Pipes)
    end;
parse([{token, Tok} | Ts], Tree, [#{node_type := leaf} = Leaf | Acc], Txn, Cmd, Pipes) ->
    ?DBG("Setting leaf value = ~p~n", [Leaf]),
    Leaf1 = Leaf#{value => Tok},
    parse(Ts, Tree, [Leaf1 | Acc], Txn, Cmd, Pipes);
parse([space | Ts], Tree, Acc, Txn, Cmd, Pipes) ->
    ?DBG("Skipping space~n", []),
    parse(Ts, Tree, Acc, Txn, Cmd, Pipes).

%% Parsing a leaf is a side excursion. We need to juggle two menus/trees:
%% 1. Possible values of the leaf for enum alikes
%% 2. The remaining menu items of a parent container that contains
%%    multiple leafs.
parse_leaf([{token, Tok}], [#{name := Tok} = Item], Tree, Item, Acc, _Txn, _Cmd, Pipes) ->
    ?DBG("Expanding leaf value after Token ~p = ~p~nAcc = ~p~n", [Tok, [Item], Acc]),
    Menu = ecli:format_menu(with_pipe(Tree, Acc, Pipes)),
    {yes, " ", Menu};
parse_leaf([{token, _Tok}], [], Tree, _Item, Acc, _Txn, _Cmd, Pipes) ->
    ?DBG("Expanding leaf value after Token ~p = ~p~nAcc = ~p~n", [_Tok, [_Item], Acc]),
    Menu = ecli:format_menu(with_pipe(Tree, Acc, Pipes)),
    {yes, " ", Menu};
parse_leaf([{token, Tok}], EnumValues, Tree, _Item, Acc, _Txn, _Cmd, Pipes) ->
    Matches = filter_by_prefix(Tok, EnumValues),
    ?DBG("Expanding leaf value after Token ~p = ~p Matches = ~p~n", [Tok, EnumValues, Matches]),
    case Matches of
        [] ->
            no;
        [#{name := Name}] when Name == Tok ->
            Menu = ecli:format_menu(with_pipe(Tree, Acc, Pipes)),
            {yes, " ", Menu};
        [#{name := Name}] ->
            {yes, chars_to_expand(Tok, Name), []};
        _ ->
            Chars = expand_menus(Tok, Matches),
            Menu = ecli:format_menu(Matches),
            {yes, Chars, Menu}
    end;
parse_leaf([space], [], _Tree, #{node_type := leaf, desc := Desc}, _Acc, _Txn, _Cmd, _Pipes) ->
    {yes, "", ["\r\n", Desc, "\r\n"]};
parse_leaf([space], EnumValues, _Tree, #{node_type := leaf}, _Acc, _Txn, _Cmd, _Pipes) ->
    Menu = ecli:format_menu(EnumValues),
    {yes, "", Menu};
parse_leaf([space], [], _Tree, _Item, _Acc, _Txn, _Cmd, _Pipes) ->
    no;
parse_leaf([space], EnumValues, _Tree, _Item, _Acc, _Txn, _Cmd, _Pipes) ->
    Menu = ecli:format_menu(EnumValues),
    {yes, "", Menu};
parse_leaf([pipe | _] = Ts, _, Tree, _Item, Acc, Txn, Cmd, Pipes) ->
    parse(Ts, Tree, Acc, Txn, Cmd, Pipes);
parse_leaf([{token, _Tok} | Ts], _, Tree, _Item, Acc, Txn, Cmd, Pipes) ->
    ?DBG("Parsing leaf value after Token ~p = ~p~nAcc = ~p~n", [_Tok, [], Acc]),
    parse(Ts, Tree, Acc, Txn, Cmd, Pipes);
parse_leaf([space | Ts], EnumValues, Tree, Item, Acc, Txn, Cmd, Pipes) ->
    ?DBG("Skipping space when expecting leaf ???~n", []),
    parse_leaf(Ts, EnumValues, Tree, Item, Acc, Txn, Cmd, Pipes).


parse_list_keys([pipe | _] = Ts, Item, Acc, Txn, Cmd, Pipes) ->
    parse(Ts, [], [Item | Acc], Txn, Cmd, Pipes);
parse_list_keys([space], Item, Acc, Txn, Cmd, Pipes) ->
    %% Ended at space with incomplete list keys
    Children = menu_item_children(Item, Txn, Cmd),
    Menu = ecli:format_menu(with_pipe(Children, [Item | Acc], Pipes)),
    {yes, "", Menu};
parse_list_keys([space | Ts], Item, Acc, Txn, Cmd, Pipes) ->
    parse_list_keys(Ts, Item, Acc, Txn, Cmd, Pipes);
parse_list_keys([{token, Tok}], Item, Acc, Txn, Cmd, Pipes) ->
    ?DBG("Reached end of list key ~p~n", [Tok]),
    Children = menu_item_children(Item, Txn, Cmd),
    ?DBG("Reached end of list key got children ~p~n", [Children]),
    Matches = filter_by_prefix(Tok, Children),
    case Matches of
        [] ->
            no;
        [#cmd{name = Name} = Matched] when Name == Tok ->
            Next = menu_item_children(Matched, Txn, Cmd),
            Menu = ecli:format_menu(with_pipe(Next, [Matched | Acc], Pipes)),
            {yes, " ", Menu};
        [#{name := Name} = Matched] when Name == Tok ->
            Next = menu_item_children(Matched, Txn, Cmd),
            Menu = ecli:format_menu(with_pipe(Next, [Matched | Acc], Pipes)),
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
parse_list_keys([{token, Tok} | Ts], #{key_names := KeyNames, key_values := KeyValues} = Item, Acc, Txn, Cmd, Pipes) ->
    KeyValues1 = KeyValues ++ [Tok],
    Item1 = Item#{key_values => KeyValues1},
    if length(KeyNames) == length(KeyValues1) ->
            Children = menu_item_children(Item1, Txn, Cmd),
            parse(Ts, Children, [Item1 | Acc], Txn, Cmd, Pipes);
       true ->
            parse_list_keys(Ts, Item1, Acc, Txn, Cmd, Pipes)
    end.


expand_after_token(_Tok, [], Acc, _Txn, _Cmd, Pipes) ->
    case offer_pipe(Acc, Pipes) of
        true ->
            {yes, " ", ecli:format_menu([pipe_menu_item()])};
        false ->
            no
    end;
expand_after_token(_Tok, Menu, [#{node_type := leaf, value := _Val} | _] = Acc, _Txn, _Cmd, Pipes) ->
    {yes, " ", ecli:format_menu(with_pipe(Menu, Acc, Pipes))};
expand_after_token(_Tok, _Menu, [#{node_type := leaf} | _Acc], _Txn, _Cmd, _Pipes) ->
    {yes, " ", []};
expand_after_token(Tok, MenuItems, Acc, Txn, Cmd, Pipes) ->
    Matches = filter_by_prefix(Tok, MenuItems),
    case Matches of
        [] ->
            no;
        [#cmd{name = Name} = Item] when Name == Tok ->
            Children = menu_item_children(Item, Txn, Cmd),
            Pipes1 = update_pipes(ecli_util:cmd_to_map(Item), Pipes),
            Menu = ecli:format_menu(with_pipe(Children, [Item | Acc], Pipes1)),
            {yes, " ", Menu};
        [#{name := Name} = Item] when Name == Tok ->
            Children = menu_item_children(Item, Txn, Cmd),
            Pipes1 = update_pipes(Item, Pipes),
            Menu = ecli:format_menu(with_pipe(Children, [Item | Acc], Pipes1)),
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

expand_after_space([], Acc, _Txn, _Cmd, Pipes) ->
    case offer_pipe(Acc, Pipes) of
        true ->
            {yes, "", ecli:format_menu([pipe_menu_item()])};
        false ->
            no
    end;
expand_after_space(Tree, [#{node_type := leaf, value := _Val} | _] = Acc, _Txn, _Cmd, Pipes) ->
    Menu = ecli:format_menu(with_pipe(Tree, Acc, Pipes)),
    {yes, "", Menu};
expand_after_space(_Menu, [#{node_type := leaf, desc := Desc, type := Type} | _Acc], _Txn, _Cmd, _Pipes) ->
    case ecli_types:completions(Type) of
        [] ->
            {yes, "", ["\r\n", Desc, "\r\n"]};
        Tree ->
            Menu = ecli:format_menu(Tree),
            {yes, "", Menu}
    end;
expand_after_space(_Menu, [#{node_type := leaf, desc := Desc} | _Acc], _Txn, _Cmd, _Pipes) ->
    {yes, "", ["\r\n", Desc, "\r\n"]};
expand_after_space([#{name := OneName} = Item], Acc, Txn, Cmd, Pipes) ->
    case has_pipes(Pipes) of
        true ->
            {yes, "", ecli:format_menu(with_pipe([Item], Acc, Pipes))};
        false ->
            Children = menu_item_children(Item, Txn, Cmd),
            Menu = ecli:format_menu(Children),
            {yes, OneName ++ " ", Menu}
    end;
expand_after_space([#cmd{name = OneName} = Item], Acc, Txn, Cmd, Pipes) ->
    case has_pipes(Pipes) of
        true ->
            {yes, "", ecli:format_menu(with_pipe([Item], Acc, Pipes))};
        false ->
            Children = menu_item_children(Item, Txn, Cmd),
            Menu = ecli:format_menu(Children),
            {yes, OneName ++ " ", Menu}
    end;
expand_after_space(Tree, Acc, _Txn, _Cmd, Pipes) ->
    ?DBG("ecli_expand_after_space: fallthorgh = ~p~n",[Tree]),
    Menu = ecli:format_menu(with_pipe(Tree, Acc, Pipes)),
    {yes, "", Menu}.

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

normalise([{string, S} | Ts]) ->
    [{token, S} | normalise(Ts)];
normalise([T | Ts]) ->
    [T | normalise(Ts)];
normalise([]) ->
    [].

has_pipes(P) when is_list(P), P =/= [] ->
    true;
has_pipes(_) ->
    false.

update_pipes(#{pipes := P}, _Prev) when P =/= undefined ->
    ecli_pipe:catalog(P);
update_pipes(_, Prev) ->
    Prev.

pipe_menu_item() ->
    #{name => "|", desc => "Pipe through a command"}.

offer_pipe([], _Pipes) ->
    false;
offer_pipe([#{node_type := leaf, value := _} | _], Pipes) ->
    has_pipes(Pipes);
offer_pipe([#{node_type := leaf} | _], _Pipes) ->
    false;
offer_pipe(_Acc, Pipes) ->
    has_pipes(Pipes).

with_pipe(Items, Acc, Pipes) ->
    case offer_pipe(Acc, Pipes) of
        true ->
            Items ++ [pipe_menu_item()];
        false ->
            Items
    end.




