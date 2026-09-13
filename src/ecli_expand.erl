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
parse_leaf([space], _EnumValues, _Tree, #{node_type := NT} = Leaf, Acc, Txn, _Cmd, _Pipes)
  when NT =:= leaf; NT =:= leaf_list ->
    {yes, "", leaf_value_prompt(Leaf, Acc, Txn)};
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
            Pipes1 = update_pipes(ecli_util:cmd_to_map(Item), Pipes),
            Menu = prompt_after_item(ecli_util:cmd_to_map(Item), Acc, Txn, Cmd, Pipes1),
            {yes, " ", Menu};
        [#{name := Name} = Item] when Name == Tok ->
            Pipes1 = update_pipes(Item, Pipes),
            Menu = prompt_after_item(Item, Acc, Txn, Cmd, Pipes1),
            {yes, " ", Menu};
        [#cmd{name = Name}] ->
            {yes, chars_to_expand(Tok, Name), []};
        [#{name := Name} = Item] ->
            {yes, chars_to_expand(Tok, Name), leaf_name_help(Item, Acc, Txn)};
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
expand_after_space(_Menu, [#{node_type := NT} = Leaf | Acc], Txn, _Cmd, _Pipes)
  when NT =:= leaf; NT =:= leaf_list ->
    {yes, "", leaf_value_prompt(Leaf, Acc, Txn)};
expand_after_space([#{name := OneName} = Item], Acc, Txn, Cmd, Pipes) ->
    case has_pipes(Pipes) of
        true ->
            {yes, "", ecli:format_menu(with_pipe([Item], Acc, Pipes))};
        false ->
            {yes, OneName ++ " ", prompt_after_item(Item, Acc, Txn, Cmd, Pipes)}
    end;
expand_after_space([#cmd{name = OneName} = Item], Acc, Txn, Cmd, Pipes) ->
    Map = ecli_util:cmd_to_map(Item),
    case has_pipes(Pipes) of
        true ->
            {yes, "", ecli:format_menu(with_pipe([Item], Acc, Pipes))};
        false ->
            {yes, OneName ++ " ", prompt_after_item(Map, Acc, Txn, Cmd, Pipes)}
    end;
expand_after_space(Tree, Acc, _Txn, _Cmd, Pipes) ->
    ?DBG("ecli_expand_after_space: fallthorgh = ~p~n",[Tree]),
    Menu = ecli:format_menu(with_pipe(Tree, Acc, Pipes)),
    {yes, "", Menu}.

filter_by_prefix(_Str, []) ->
    [];
filter_by_prefix(Str, Menu) ->
    lists:filter(fun(Item) -> lists:prefix(Str, item_name(Item)) end, Menu).

item_name(#{name := Name}) when is_list(Name) -> Name;
item_name(#cmd{name = Name}) -> Name;
item_name(_) -> "".

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

%% After a fully matched node: containers show children, leaves show
%% the leaf name, description, and any stored value.
prompt_after_item(#{node_type := NT} = Item, Acc, Txn, _Cmd, _Pipes)
  when NT =:= leaf; NT =:= leaf_list ->
    leaf_value_prompt(Item, Acc, Txn);
prompt_after_item(Item, Acc, Txn, Cmd, Pipes) ->
    Children = menu_item_children(Item, Txn, Cmd),
    ecli:format_menu(with_pipe(Children, [Item | Acc], Pipes)).

leaf_name_help(#{node_type := NT} = Item, Acc, Txn)
  when NT =:= leaf; NT =:= leaf_list ->
    leaf_value_prompt(Item, Acc, Txn);
leaf_name_help(_, _, _) ->
    [].

%% Prompt shown once the leaf name is complete and the user is about
%% to type a value: leaf name, description, [existing] if any, then
%% enum / boolean choices.
leaf_value_prompt(Leaf, Acc, Txn) ->
    Desc0 = maps:get(desc, Leaf, ""),
    Desc = case existing_value(Leaf, Acc, Txn) of
               undefined ->
                   Desc0;
               Val ->
                   ValStr = fmt_existing(Leaf, Val),
                   case Desc0 of
                       "" -> "[" ++ ValStr ++ "]";
                       _ -> Desc0 ++ " [" ++ ValStr ++ "]"
                   end
           end,
    Type = maps:get(type, Leaf, undefined),
    ecli:format_menu([Leaf#{desc => Desc} | ecli_types:completions(Type)]).

existing_value(#{role := cmd}, _Acc, _Txn) ->
    undefined;
existing_value(_Leaf, _Acc, Txn) when Txn =:= undefined; Txn =:= no_txn ->
    undefined;
existing_value(Leaf, Acc, Txn) ->
    case callback_mod([Leaf | Acc]) of
        undefined ->
            undefined;
        Mod ->
            Path = data_path([Leaf | Acc]),
            try Mod:get_value(Txn, Path) of
                {ok, undefined} -> undefined;
                {ok, Val} -> Val;
                _ -> undefined
            catch
                _:_ -> undefined
            end
    end.

callback_mod([#{data_callback := Mod} | _]) when Mod =/= undefined ->
    Mod;
callback_mod([_ | Rest]) ->
    callback_mod(Rest);
callback_mod([]) ->
    undefined.

data_path(Acc) ->
    lists:flatmap(fun path_elem/1, lists:reverse(Acc)).

path_elem(#{role := cmd}) ->
    [];
path_elem(#{node_type := list, name := Name, key_values := KVs})
  when is_list(KVs), KVs =/= [] ->
    [Name, list_to_tuple(KVs)];
path_elem(#{name := Name}) ->
    [Name];
path_elem(_) ->
    [].

fmt_existing(#{node_type := leaf_list}, Vals) when is_list(Vals) ->
    lists:flatten(lists:join(" ", [fmt_scalar(V) || V <- Vals]));
fmt_existing(_Leaf, Val) ->
    fmt_scalar(Val).

fmt_scalar(Bin) when is_binary(Bin) ->
    binary_to_list(Bin);
fmt_scalar(Int) when is_integer(Int) ->
    integer_to_list(Int);
fmt_scalar(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
fmt_scalar({A, B, C, D})
  when is_integer(A), is_integer(B), is_integer(C), is_integer(D) ->
    lists:flatten(io_lib:format("~p.~p.~p.~p", [A, B, C, D]));
fmt_scalar(List) when is_list(List) ->
    case io_lib:printable_unicode_list(List) of
        true -> List;
        false -> lists:flatten(io_lib:format("~p", [List]))
    end;
fmt_scalar(Else) ->
    lists:flatten(io_lib:format("~p", [Else])).

%% Find characters to add to fill up to where the node names diverge
%% e.g. names configure and contain given an input of "c" should return "on"
expand_menus(Str, Menus) ->
    %% ?DBG("ecli_expand:expand_menus ~p~n ~p ~n",[Str, Menus]),
    StrLen = length(Str),
    Suffixes = [lists:nthtail(StrLen, item_name(I)) || I <- Menus],
    %% ?DBG("expand_menus ML = ~p~n",[Suffixes]),
    longest_common_prefix(Suffixes).

-spec longest_common_prefix([string()]) -> string().
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




