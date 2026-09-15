%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Useful common routines
%%%
%%% @end
%%% Created : 17 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(ecli_util).

-include("ecli_internal.hrl").
-include("../include/ecli.hrl").

-export([children/3, expand_children/2]).
-export([strip_ws/1]).
-export([cmd_to_map/1]).

%% Get the children of a tree item

children(#cmd{} = Cmd, Txn, CmdType) ->
    Item = cmd_to_map(Cmd),
    children(Item, Txn, CmdType);
children(#{node_type := container, children := Cs} = Item, _Txn, _CmdType) ->
    Path = maps:get(path, Item, []),
    Children = expand_children(Cs, Path),
    insert_full_path(Children, Path);
children(#{node_type := list,
           path := Path,
           children := Cs,
           data_callback := DataCallbackMod,
           key_names := KeyNames,
           key_values := KeyValues} =
             Item,
         Txn,
         CmdType) ->
    ?DBG("list children at path ~p with name ~p key_names ~p key_values ~p~n", [Path, Name, KeyNames, KeyValues]),
    %% Children for list items are the list keys plus maybe a wildcard
    %% if it's a set command and we want to allow adding a new list
    %% item (indicated by CmdType = set).
    %% If it has a compound key which one depends on how far we got in
    %% gathering the list keys. We can abuse the key_values field to
    %% track how many list keys we have, and re-use the same
    %% #cfg_schema{} list item for all the list item "children" so we
    %% still have it around for the real children.
    KeysSoFar = length(KeyValues),
    KeysNeeded = length(KeyNames),
    EffectiveCmd = effective_cmd(CmdType, Item),
    if KeysSoFar == KeysNeeded ->
            case {EffectiveCmd, maps:get(ordered_by, Item, system)} of
                {move, user} ->
                    where_menu(Item, Txn);
                _ ->
                    %% Now we have all the keys return the child nodes of the list.
                    FullPath = Path ++ [list_to_tuple(KeyValues)],
                    ?DBG("Full list path ~p~n", [FullPath]),
                    Children = expand_children(Cs, FullPath),
                    ?DBG("Children ~p~n", [Children]),
                    Filtered = filter_list_key_leafs(Children, KeyNames),
                    insert_full_path(Filtered, FullPath)
            end;
       true ->
            %% First time: Needed = 2, SoFar == 0, element = 1
            %% 2nd time:   Needed = 2, SoFar = 1, element = 2
            %% Last time:  Needed = 2, SoFar = 2
            NextKey = lists:nth(KeysSoFar + 1, KeyNames),
            _Keys = [NextKey | KeyValues],
            ?DBG("more keys needed ~p ~p ~p~n", [CmdType, NextKey, KeyValues]),
            ListKeyMatch = list_keys_match(KeysSoFar, KeysNeeded, KeyValues),
            ListItemPath =
                case KeysSoFar of
                    0 ->
                        Path;
                    _ ->
                        Path
                end,
            ListKeys = DataCallbackMod:list_keys(Txn, ListItemPath, ListKeyMatch),
            %% This is not yet all the keys. We need to only pick the current
            %% level, only unique values, and only items where the
            %% previous key parts match
            ?DBG("ListKeys ~p ~p~n", [ListItemPath, ListKeys]),
            %%
            %% We don't have all this: the path isn't filled in, and
            %% we don't have the previous key values
            %% FIXME: Fill the path in
            %% FIXME: include previous key parts somewhere
            %% First time: show uniq values of first list key. key_values is []
            %% Second time - show uniq second values where the first value matches key_values
            Template = Item#{path => ListItemPath},

            KeysItems =
                lists:map(fun(K) ->
                                  Template#{name => K,
                                            node_type => list_key,
                                            key_values => KeyValues}
                          end,
                          ListKeys),
            %% The goal here is to return the set of possible values
            %% at this point, plus something that will prompt for a
            %% new list item. We need to just convince the menu thingy
            %% we are a normal list of children, and we need to keep
            %% enough blah around so we can carry on afterwards
            case EffectiveCmd of
                set ->
                    [Template#{node_type => new_list_item} | KeysItems];
                _ ->
                    ?DBG("ListKeys Items ~p~n", [KeysItems]),
                    KeysItems
            end
    end;
children(_Item, _, _) ->
    ?DBG("list children falltrhough ~p~n", [_Item]),
    [].

effective_cmd(undefined, Item) ->
    maps:get(cmd_type, Item, undefined);
effective_cmd(CmdType, _Item) ->
    CmdType.

%% After an `ordered-by user` list instance, `move` takes a position
%% instead of descending into the item's leaves.
where_menu(#{path := Path, key_values := KeyValues,
             key_names := KeyNames} = Item, Txn) ->
    Self = case KeyValues of
               [Tok] -> Tok;
               _ -> list_to_tuple(KeyValues)
           end,
    Mod = maps:get(data_callback, Item, undefined),
    Match = list_keys_match(0, length(KeyNames), []),
    Raw = case Mod of
              undefined -> [];
              _ ->
                  try Mod:list_keys(Txn, Path, Match) of
                      Keys when is_list(Keys) -> Keys;
                      _ -> []
                  catch
                      _:_ -> []
                  end
          end,
    Others = [K || K <- Raw, K =/= Self],
    Cmds = [first_pos(), last_pos(),
            before_pos(Others), after_pos(Others)],
    insert_full_path([cmd_to_map(C) || C <- Cmds], Path).

first_pos() ->
    #cmd{name = "first",
         desc = "Move to the start of the list",
         action = {move, first}}.

last_pos() ->
    #cmd{name = "last",
         desc = "Move to the end of the list",
         action = {move, last}}.

before_pos(Keys) ->
    #cmd{name = "before",
         desc = "Move before an existing list entry",
         children = fun() -> sibling_cmds(before, Keys) end}.

after_pos(Keys) ->
    #cmd{name = "after",
         desc = "Move after an existing list entry",
         children = fun() -> sibling_cmds('after', Keys) end}.

sibling_cmds(Side, Keys) ->
    [#cmd{name = key_token(K),
          desc = "Existing list entry",
          action = {move, {Side, point_key(K)}}}
     || K <- Keys].

point_key(K) when is_tuple(K) ->
    K;
point_key(K) when is_list(K) ->
    {K}.

key_token({S}) when is_list(S) ->
    S;
key_token(T) when is_tuple(T) ->
    lists:flatten(lists:join(" ", [fmt_tok(X) || X <- tuple_to_list(T)]));
key_token(S) when is_list(S) ->
    S.

fmt_tok(S) when is_list(S) -> S;
fmt_tok(N) when is_integer(N) -> integer_to_list(N);
fmt_tok(X) -> lists:flatten(io_lib:format("~p", [X])).

expand_children(F, Path) when is_function(F) ->
    ?DBG("expand children ~p~n", [Path]),
    Children = case erlang:fun_info(F, arity) of
                   {arity, 0} ->
                       F();
                   {arity, 1} ->
                       F(Path)
               end,
    lists:map(fun(#cmd{} = C) -> cmd_to_map(C);
                 (C) -> C
              end, Children);
expand_children(L, _) when is_list(L) ->
    L;
expand_children(_, _) ->
    [].

list_keys_match(Needed, Needed, Keys) ->
    list_to_tuple(Keys);
list_keys_match(SoFar, Needed, Keys) ->
    %% Create list like ["Key1", "Key2", '$1', '_','_']
    Pattern = Keys ++ ['$1'] ++ lists:duplicate(Needed - SoFar - 1, '_'),
    list_to_tuple(Pattern).

insert_full_path(Children, Path) ->
    lists:map(fun(#{role := cmd} = S) -> S#{path => Path};
                 (#{name := Name} = S) -> S#{path => Path ++ [Name]}
              end, Children).

filter_list_key_leafs(Children, KeyNames) ->
    lists:filter(fun(#{name := Name}) -> not lists:member(Name, KeyNames) end, Children).

%% @doc strip spaces and tabs from the start of a listy string.
-spec strip_ws(string()) -> string().
strip_ws([$\s | Str]) ->
    strip_ws(Str);
strip_ws([$\t | Str]) ->
    strip_ws(Str);
strip_ws([]) ->
    "";
strip_ws(Str) ->
    Str.

cmd_to_map(#cmd{name = Name,
                desc = Desc,
                action = Action,
                children = Children,
                pipes = Pipes,
                access = Access}) ->
    #{role => cmd,
      node_type => container,
      name => Name,
      desc => Desc,
      action => Action,
      children => Children,
      pipes => Pipes,
      access => Access}.
