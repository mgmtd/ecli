%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Useful common routines
%%%
%%% @end
%%% Created : 17 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cli_util).

-include("cli.hrl").
-include("debug.hrl").

-export([children/3, expand_children/1]).

-export([strip_ws/1]).


%% Get the actual children from a tree item
children(#{node_type := container, name := Name, children := Cs} = Item,
         Txn, _CmdType) ->
    ?DBG("container children~p~n", [Cs]),
    Children = expand_children(Cs),
    ?DBG("container expanded children~p~n", [Children]),
    case Item of
        #{path := Path} ->
            insert_full_path(Children, Path ++ [Name]);
        _ ->
            insert_full_path(Children, [Name])
    end;
children(#{node_type := List, path := Path, name := Name, children := Cs,
           key_names := KeyNames, key_values := KeyValues} = S,
         Txn, CmdType) when List == list orelse
                            List == new_list_item ->
    %% Children for list items are the list keys plus maybe a wildcard
    %% if it's a set command and we want to allow adding a new list
    %% item (indicated by AddListItems = true).

    %% If it has a compound key which one depends on how far we got in
    %% gathering the list keys. We can abuse the key_values field to
    %% track how many list keys we have, and re-use the same
    %% #cfg_schema{} list item for all the list item "children" so we
    %% still have it around for the real children.
    KeysSoFar = length(KeyValues),
    KeysNeeded = length(KeyNames),
    if KeysSoFar == KeysNeeded ->
            ?DBG("all keys needed~n"),
            %% Now we have all the keys return the real child list.
            %% FIXME - remove the list keys from this list
            Children = expand_children(Cs),
            Filtered = filter_list_key_leafs(Children, KeyNames),
            insert_full_path(Filtered, Path ++ [Name]);
       true ->
            %% First time: Needed = 2, SoFar == 0, element = 1
            %% 2nd time:   Needed = 2, SoFar = 1, element = 2
            %% Last time:  Needed = 2, SoFar = 2
            NextKey = lists:nth(KeysSoFar + 1, KeyNames),
            Keys = [NextKey | KeyValues],
            ?DBG("more keys needed ~p ~p~n", [NextKey, KeyValues]),

            Template = S#{key_values => Keys},
            ListKeys = cfg_txn:list_keys(Txn, Path ++ [Name]),
            %% This is all the keys. We need to only show the current
            %% level, only unique values, and only items where the
            %% previous key parts match
            ?DBG("ListKeys ~p ~p~n", [Path ++ [Name], ListKeys]),
            %%
            %% We don't have all this: the path isn't filled in, and
            %% we don't have the previous key values
            %% FIXME: Fill the path in
            %% FIXME: include previous key parts somewhere

            KeysItems = lists:map(
                          fun(K) ->
                                  KName = element(KeysSoFar + 1, K),
                                  Template#{name => KName}
                          end, ListKeys),

            %% The goal here is to return the set of possible values
            %% at this point, plus something that will prompt for a
            %% new list item. We need to just convince the menu thingy
            %% we are a normal list of children, and we need to keep
            %% enough blah around so we can carry on afterwards
            case CmdType of
                set ->
                    [Template#{node_type => new_list_item} | KeysItems];
                _ ->
                    KeysItems
            end
    end;
children(_, _, _) ->
    [].

expand_children(F) when is_function(F) -> F();
expand_children(L) when is_list(L) -> L;
expand_children(_) -> [].

insert_full_path(Children, Path) ->
    lists:map(fun(S) ->
                      S#{path => Path}
              end, Children).

filter_list_key_leafs(Children, KeyNames) ->
    lists:filter(fun(#{name := Name}) ->
                         not lists:member(Name, KeyNames)
                 end, Children).

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


