%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Looking up node in a tree from a command string
%%%      Similar functionality to ecli_expand but just a bit messy to
%%%      combine them
%%%
%%% @end
%%% Created : 18 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(ecli_lookup).

-include("debug.hrl").

-export([lookup/3]).

%% @doc Given a string in the form of a CLI command or path
%% e.g. "set interface ge/0/0/0 enable true" and a schema tree, parse
%% the string validating each level against the schema.
%%
%% Returns {error, Reason} if any part of the string does not match
%% the schema.
%%
%% Returns {ok, Items, Tail} on a match, where Items are a list of
%% schema nodes for the full path to the node matched, and Tail is the
%% string following the final schema node if any.

lookup(Str, Tree, Txn) ->
    %% ?DBG("lookup ~p~n Tree: ~p~n Txn:~p~n",[Str, Tree, Txn]),
    lookup(Str, Tree, Txn, []).

lookup(Str, Tree, Txn, Acc) ->
    case next_element(Str) of
        {[],_} ->
            {error, "Unknown no token"};
        {PathPart, []} ->
            case lookup_by_name(PathPart, Tree) of
                {ok, Item} ->
                    return([Item|Acc], "");
                {error, _} = Err ->
                   Err
            end;
        {PathPart, Tail} ->
            case lookup_by_name(PathPart, Tree) of
                {ok, #{node_type := NodeType} = Item} ->
                    case NodeType of
                        _ when NodeType == leaf orelse NodeType == leaf_list ->
                            return([Item|Acc], Tail);
                        list ->
                            %% For a list item the following N items
                            %% will make up the list keys and need to
                            %% be put in the path. List schema items
                            %% are required to have list_keys and
                            %% list_values entries, which we can use
                            #{key_names := KeyNames} = Item,
                            %% ?DBG("KEY NAMES ~p~n",[KeyNames]),
                            %% The length of KeyNames tells us how
                            %% many command parts to fetch to make up
                            %% the list key
                            {KeyValues, T} = parse_list_keys(Tail, length(KeyNames)),
                            ListItem = Item#{key_values := KeyValues},
                            Children = ecli_util:children(ListItem, Txn, undef),
                            lookup(T, Children, Txn, [ListItem|Acc]);
                        _ ->
                            Children = ecli_util:children(Item, Txn, undef),
                            lookup(Tail, Children, Txn, [Item|Acc])
                    end;
                {error, _} = Err ->
                    Err
            end
    end.

parse_list_keys(Str, N) ->
    parse_list_keys(Str, N, []).

parse_list_keys(Str, 0, Acc) ->
    {lists:reverse(Acc), Str};
parse_list_keys("", _N, Acc) ->
    {lists:reverse(Acc), ""};
parse_list_keys(Str, N, Acc) ->
    {Next, Tail} = next_element(Str),
    parse_list_keys(Tail, N-1, [Next|Acc]).


lookup_by_name(Name, [#{name := Name} = TreeItem |_]) ->
    {ok, TreeItem};
lookup_by_name(Name, [_ | Tree]) ->
    lookup_by_name(Name, Tree);
lookup_by_name(_, []) ->
    {error, "No such command"}.

return(PathItems, Tail) ->
    {Cmd, Items} = lists:splitwith(fun(#{role := Role}) -> Role == cmd end, lists:reverse(PathItems)),
    {ok, Cmd, Items, Tail}.

next_element(Str) ->
    Trimmed = string:trim(Str, leading),
    next_element(Trimmed, []).

next_element([$\s|Cs], Acc) ->
    {lists:reverse(Acc), string:trim(Cs, leading)};
next_element([C|Cs], Acc) ->
    next_element(Cs, [C|Acc]);
next_element([], Acc) ->
    {lists:reverse(Acc), []}.

