%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Looking up node in a tree from a command string
%%%      Similar functionality to cli_expand but just a bit messy to
%%%      combine them
%%%
%%% @end
%%% Created : 18 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cli_lookup).

-include("debug.hrl").

-export([lookup/4]).

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

lookup(Str, Tree, Accessors, Txn) ->
    %% ?DBG("lookup ~p~n Tree: ~p~n Accessors:~p~n Txn:~p~n",[Str, Tree, Accessors, Txn]),
    lookup(Str, Tree, Accessors, Txn, undefined, []).

lookup(Str, Tree, Accessors, Txn, Cmd, Acc) ->
    case next_element(Str) of
        {[],_} ->
            {error, "Unknown no token"};
        {PathPart, []} ->
            case lookup_by_name(PathPart, Tree, Accessors) of
                {ok, Item} ->
                    return(Cmd, [Item|Acc], "");
                {error, _} = Err ->
                   Err
            end;
        {PathPart, Tail} ->
            case lookup_by_name(PathPart, Tree, Accessors) of
                {ok, Item} ->
                    NodeType = cli_util:get_node_type(Accessors, Item),
                    case NodeType of
                        _ when NodeType == leaf orelse NodeType == leaf_list ->
                            return(Cmd, [Item|Acc], Tail);
                        list ->
                            %% For a list item the following N items
                            %% will make up the list keys and need to
                            %% be put in the path. List schema items
                            %% are required to have list_keys and
                            %% list_values entries, which we can use
                            KeyNames = cli_util:get_list_key_names(Accessors,
                                                                   Item),
                            ?DBG("KEY NAMES ~p~n",[KeyNames]),
                            %% The length of KeyNames tells us how
                            %% many command parts to fetch to make up
                            %% the list key
                            {KeyValues, T} = parse_list_keys(Tail, length(KeyNames)),
                            ListItem = cli_util:set_list_key_values(Accessors, Item, KeyValues),
                            ChildSpec = cli_util:get_children(Accessors, ListItem, Txn),
                            {Children, NewAccessors, _AddingListItem} =
                                case cli_util:eval_childspec(ChildSpec) of
                                    {Cs, Gs, AddingListItem1} ->
                                        {Cs, Gs, AddingListItem1};
                                    Cs ->
                                        {Cs, Accessors, false}
                                end,
                            if Cmd == undefined ->
                                    lookup(T, Children, NewAccessors, Txn, [ListItem], Acc);
                               true ->
                                    lookup(T, Children, NewAccessors, Txn, Cmd, [ListItem|Acc])
                            end;
                        _ ->
                            ChildSpec = cli_util:get_children(Accessors, Item, Txn),
                            {Children, NewAccessors, _AddingListItem} =
                                case cli_util:eval_childspec(ChildSpec) of
                                    {Cs, Gs, AddingListItem1} ->
                                        {Cs, Gs, AddingListItem1};
                                    Cs ->
                                        {Cs, Accessors, false}
                                end,
                            if Cmd == undefined ->
                                    lookup(Tail, Children, NewAccessors, Txn, [Item], Acc);
                               true ->
                                    lookup(Tail, Children, NewAccessors, Txn, Cmd, [Item|Acc])
                            end
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


lookup_by_name(Name, [TreeItem|Tree], Accessors) ->
    case cli_util:get_name(Accessors, TreeItem) of
        Name ->
            {ok, TreeItem};
        _ ->
            lookup_by_name(Name, Tree, Accessors)
    end;
lookup_by_name(_, [], _) ->
    {error, "No such command"}.

return(Cmd, Items, Tail) ->
    if Cmd == undefined ->
            %% Assume the first nodes encountered are
            %% the command (i.e. show|set etc)
            {ok, Items, undefined, Tail};
       true ->
            {ok, Cmd, lists:reverse(Items), Tail}
    end.

next_element(Str) ->
    Trimmed = string:trim(Str, leading),
    next_element(Trimmed, []).

next_element([$\s|Cs], Acc) ->
    {lists:reverse(Acc), string:trim(Cs, leading)};
next_element([C|Cs], Acc) ->
    next_element(Cs, [C|Acc]);
next_element([], Acc) ->
    {lists:reverse(Acc), []}.

