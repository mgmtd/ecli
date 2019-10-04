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

-export([lookup/3]).

%% @doc Given a string in the form of a CLI command or path
%% e.g. "set interface ge/0/0/0 enable true" and a schema tree, parse
%% the string validating each level against the schema.
%%
%% Returns {error, Reason} if any part of the string does not match
%% the schema.
%%
%% Returns {ok, Item, Tail} on a match, where Item is the final schema
%% node matched, and Tail is the string following the final schema
%% node if any.

lookup(Str, Tree, Getters) ->
    lookup(Str, Tree, Getters, undefined).

lookup(Str, Tree, Getters, Cmd) ->
    case next_token(Str) of
        {[],_} ->
            {error, "Unknown no token"};
        {PathPart, []} ->
            case lookup_by_name(PathPart, Tree, Getters) of
                {ok, Item} ->
                    return(Cmd, Item, "");
                {error, _} = Err ->
                   Err
            end;
        {PathPart, Tail} ->
            case lookup_by_name(PathPart, Tree, Getters) of
                {ok, Item} ->
                    NodeType = cli_util:get_node_type(Getters, Item),
                    case NodeType of
                        _ when NodeType == leaf orelse NodeType == leaf_list ->
                            return(Cmd, Item, Tail);
                        _ ->
                            ChildSpec = cli_util:get_children(Getters, Item),
                            {Children, NewGetters} =
                                case cli_util:eval_childspec(ChildSpec) of
                                    {Cs, Gs} ->
                                        {Cs, Gs};
                                    Cs ->
                                        {Cs, Getters}
                                end,
                            if Cmd == undefined ->
                                    lookup(Tail, Children, NewGetters, Item);
                               true ->
                                    lookup(Tail, Children, NewGetters, Cmd)
                            end
                    end;
                {error, _} = Err ->
                    Err
            end
    end.

lookup_by_name(Name, [TreeItem|Tree], Getters) ->
    case cli_util:get_name(Getters, TreeItem) of
        Name ->
            {ok, TreeItem};
        _ ->
            lookup_by_name(Name, Tree, Getters)
    end;
lookup_by_name(_, [], _) ->
    {error, "No such command"}.

return(Cmd, Item, Tail) ->
    if Cmd == undefined ->
            %% Assume the first node encountered is
            %% the command (i.e. show|set etc)
            {ok, Item, undefined, Tail};
       true ->
            {ok, Cmd, Item, Tail}
    end.

next_token(Str) ->
    Trimmed = string:trim(Str, leading),
    next_token(Trimmed, []).

next_token([$\s|Cs], Acc) ->
    {lists:reverse(Acc), string:trim(Cs, leading)};
next_token([C|Cs], Acc) ->
    next_token(Cs, [C|Acc]);
next_token([], Acc) ->
    {lists:reverse(Acc), []}.

