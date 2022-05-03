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
    ?DBG("lookup ~p~n Tree: ~p~n Txn:~p~n",[Str, Tree, Txn]),
    case ecli_tokenise:string(Str) of
        {ok, Tokens} ->
            ?DBG("tokens ~p~n",[Tokens]),
            parse(Tokens, Tree, [], Txn);
        no ->
            {error, "Command not understood"}
    end.

parse([], _Tree, Acc, _Txn) ->
    {Cmd, Items} = lists:splitwith(fun(#{role := Role}) -> Role == cmd end, lists:reverse(Acc)),
    %% io:format(user, "Looked up Cmd = ~p~nItems = ~p~n", [Cmd, Items]),
    {ok, Cmd, Items};
parse([{part_string, _Str}], _Tree, _Acc, _Txn) ->
    %% Nothing we can do here, ending in an incomplete quotes delimited string
    {error, "Command not understood"};
parse([{token, ""} | _Ts], _Tree, [], _Txn) ->
    %% Completely empty command, this is fine
    {error, ""};
parse([{token, Tok} | Ts], Tree, [], Txn) ->
    %% A token at the start. Expect it to match a container in the initial Tree
    case lookup(Tok, Tree) of
        {ok, #{node_type := container} = Item} ->
            Children = ecli_util:children(Item, Txn, undefined),
            %% io:format(user, "Starting with Item = ~p~n", [Item]),
            parse(Ts, Children, [Item], Txn);
        {ok, #{node_type := leaf} = Item} ->
            %% io:format(user, "Starting with Item = ~p~n", [Item]),
            parse(Ts, [], [Item], Txn);
        false ->
            %% Oops, end of the line
            {error, "Command not understood"}
    end;
parse([{token, Tok} | Ts], Tree, [#{node_type := container} | _] = Acc, Txn) ->
    %% A token after a container. Expect it to match an entry in the Tree
    case lookup(Tok, Tree) of
        {ok, #{node_type := container} = Item} ->
            Children = ecli_util:children(Item, Txn, undefined),
            %% io:format(user, "Adding Container Item in container = ~p~n", [Item]),
            parse(Ts, Children, [Item | Acc], Txn);
        {ok, #{node_type := Leaf} = Item} when Leaf == leaf; Leaf == leaf_list ->
            %% Expecting a leaf value, possibly followed by more entries in the same
            %% container. Keep the same possible future tree, but without this node
            Tree1 = remove(Tok, Tree),
            %% io:format(user, "Adding Leaf Item in container = ~p~n", [Item]),
            parse(Ts, Tree1, [Item | Acc], Txn);
        {ok, #{node_type := list} = Item} ->
            %% Expecting a key value next. Children could be all entries in the list..
            %% so ignore and deal with if we need completion
            parse(Ts, [], [Item | Acc], Txn);
        false ->
            %% Oops, end of the line
            {error, "Command not understood"}
    end;
parse([{token, Tok} | Ts], Tree, [#{node_type := leaf, value := _Value} | _] = Acc, Txn) ->
    %% Token after a leaf that already has a value can only be another leaf in some multi leaf container setup.
    %% io:format(user, "Setting leaf value in container = ~p ~p ~n", [Leaf, Container]),
    case lookup(Tok, Tree) of
        {ok, #{node_type := Leaf} = Item} when Leaf == leaf; Leaf == leaf_list ->
            %% Expecting a leaf value, possibly followed by more entries in the same
            %% container. Keep the same possible future tree, but without this node
            Tree1 = remove(Tok, Tree),
            %% io:format(user, "Adding Leaf Item in container = ~p~n", [Item]),
            parse(Ts, Tree1, [Item | Acc], Txn);
        false ->
            %% Oops, end of the line
            {error, "Unkown parameter"}
    end;
parse([{token, Tok} | Ts], Tree, [#{node_type := leaf, type := Type} = Leaf | Acc], Txn) ->
    %% Token after a leaf. This is the value. Just put it it in the leaf
    %% io:format(user, "Setting leaf value = ~p~n", [Leaf]),
    case parse_value(Type, Tok) of
        {error, _Reason} = Err ->
            Err;
        Value ->
            Leaf1 = Leaf#{value => Value},
            parse(Ts, Tree, [Leaf1 | Acc], Txn)
    end;
parse([{token, _Tok} | Ts], _Tree, [#{node_type := leaf} | Acc], Txn) ->
    %% Token after a leaf without a type. i.e. a command type node.
    %% This might be a command modifier of some sort in the future
    %% io:format(user, "Setting leaf value = ~p~n", [Leaf]),
    parse(Ts, [], Acc, Txn);
parse([space | Ts], Tree, Acc, Txn) ->
    %% Spaces not relevant outside completion
    %% io:format(user, "Skipping space~n", []),
    parse(Ts, Tree, Acc, Txn).

parse_value(integer, Token) -> parse_integer(Token);
parse_value(string, Token) -> Token;
parse_value(boolean, "true") -> true;
parse_value(boolean, "false") -> false;
parse_value({Mod, Type}, Token) when is_atom(Type) ->
    case Mod:parse_value(Type, Token) of
        {ok, Value} -> Value;
        {error, _Err} = Err ->
            Err
    end;
parse_value(Type, Token) ->
    io:format("CLI unsupported leaf type: ~p\n", [Type]),
    Token.

parse_integer(Token) ->
    case catch list_to_integer(Token) of
        {'EXIT', _} ->
            {error, "Expected an integer value"};
        Int ->
            Int
    end.

lookup(Name, [#{name := Name} = Item | _Tree]) ->
    {ok, Item};
lookup(Name, [#{} | Tree]) ->
    lookup(Name, Tree);
lookup(_, []) ->
    false.

remove(Tok, Tree) ->
    lists:filter(fun(#{name := Name}) -> Name /= Tok end, Tree).