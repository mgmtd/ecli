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

-include("ecli_internal.hrl").
-include("../include/ecli.hrl").

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
    ?DBG("Looked up Cmd = ~p~nItems = ~p~n", [Cmd, Items]),
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
        {ok, #cmd{} = CmdItem} ->
            Item = ecli_util:cmd_to_map(CmdItem),
            Children = ecli_util:children(Item, Txn, undefined),
            %% io:format(user, "Starting with Item = ~p~n", [Item]),
            parse(Ts, Children, [Item], Txn);
        {ok, #{node_type := container} = Item} ->
            %% io:format(user, "Starting with Item = ~p~n", [Item]),
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
parse([{token, Tok} | Ts], Tree, [#{node_type := NodeType} | _] = Acc, Txn) when NodeType == container; NodeType == list ->
    %% A token after a container. Expect it to match an entry in the Tree
    case lookup(Tok, Tree) of
        {ok, #cmd{} = CmdItem} ->
            Item = ecli_util:cmd_to_map(CmdItem),
            Children = ecli_util:children(Item, Txn, undefined),
            %% io:format(user, "Starting with Item = ~p~n", [Item]),
            parse(Ts, Children, [Item | Acc], Txn);
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
            parse_list_keys(Ts, Item, Acc, Txn);
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
parse([{token, Tok} | Ts], Tree, [#{node_type := leaf} = Leaf | Acc], Txn) ->
    %% Token after a leaf. This is the value. Just put it it in the leaf
    %% io:format(user, "Setting leaf value = ~p~n", [Leaf]),
    case parse_value(Leaf, Tok) of
        {error, _Reason} = Err ->
            Err;
        Value ->
            Leaf1 = Leaf#{value => Value},
            parse(Ts, Tree, [Leaf1 | Acc], Txn)
    end;
parse([space | Ts], Tree, Acc, Txn) ->
    %% Spaces not relevant outside completion
    %% io:format(user, "Skipping space~n", []),
    parse(Ts, Tree, Acc, Txn).

parse_value(#{type := uint64, range := Range}, Token) ->
    FullRange = merge_ranges(uint64_range(), Range),
    parse_integer(Token, FullRange);
parse_value(#{type := uint64}, Token) -> parse_integer(Token, uint64_range());

parse_value(#{type := uint32, range := Range}, Token) ->
    FullRange = merge_ranges(uint32_range(), Range),
    parse_integer(Token, FullRange);
parse_value(#{type := uint32}, Token) -> parse_integer(Token, uint32_range());

parse_value(#{type := uint16, range := Range}, Token) ->
    FullRange = merge_ranges(uint16_range(), Range),
    parse_integer(Token, FullRange);
parse_value(#{type := uint16}, Token) -> parse_integer(Token, uint16_range());

parse_value(#{type := uint8, range := Range}, Token) ->
    FullRange = merge_ranges(uint8_range(), Range),
    parse_integer(Token, FullRange);
parse_value(#{type := uint8}, Token) -> parse_integer(Token, uint8_range());

parse_value(#{type := int64, range := Range}, Token) ->
    FullRange = merge_ranges(int64_range(), Range),
    parse_integer(Token, FullRange);
parse_value(#{type := int64}, Token) -> parse_integer(Token, int64_range());

parse_value(#{type := int32, range := Range}, Token) ->
    FullRange = merge_ranges(int32_range(), Range),
    parse_integer(Token, FullRange);
parse_value(#{type := int32}, Token) -> parse_integer(Token, int32_range());

parse_value(#{type := int16, range := Range}, Token) ->
    FullRange = merge_ranges(int16_range(), Range),
    parse_integer(Token, FullRange);
parse_value(#{type := int16}, Token) -> parse_integer(Token, int16_range());

parse_value(#{type := int8, range := Range}, Token) ->
    FullRange = merge_ranges(int8_range(), Range),
    parse_integer(Token, FullRange);
parse_value(#{type := int8}, Token) -> parse_integer(Token, int8_range());

parse_value(#{type := string}, Token) -> Token;
parse_value(#{type := boolean}, "true") -> true;
parse_value(#{type := boolean}, "false") -> false;
parse_value(#{type := {Mod, Type}}, Token) when is_atom(Mod) ->
    case Mod:parse_value(Type, Token) of
        {ok, Value} -> Value;
        {error, _Err} = Err ->
            Err
    end;
parse_value(Type, Token) ->
    io:format("CLI unsupported leaf type: ~p\n", [Type]),
    Token.

parse_list_keys([], Item, Acc, Txn) ->
    parse([], [], [Item | Acc], Txn);
parse_list_keys([space], Item, Acc, Txn) ->
    parse([], [], [Item | Acc], Txn);
parse_list_keys([space | Ts], Item, Acc, Txn) ->
    parse_list_keys(Ts, Item, Acc, Txn);
parse_list_keys([{token, Tok} | Ts], #{key_names := KeyNames, key_values := KeyValues} = Item, Acc, Txn) ->
    KeyValues1 = KeyValues ++ [Tok],
    Item1 = Item#{key_values => KeyValues1},
    if length(KeyNames) == length(KeyValues1) ->
            %% Got the list keys, carry on in the main parser
            Children = ecli_util:children(Item1, Txn, undefined),
            parse(Ts, Children, [Item1 | Acc], Txn);
        true ->
            parse_list_keys(Ts, Item1, Acc, Txn)
    end.

merge_ranges([{min, Min} | Rs], UserRange) ->
    case lists:keyfind(min, 1, UserRange) of
        {min, UMin} when UMin > Min ->
            [{min, UMin} | merge_ranges(Rs, UserRange)];
        _ ->
            [{min, Min} | merge_ranges(Rs, UserRange)]
    end;
merge_ranges([{max, Max} | Rs], UserRange) ->
    case lists:keyfind(max, 1, UserRange) of
        {max, UMax} when UMax < Max ->
            [{max, UMax} | merge_ranges(Rs, UserRange)];
        _ ->
            [{max, Max} | merge_ranges(Rs, UserRange)]
    end;
merge_ranges([], _) ->
    [].

uint64_range() -> [{min, 0}, {max, 18446744073709551615}].
uint32_range() -> [{min, 0}, {max, 4294967295}].
uint16_range() -> [{min, 0}, {max, 65535}].
uint8_range() -> [{min, 0}, {max, 255}].

int64_range() -> [{min, -9223372036854775808}, {max, 9223372036854775807}].
int32_range() -> [{min, -2147483648}, {max, 2147483647}].
int16_range() -> [{min, -32768}, {max, 32767}].
int8_range() -> [{min, -128}, {max, 127}].

parse_integer(Token, Range) ->
    case catch list_to_integer(Token) of
        {'EXIT', _} ->
            {error, "Expected an integer value"};
        Int ->
            parse_integer_in_range(Int, Range)
    end.

parse_integer_in_range(Int, [{min, Min} | Rs]) when Int >= Min ->
    parse_integer_in_range(Int, Rs);
parse_integer_in_range(Int, [{max, Max} | Rs]) when Int =< Max ->
    parse_integer_in_range(Int, Rs);
parse_integer_in_range(Int, []) ->
    Int;
parse_integer_in_range(_Int, _) ->
    {error, "Value out of range"}.

lookup(Name, [#{name := Name} = Item | _Tree]) ->
    {ok, Item};
lookup(Name, [#cmd{name = Name} = Cmd | _Tree]) ->
    Item = ecli_util:cmd_to_map(Cmd),
    {ok, Item};
lookup(Name, [_ | Tree]) ->
    lookup(Name, Tree);
lookup(_, []) ->
    false.

remove(Tok, Tree) ->
    lists:filter(fun(#{name := Name}) -> Name /= Tok end, Tree).