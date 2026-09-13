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

-spec lookup(string(), list(), term()) ->
          {ok, [map()], [map()], [[map()]]} | {error, string()}.
lookup(Str, Tree, Txn) ->
    ?DBG("lookup ~p~n Tree: ~p~n Txn:~p~n",[Str, Tree, Txn]),
    case ecli_tokenise:string(Str) of
        {ok, Tokens} ->
            ?DBG("tokens ~p~n",[Tokens]),
            parse(normalise(Tokens), Tree, [], Txn, undefined);
        no ->
            {error, "Command not understood"}
    end.

parse([], _Tree, Acc, _Txn, _Pipes) ->
    {Cmd, Items} = split_cmd(Acc),
    ?DBG("Looked up Cmd = ~p~nItems = ~p~n", [Cmd, Items]),
    {ok, Cmd, Items, []};
parse([pipe | Ts], _Tree, Acc, Txn, Pipes) ->
    case has_pipes(Pipes) andalso Acc =/= [] of
        false ->
            {error, "Pipe commands not allowed"};
        true ->
            {Cmd, Items} = split_cmd(Acc),
            parse_pipe_stage(Ts, Pipes, [], [], Cmd, Items, Pipes, Txn)
    end;
parse([{part_string, _Str}], _Tree, _Acc, _Txn, _Pipes) ->
    {error, "Command not understood"};
parse([{token, ""} | _Ts], _Tree, [], _Txn, _Pipes) ->
    {error, ""};
parse([{token, Tok} | Ts], Tree, [], Txn, Pipes) ->
    case lookup(Tok, Tree) of
        {ok, #cmd{} = CmdItem} ->
            Item = ecli_util:cmd_to_map(CmdItem),
            Children = ecli_util:children(Item, Txn, undefined),
            parse(Ts, Children, [Item], Txn, update_pipes(Item, Pipes));
        {ok, #{node_type := container} = Item} ->
            Children = ecli_util:children(Item, Txn, undefined),
            parse(Ts, Children, [Item], Txn, update_pipes(Item, Pipes));
        {ok, #{node_type := leaf} = Item} ->
            parse(Ts, [], [Item], Txn, update_pipes(Item, Pipes));
        false ->
            {error, "Command not understood"}
    end;
parse([{token, Tok} | Ts], Tree, [#{node_type := NodeType} | _] = Acc, Txn, Pipes)
  when NodeType == container; NodeType == list ->
    case lookup(Tok, Tree) of
        {ok, #cmd{} = CmdItem} ->
            Item = ecli_util:cmd_to_map(CmdItem),
            Children = ecli_util:children(Item, Txn, undefined),
            parse(Ts, Children, [Item | Acc], Txn, update_pipes(Item, Pipes));
        {ok, #{node_type := container} = Item} ->
            Children = ecli_util:children(Item, Txn, undefined),
            parse(Ts, Children, [Item | Acc], Txn, update_pipes(Item, Pipes));
        {ok, #{node_type := Leaf} = Item} when Leaf == leaf; Leaf == leaf_list ->
            Tree1 = remove(Tok, Tree),
            parse(Ts, Tree1, [Item | Acc], Txn, update_pipes(Item, Pipes));
        {ok, #{node_type := list} = Item} ->
            parse_list_keys(Ts, Item, Acc, Txn, update_pipes(Item, Pipes));
        false ->
            {error, "Command not understood"}
    end;
parse([{token, Tok} | Ts], Tree, [#{node_type := leaf, value := _Value} | _] = Acc, Txn, Pipes) ->
    case lookup(Tok, Tree) of
        {ok, #{node_type := Leaf} = Item} when Leaf == leaf; Leaf == leaf_list ->
            Tree1 = remove(Tok, Tree),
            parse(Ts, Tree1, [Item | Acc], Txn, update_pipes(Item, Pipes));
        {ok, #{node_type := container} = Item} ->
            Children = ecli_util:children(Item, Txn, undefined),
            parse(Ts, Children, [Item | Acc], Txn, update_pipes(Item, Pipes));
        {ok, #{node_type := list} = Item} ->
            parse_list_keys(Ts, Item, Acc, Txn, update_pipes(Item, Pipes));
        false ->
            {error, "Unkown parameter"}
    end;
parse([{token, Tok} | Ts], Tree, [#{node_type := leaf} = Leaf | Acc], Txn, Pipes) ->
    case parse_value(Leaf, Tok) of
        {error, _Reason} = Err ->
            Err;
        Value ->
            Leaf1 = Leaf#{value => Value},
            parse(Ts, Tree, [Leaf1 | Acc], Txn, Pipes)
    end;
parse([space | Ts], Tree, Acc, Txn, Pipes) ->
    parse(Ts, Tree, Acc, Txn, Pipes);
parse([{token, _} | _], _Tree, _Acc, _Txn, _Pipes) ->
    {error, "Command not understood"}.

parse_value(#{type := Type, range := Range}, Token) when is_atom(Type) ->
    unwrap_parse(ecli_types:parse({Type, Range}, Token));
parse_value(#{type := Type}, Token) ->
    unwrap_parse(ecli_types:parse(Type, Token));
parse_value(_Leaf, Token) ->
    Token.

unwrap_parse({ok, Value}) -> Value;
unwrap_parse({error, _Reason} = Err) -> Err.

parse_list_keys([pipe | _] = Ts, Item, Acc, Txn, Pipes) ->
    parse(Ts, [], [Item | Acc], Txn, Pipes);
parse_list_keys([], Item, Acc, Txn, Pipes) ->
    parse([], [], [Item | Acc], Txn, Pipes);
parse_list_keys([space], Item, Acc, Txn, Pipes) ->
    parse([], [], [Item | Acc], Txn, Pipes);
parse_list_keys([space | Ts], Item, Acc, Txn, Pipes) ->
    parse_list_keys(Ts, Item, Acc, Txn, Pipes);
parse_list_keys([{token, Tok} | Ts], #{key_names := KeyNames, key_values := KeyValues} = Item, Acc, Txn, Pipes) ->
    KeyValues1 = KeyValues ++ [Tok],
    Item1 = Item#{key_values => KeyValues1},
    if length(KeyNames) == length(KeyValues1) ->
            Children = ecli_util:children(Item1, Txn, undefined),
            parse(Ts, Children, [Item1 | Acc], Txn, Pipes);
       true ->
            parse_list_keys(Ts, Item1, Acc, Txn, Pipes)
    end.

%%--------------------------------------------------------------------
%% Pipe stages
%%--------------------------------------------------------------------

parse_pipe_stage([pipe | _Ts], _Tree, [], _Stages, _Cmd, _Items, _Catalog, _Txn) ->
    {error, "Incomplete command"};
parse_pipe_stage([pipe | Ts], _Tree, StageAcc, Stages, Cmd, Items, Catalog, Txn) ->
    case stage_complete(StageAcc) of
        false ->
            {error, "Incomplete command"};
        true ->
            parse_pipe_stage(Ts, Catalog, [], [lists:reverse(StageAcc) | Stages],
                             Cmd, Items, Catalog, Txn)
    end;
parse_pipe_stage([], _Tree, StageAcc, Stages, Cmd, Items, _Catalog, _Txn) ->
    finish_pipes(StageAcc, Stages, Cmd, Items);
parse_pipe_stage([space | Ts], Tree, StageAcc, Stages, Cmd, Items, Catalog, Txn) ->
    parse_pipe_stage(Ts, Tree, StageAcc, Stages, Cmd, Items, Catalog, Txn);
parse_pipe_stage([{token, Tok} | Ts], Tree, [], Stages, Cmd, Items, Catalog, Txn) ->
    case lookup(Tok, Tree) of
        {ok, Item0} ->
            Item = item_map(Item0),
            Children = ecli_util:children(Item, Txn, undefined),
            parse_pipe_stage(Ts, Children, [Item], Stages, Cmd, Items, Catalog, Txn);
        false ->
            {error, "Command not understood"}
    end;
parse_pipe_stage([{token, Tok} | Ts], Tree, [#{node_type := container} | _] = StageAcc,
                 Stages, Cmd, Items, Catalog, Txn) ->
    case lookup(Tok, Tree) of
        {ok, Item0} ->
            Item = item_map(Item0),
            Children = ecli_util:children(Item, Txn, undefined),
            parse_pipe_stage(Ts, Children, [Item | StageAcc], Stages, Cmd, Items, Catalog, Txn);
        false ->
            {error, "Command not understood"}
    end;
parse_pipe_stage([{token, Tok} | Ts], Tree, [#{node_type := leaf} = Leaf | StageAcc],
                 Stages, Cmd, Items, Catalog, Txn) ->
    case maps:is_key(value, Leaf) of
        true ->
            {error, "Command not understood"};
        false ->
            case parse_value(Leaf, Tok) of
                {error, _Reason} = Err ->
                    Err;
                Value ->
                    Leaf1 = Leaf#{value => Value},
                    parse_pipe_stage(Ts, Tree, [Leaf1 | StageAcc], Stages, Cmd, Items, Catalog, Txn)
            end
    end;
parse_pipe_stage([{token, _} | _], _Tree, _StageAcc, _Stages, _Cmd, _Items, _Catalog, _Txn) ->
    {error, "Command not understood"};
parse_pipe_stage([{part_string, _}], _Tree, _StageAcc, _Stages, _Cmd, _Items, _Catalog, _Txn) ->
    {error, "Command not understood"}.

finish_pipes([], [], _Cmd, _Items) ->
    {error, "Incomplete command"};
finish_pipes([], _Stages, _Cmd, _Items) ->
    %% Trailing `|` with no following pipe command
    {error, "Incomplete command"};
finish_pipes(StageAcc, Stages, Cmd, Items) ->
    case stage_complete(StageAcc) of
        false ->
            {error, "Incomplete command"};
        true ->
            {ok, Cmd, Items, lists:reverse([lists:reverse(StageAcc) | Stages])}
    end.

stage_complete([#{action := {pipe, _}, value := _} | _]) ->
    true;
stage_complete([#{action := {pipe, match}} | _]) ->
    false;
stage_complete([#{action := {pipe, except}} | _]) ->
    false;
stage_complete([#{action := {pipe, {compare, rollback}}} | _]) ->
    false;
stage_complete([#{action := {pipe, _}} | _]) ->
    true;
stage_complete([#{value := _} | _]) ->
    true;
stage_complete(_) ->
    false.

split_cmd(Acc) ->
    lists:splitwith(fun(#{role := Role}) -> Role == cmd end, lists:reverse(Acc)).

item_map(#cmd{} = C) ->
    ecli_util:cmd_to_map(C);
item_map(M) ->
    M.

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
