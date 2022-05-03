%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc External API to the cli application
%%%
%%% @end
%%% Created : 11 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(ecli).

-include("ecli.hrl").

%% API
-export([open/2, close/1]).

-export([
         sequence/1, tree/2, value/1
        ]).

-export([
         expand/2, expand/3,
         lookup/3,
         format/1,
         format_table/1,
         format_menu/1,
         format_simple_tree/1
        ]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Open the server end of the CLI at the provided file system path
%%      One reasonably sane location could be: /var/tmp/
%% @end
%%--------------------------------------------------------------------
-spec open(Path::string(), CLIModule::atom()) -> {ok, pid()} | {error, term()}.
open(Path, CLIModule) ->
    ecli_sup:start_child(Path, CLIModule).

%%--------------------------------------------------------------------
%% @doc Close the server end of a previously opened CLI socket. Pid must
%%      be the return value from the previous open call.
%% @end
%%--------------------------------------------------------------------
-spec close(pid()) -> ok | {error, running | restarting | not_found}.
close(Pid) ->
    ecli_sup:stop_child(Pid).

%%--------------------------------------------------------------------
%% @doc Create a more complex spec for children in the cli tree
%%--------------------------------------------------------------------
sequence(Seq) ->
    #cli_sequence{seq = Seq}.

tree(Fun, Opts) ->
    #cli_tree{tree_fun = Fun,
              pipe_cmds = proplists:get_value(pipe_cmds, Opts, []),
              add_list_items = proplists:get_value(add_list_items, Opts, false)
             }.

value(Fun) ->
    #cli_value{value_fun = Fun}.

%%--------------------------------------------------------------------
%% @doc
%%  Given a partial or full string and a tree of items, expand as
%%  far as possible or show a menu. If a TargetNodeType is provided
%%  stop when a single node of that type is reached
%% @end
%%--------------------------------------------------------------------
expand(Str, Tree) ->
    ecli_expand:expand(Str, Tree).

expand(Str, Tree, UserTxn) ->
    ecli_expand:expand(Str, Tree, UserTxn).

%%--------------------------------------------------------------------
%% @doc Given a full command string and a tree of items return false
%%      if the command doesn't point to a leaf in the tree,
%%      or {ok, Action} where action is a fun that will execute the
%%      command.
%% @end
%%--------------------------------------------------------------------
-spec lookup(Path::string(), Tree::term(), term()) -> {ok, fun()} | false.
lookup(Str, Tree, Txn) ->
    ecli_lookup:lookup(Str, Tree, Txn).

%%--------------------------------------------------------------------
%% @doc Given a list of menu items format it for display inserting
%%      padding to align the descriptions.
%% @end
%%--------------------------------------------------------------------
format_menu([]) -> "";
format_menu([#{node_type := NodeType}|_] = Items) when NodeType == new_list_item; NodeType == list_key ->
    format_list_menu(Items);
format_menu(Items) ->
    format_normal_menu(Items).

format_normal_menu(Items) ->
    MaxCmdLen = max_cmd_len(Items),
    Menu = lists:map(fun(#{name := Cmd, desc := Desc}) ->
                             ["  ", pad(Cmd, MaxCmdLen + 1), Desc, "\r\n"]
                     end, Items),
    ["\r\n", Menu].

format_list_menu([#{key_names := KeyNames, key_values := KeyValues,
                    node_type := new_list_item}|Items]) ->
    %% The first item is passed as a placeholder for a new list key
    %% which is needed if this is a set command.
    KeyName = next_list_key(KeyNames, KeyValues),
    NewItem = ["Add new entry\r\n  <", KeyName, ">\r\n"],
    ["\r\n", NewItem, format_list_menu(Items)];

format_list_menu([]) ->
    [];
format_list_menu(Items) ->
    Select = "Select from the existing entries\r\n",
    MaxCmdLen = max_cmd_len(Items),
    Menu = lists:map(fun(#{name := Name, desc := _Desc}) ->
                             ["  ", pad(Name, MaxCmdLen + 1), "\r\n"]
                     end, Items),
    ["\r\n", Select, Menu].

next_list_key([Name | _], []) ->
    Name;
next_list_key([_Name|Ns], [_Key|Ks]) ->
    next_list_key(Ns, Ks).


max_cmd_len([]) ->
    0;
max_cmd_len(Items) ->
    lists:max(lists:map(fun(#{name := Name}) -> length(Name) end, Items)).

format_simple_tree(Tree) ->
    format_simple_tree(Tree, 0).

format_simple_tree([{Name, {value, Val}}|Ts], Padding) ->
    [spaces(Padding), Name," ", Val, ";\r\n",
     format_simple_tree(Ts, Padding)];
format_simple_tree([{Name, Children}|Ts], Padding) ->
    [spaces(Padding), fmt_name(Name), " {\r\n",
     format_simple_tree(Children, Padding + 2),
     spaces(Padding), "}\r\n",
     format_simple_tree(Ts, Padding)];
format_simple_tree([], _) ->
    [].

fmt_name(T) when is_tuple(T) ->
    lists:join(" ", tuple_to_list(T));
fmt_name(N) -> N.

%%--------------------------------------------------------------------
%% @doc Format output from a command in pretty format.
%%      padding to align the descriptions.
%%
%%      Takes a map or Key value list
%% @end
%%--------------------------------------------------------------------
format(#{} = Map) ->
    Keys = maps:keys(Map),
    Longest = lists:max(lists:map(fun(K) -> key_size(K) end, Keys)),
    lists:map(fun(K) -> [pad(K, Longest), " : ", format_value(maps:get(K, Map)), "\r\n"] end, Keys).

format_value(Bin) when is_binary(Bin) -> Bin;
format_value(Int) when is_integer(Int) -> integer_to_binary(Int);
format_value(Atom) when is_atom(Atom) -> atom_to_list(Atom);
format_value(Else) -> io_lib:format("~p", [Else]).

format_table([#{} = Map | _] = Maps) ->
    Titles = maps:keys(Map),
    TitleLengths = lists:map(fun(T) -> {T, key_size(T)} end, Titles),
    Lengths = maps:from_list(TitleLengths),
    ColLengths = lists:foldl(
                    fun(M, Ls) ->
                        maps:fold(fun(K, V, L) ->
                            maps:put(K, max(maps:get(K, L), printable_size(V)), L)
                        end, Ls, M)
                    end, Lengths, Maps),
    TitleRow = pad_row(maps:to_list(ColLengths)),
    TitleUnderline = pad_row(lists:map(fun({T, L}) -> {list_to_binary(lists:duplicate(printable_size(T), $-)), L} end, maps:to_list(ColLengths))),
    Rows = lists:map(fun(Row) ->
                        RowWithLengths = lists:map(fun(T) -> {maps:get(T, Row), maps:get(T, ColLengths)} end, Titles),
                        [pad_row(RowWithLengths), "\r\n"]
                     end, Maps),
    [TitleRow, "\r\n", TitleUnderline, "\r\n", Rows].

%%%===================================================================
%%% Internal functions
%%%===================================================================
key_size(Bin) when is_binary(Bin) -> size(Bin);
key_size(Atom) when is_atom(Atom) -> length(atom_to_list(Atom));
key_size(Str) when is_list(Str) -> length(Str).

printable_size(Int) when is_integer(Int) -> size(integer_to_binary(Int));
printable_size(Atom) when is_atom(Atom) -> length(atom_to_list(Atom));
printable_size(V) -> iolist_size(V).

pad(Val, Len) ->
    Sz = key_size(Val),
    Pad = max(0, Len - Sz),
    [Val, spaces(Pad)].

pad_row([]) ->
    [];
pad_row([{Val, _Len}]) ->
    [format_value(Val)];
pad_row([{Val, Len} | Vals]) ->
    Printable = format_value(Val),
    [pad(Printable, Len + 1) | pad_row(Vals)].

spaces(Num) ->
    lists:duplicate(Num, $\s).