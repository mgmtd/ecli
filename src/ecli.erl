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
-include("debug.hrl").

%% API
-export([open/2, close/1]).

-export([
         sequence/1, tree/2, value/1
        ]).

-export([
         expand/2, expand/3,
         lookup/3,
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
format_menu([#{node_type := NodeType}|_] = Items) ->
    case NodeType of
        List when List == new_list_item; List == list_key  ->
            format_list_menu(Items);
        _ ->
            format_normal_menu(Items)
    end.

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
    Menu = lists:map(fun(#{name := Name, desc := Desc}) ->
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

pad(Str, Len) ->
    Pad = spaces(Len - length(Str)),
    [Str, Pad].


format_simple_tree(Tree) ->
    format_simple_tree(Tree, 0).

format_simple_tree([{Name, {value, Val}}|Ts], Padding) ->
    [spaces(Padding), Name," ", Val, ";\r\n",
     format_simple_tree(Ts, Padding)];
format_simple_tree([{Name, Children}|Ts], Padding) ->
    Pad = Padding + 2,
    [spaces(Padding), fmt_name(Name), " {\r\n",
     format_simple_tree(Children, Padding + 2),
     spaces(Padding), "}\r\n",
     format_simple_tree(Ts, Padding)];
format_simple_tree([], _) ->
    [].

fmt_name(T) when is_tuple(T) ->
    lists:join(" ", tuple_to_list(T));
fmt_name(N) -> N.


spaces(Num) ->
    lists:duplicate(Num, $\s).

%%%===================================================================
%%% Internal functions
%%%===================================================================
