%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc External API to the cli application
%%%
%%% @end
%%% Created : 11 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cli).

-include("cli.hrl").
-include("debug.hrl").

%% API
-export([open/2, close/1]).

-export([
         accessors/1,  accessors/5, accessors/8,
         sequence/1, tree/2, value/1
        ]).

-export([
         expand/3, expand/4,
         lookup/4,
         format_menu/2
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
    cli_sup:start_child(Path, CLIModule).

%%--------------------------------------------------------------------
%% @doc Close the server end of a previously opened CLI socket. Pid must
%%      be the return value from the previous open call.
%% @end
%%--------------------------------------------------------------------
-spec close(pid()) -> ok | {error, running | restarting | not_found}.
close(Pid) ->
    cli_sup:stop_child(Pid).

%%--------------------------------------------------------------------
%% @doc
%% Create a #accessors{} record to tell cli how to navigate the tree
%%
%% This module doesn't care about the structure of the individual
%% elements in the tree it is operating on, all it needs to know is
%% how to extract the name of the node, its description and
%% children. Systems providing part of a cli tree must provide
%% their own functions to extract these fields from their own tree
%% items.
%%
%% @end
%%--------------------------------------------------------------------
accessors(Mod) when is_atom(Mod) ->
    accessors(fun Mod:name/1, fun Mod:desc/1, fun Mod:children/1,
              fun Mod:action/1, fun Mod:node_type/1,
              fun Mod:list_key_names/1, fun Mod:list_key_values/1,
              fun Mod:set_list_key_values/2).

accessors(NameFun, DescFun, ChildrenFun, ActionFun, NodeTypeFun, ListKeyNamesFun, ListKeyValuesFun, SetListKeyValuesFun) ->
    A = accessors(NameFun, DescFun, ChildrenFun, ActionFun, NodeTypeFun),
    A#accessors{
      list_key_names_fun = ListKeyNamesFun,
      list_key_values_fun = ListKeyValuesFun,
      set_list_key_values_fun = SetListKeyValuesFun
     }.

accessors(NameFun, DescFun, ChildrenFun, ActionFun, NodeTypeFun) ->
    #accessors{
       name_fun = NameFun,
       desc_fun = DescFun,
       children_fun = ChildrenFun,
       action_fun = ActionFun,
       node_type_fun = NodeTypeFun,
       list_key_names_fun = fun(_) -> [] end,
       list_key_values_fun = fun(_) -> [] end,
       set_list_key_values_fun = fun(X) -> X end
      }.

%%--------------------------------------------------------------------
%% @doc Create a more complex spec for children in the cli tree
%%--------------------------------------------------------------------
sequence(Seq) ->
    #cli_sequence{seq = Seq}.

tree(Fun, Opts) ->
    #cli_tree{tree_fun = Fun,
              accessors = proplists:get_value(accessors, Opts),
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
expand(Str, Tree, Accessors) ->
    cli_expand:expand(Str, Tree, Accessors).

expand(Str, Tree, Accessors, UserTxn) ->
    cli_expand:expand(Str, Tree, Accessors, UserTxn).

%%--------------------------------------------------------------------
%% @doc Given a full command string and a tree of items return false
%%      if the command doesn't point to a leaf in the tree,
%%      or {ok, Action} where action is a fun that will execute the
%%      command.
%% @end
%%--------------------------------------------------------------------
-spec lookup(Path::string(), Tree::term(), proplists:proplist(), term()) -> {ok, fun()} | false.
lookup(Str, Tree, Accessors, Txn) ->
    cli_lookup:lookup(Str, Tree, Accessors, Txn).


%%--------------------------------------------------------------------
%% @doc Given a list of menu items format it for display inserting
%%      padding to align the descriptions.
%% @end
%%--------------------------------------------------------------------
format_menu([], _) -> "";
format_menu([I|_] = Items, Accessors) ->
    case cli_util:get_node_type(Accessors, I) of
        new_list_item ->
            format_list_menu(Items, Accessors);
        _ ->
            format_normal_menu(Items, Accessors)
    end.

format_normal_menu(Items, Accessors) ->
    MaxCmdLen = max_cmd_len(Items, Accessors),
    Menu = lists:map(fun(Item) ->
                             Cmd = cli_util:get_name(Accessors, Item),
                             Desc = cli_util:get_description(Accessors, Item),
                             ["  ", pad(Cmd, MaxCmdLen + 1), Desc, "\r\n"]
                     end, Items),
    ["\r\n", Menu].

format_list_menu([I|Items], Accessors) ->
    %% The first item is passed as a placeholder for a new list key
    %% which is needed if this is a set command.
    KeyName = hd(cli_util:get_list_key_names(Accessors, I)),
    NewItem = ["Add new entry\r\n  <", KeyName, ">\r\n"],
    Select = "Select from the existing entries\r\n",
    MaxCmdLen = max_cmd_len(Items, Accessors),
    Menu = lists:map(fun(Item) ->
                             Cmd = cli_util:get_name(Accessors, Item),
                             Desc = cli_util:get_description(Accessors, Item),
                             ["  ", pad(Cmd, MaxCmdLen + 1), "\r\n"]
                     end, Items),
    ["\r\n", NewItem, Select, Menu].

max_cmd_len([], _Gs) ->
    0;
max_cmd_len(Items, Gs) ->
    lists:max(lists:map(fun(Item) -> length(cli_util:get_name(Gs, Item)) end, Items)).

pad(Str, Len) ->
    Pad = lists:duplicate(Len - length(Str), $\s),
    [Str, Pad].


%%%===================================================================
%%% Internal functions
%%%===================================================================
