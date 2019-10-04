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

%% API
-export([open/2, close/1]).

-export([
         getters/1,  getters/5,
         sequence/1, tree/2, value/1
        ]).

-export([
         expand/3, expand/4,
         lookup/3,
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
%%
%% This module doesn't care about the structure of the individual
%% elements in the tree it is operating on, all it needs to know is
%% how to extract the name of the node, its description and
%% children. Subsystems providing part of a cli tree must provide
%% their own functions to extract these fields from their own tree
%% items.
%%
%% @end
%%--------------------------------------------------------------------
getters(Mod) when is_atom(Mod) ->
    getters(fun Mod:name/1, fun Mod:desc/1, fun Mod:children/1,
            fun Mod:action/1, fun Mod:node_type/1).

getters(NameFun, DescFun, ChildrenFun, ActionFun, NodeTypeFun) ->
    #getters{
       name_fun = NameFun,
       desc_fun = DescFun,
       children_fun = ChildrenFun,
       action_fun = ActionFun,
       node_type_fun = NodeTypeFun
      }.

%%--------------------------------------------------------------------
%% @doc Create a more complex spec for children in the cli tree
%%--------------------------------------------------------------------
sequence(Seq) ->
    #cli_sequence{seq = Seq}.

tree(Fun, Opts) ->
    #cli_tree{tree_fun = Fun,
              getters = proplists:get_value(getters, Opts),
              pipe_cmds = proplists:get_value(pipe_cmds, Opts, [])
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
expand(Str, Tree, Getters) ->
    cli_expand:expand(Str, Tree, Getters).

expand(Str, Tree, Getters, UserTxn) ->
    cli_expand:expand(Str, Tree, Getters, UserTxn).

%%--------------------------------------------------------------------
%% @doc Given a full command string and a tree of items return false
%%      if the command doesn't point to a leaf in the tree,
%%      or {ok, Action} where action is a fun that will execute the
%%      command.
%% @end
%%--------------------------------------------------------------------
-spec lookup(Path::string(), Tree::term(), proplists:proplist()) -> {ok, fun()} | false.
lookup(Str, Tree, Getters) ->
    cli_lookup:lookup(Str, Tree, Getters).


%%--------------------------------------------------------------------
%% @doc Given a list of menu items format it for display inserting
%%      padding to align the descriptions.
%% @end
%%--------------------------------------------------------------------
format_menu([], _) -> "";
format_menu(Items, Getters) ->
    MaxCmdLen = max_cmd_len(Items, Getters),
    Menu = lists:map(fun(Item) ->
                             Cmd = cli_util:get_name(Getters, Item),
                             Desc = cli_util:get_description(Getters, Item),
                             [pad(Cmd, MaxCmdLen + 1), Desc, "\r\n"]
                     end, Items),
    ["\r\n", Menu].

max_cmd_len(Items, Gs) ->
    lists:max(lists:map(fun(Item) -> length(cli_util:get_name(Gs, Item)) end, Items)).

pad(Str, Len) ->
    Pad = lists:duplicate(Len - length(Str), $\s),
    [Str, Pad].


%%%===================================================================
%%% Internal functions
%%%===================================================================
