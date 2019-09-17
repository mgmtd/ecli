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
-export([open/1, close/1]).

-export([
         getters/3,
         expand/3,
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
-spec open(Path::string()) -> {ok, pid()} | {error, term()}.
open(Path) ->
    cli_sup:start_child(Path).

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
getters(NameFun, DescFun, ChildrenFun) ->
    #getters{
       name_fun = NameFun,
       desc_fun = DescFun,
       children_fun = ChildrenFun
      }.

%%--------------------------------------------------------------------
%% @doc Given a partial or full string and a tree of items expand as
%%      far as possible or show a menu
%% @end
%%--------------------------------------------------------------------
expand(Str, Tree, Getters) ->
    cli_expand:expand(Str, Tree, Getters).


%%--------------------------------------------------------------------
%% @doc Given a list of menu items format it for display inserting
%%      padding to align the descriptions.
%% @end
%%--------------------------------------------------------------------
format_menu([], _) -> "";
format_menu(Items, Getters) ->
    MaxCmdLen = max_cmd_len(Items, Getters),
    Menu = lists:map(fun(Item) ->
                             Cmd = cli_util:name(Getters, Item),
                             Desc = cli_util:description(Getters, Item),
                             [pad(Cmd, MaxCmdLen + 1), Desc, "\r\n"]
                     end, Items),
    ["\r\n", Menu].

max_cmd_len(Items, Gs) ->
    lists:max(lists:map(fun(Item) -> length(cli_util:name(Gs, Item)) end, Items)).

pad(Str, Len) ->
    Pad = lists:duplicate(Len - length(Str), $\s),
    [Str, Pad].


%%%===================================================================
%%% Internal functions
%%%===================================================================
