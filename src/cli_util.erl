%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Useful common routines
%%%
%%% @end
%%% Created : 17 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cli_util).

-include("cli.hrl").

-export([name/2, description/2, children/2, action/2, node_type/2, eval_childspec/1]).


name(#getters{name_fun = Fn}, Item) -> Fn(Item).

description(#getters{desc_fun = Fn}, Item) -> Fn(Item).

children(#getters{children_fun = Fn}, Item) -> Fn(Item).

node_type(#getters{node_type_fun = Fn}, Item) -> Fn(Item).

action(#getters{action_fun = Fn}, Item) -> Fn(Item).

eval_childspec(F) when is_function(F) -> F();
eval_childspec(L) when is_list(L) -> L;
eval_childspec(_) -> [].

