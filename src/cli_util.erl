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

-export([name/2, description/2, children/2]).


name(#getters{name_fun = Fn}, Item) -> Fn(Item).

description(#getters{desc_fun = Fn}, Item) -> Fn(Item).

children(#getters{children_fun = Fn}, Item) -> Fn(Item).
