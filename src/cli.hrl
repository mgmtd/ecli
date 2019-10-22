%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Common record defs
%%%
%%% @end
%%% Created : 17 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------

-record(cli_sequence,
        {
         seq = []
        }).

-record(cli_tree,
        {
         tree_fun,
         pipe_cmds = [],
         add_list_items = false
        }).

-record(cli_value,
        {
         value_fun
        }).
