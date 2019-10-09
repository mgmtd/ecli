%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Common record defs
%%%
%%% @end
%%% Created : 17 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------

-record(getters,
        {
         name_fun,
         desc_fun,
         children_fun,
         action_fun,
         node_type_fun
        }).

-record(cli_sequence,
        {
         seq = []
        }).

-record(cli_tree,
        {
         tree_fun,
         getters,
         pipe_cmds = [],
         add_list_items = false
        }).

-record(cli_value,
        {
         value_fun
        }).
