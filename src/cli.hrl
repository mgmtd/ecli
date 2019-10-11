%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Common record defs
%%%
%%% @end
%%% Created : 17 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------

-record(accessors,
        {
         name_fun,
         desc_fun,
         children_fun,
         action_fun,
         node_type_fun,
         list_key_names_fun,
         list_key_values_fun,
         set_list_key_values_fun
        }).

-record(cli_sequence,
        {
         seq = []
        }).

-record(cli_tree,
        {
         tree_fun,
         accessors,
         pipe_cmds = [],
         add_list_items = false
        }).

-record(cli_value,
        {
         value_fun
        }).
