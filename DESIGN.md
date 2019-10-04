Command expander
================

Command expansion or TAB completion as a library

Requirements
------------

The kind of commands we want to expand from anywhere in the string:

* Something with a list item inside:

    show interface ge/1/1/1 status

* Something with a dynamic list item

    show interface g

* Something with a pipe command at an arbitrarly level in the tree

    show interface | display table

* A simple short command

    configure

* Something that creates a new list item in a configuration tree

    set interface ge/3/4/5

* Setting a leaf value inside an existing or new list item

    set interface ge/4/5/6 ipv4 address 10.0.2.3

* Multiple pipe modifications starting at any point in the tree for
  certain commands

    show interface | display tree | defaults

At any point we need to know the possible set of next items

We want to be able to define simple trees of configuration items
e.g. from yang without caring whether the tree is being used in a set
or show command.

So maybe for a show users command that allows pipe commands only at the
very end something like this:

    #cmd{name = "show",
         children = [#cmd{name = "users",
                          children = {sequence [fun show_pipes/1]}}]}

For a set command with a configuration tree followed by a value:

    #cmd{name = "set",
         children = {sequence, [{fun cfg_tree/1, cfg_getters()},
                                fun cfg_value/1]}}


For a show command with a switch to pipe commands allowed at the end
of any level

    #cmd{name = "show",
         children = {sequence [{fun cfg_tree/1, cfg_getters()},
                                fun cfg_value/1]}}

Or maybe a dedicated record to control the grammar of the part after
the command:

    #cmd{name = "set",
         children = #grammar{sequence =  [{fun cfg_tree/1, cfg_getters()},
                                           fun cfg_value/1],
                             pipe_cmds = [{$|, fun cfg_pipe_cmds/1}]}}


Should the expander code be able to do all this itself or should there be a 
