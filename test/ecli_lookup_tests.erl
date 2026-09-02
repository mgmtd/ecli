-module(ecli_lookup_tests).

-include_lib("eunit/include/eunit.hrl").

%% data_callback for list-key completion; unused once keys are in the command.
-export([list_keys/3]).

list_keys(_Txn, _Path, _Match) ->
    [].

%% `set logger def level error config file /u01/log` — leaf then nested
%% container under a list item. Used to crash ecli_lookup:parse/4.
lookup_leaf_then_nested_container_test() ->
    {ok, Cmd, Items} =
        ecli_lookup:lookup("set logger def level error config file /u01/log",
                           tree(), undefined),
    ?assertMatch([#{name := "set"}], Cmd),
    Names = [maps:get(name, I) || I <- Items],
    ?assertEqual(["logger", "level", "config", "file"], Names),
    Level = lists:nth(2, Items),
    File = lists:last(Items),
    ?assertEqual("error", maps:get(value, Level)),
    ?assertEqual("/u01/log", maps:get(value, File)),
    Logger = hd(Items),
    ?assertEqual(["def"], maps:get(key_values, Logger)).

lookup_sibling_leaves_still_work_test() ->
    {ok, _Cmd, Items} =
        ecli_lookup:lookup("set logger def level error module logger_std_h",
                           tree(), undefined),
    Names = [maps:get(name, I) || I <- Items],
    ?assertEqual(["logger", "level", "module"], Names),
    ?assertEqual("error", maps:get(value, lists:nth(2, Items))),
    ?assertEqual("logger_std_h", maps:get(value, lists:nth(3, Items))).

lookup_nested_container_only_test() ->
    {ok, _Cmd, Items} =
        ecli_lookup:lookup("set logger def config file /u01/log",
                           tree(), undefined),
    Names = [maps:get(name, I) || I <- Items],
    ?assertEqual(["logger", "config", "file"], Names),
    ?assertEqual("/u01/log", maps:get(value, lists:last(Items))).

%%--------------------------------------------------------------------
tree() ->
    [#{role => cmd,
       node_type => container,
       name => "set",
       action => fun(_, _) -> ok end,
       children => fun logger_schema/0}].

logger_schema() ->
    [#{role => schema,
       node_type => list,
       name => "logger",
       desc => "Handlers",
       path => ["logger"],
       key_names => ["id"],
       key_values => [],
       data_callback => ?MODULE,
       children => fun logger_handler/0}].

logger_handler() ->
    [#{role => schema,
       node_type => leaf,
       name => "id",
       desc => "Handler id",
       type => string},
     #{role => schema,
       node_type => leaf,
       name => "level",
       desc => "Log level",
       type => {enum, ["error", "debug"]}},
     #{role => schema,
       node_type => leaf,
       name => "module",
       desc => "Handler module",
       type => string},
     #{role => schema,
       node_type => container,
       name => "config",
       desc => "Handler-specific config",
       children => fun logger_config/0}].

logger_config() ->
    [#{role => schema,
       node_type => leaf,
       name => "file",
       desc => "Log file",
       type => string}].
