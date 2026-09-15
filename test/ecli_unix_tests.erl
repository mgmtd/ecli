-module(ecli_unix_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("ecli/include/ecli.hrl").

peercred_test() ->
    Path = "/tmp/ecli-peercred-" ++ integer_to_list(erlang:system_time(microsecond)) ++ ".sock",
    file:delete(Path),
    {ok, L} = gen_tcp:listen(0, [{ifaddr, {local, Path}}, binary, {active, false}]),
    Client = spawn(fun() ->
                           {ok, C} = gen_tcp:connect({local, Path}, 0, [binary, {active, false}]),
                           receive
                               close ->
                                   gen_tcp:close(C)
                           end
                   end),
    {ok, A} = gen_tcp:accept(L),
    Cred = ecli:peercred(A),
    ?assertMatch(#{uid := Uid} when is_integer(Uid) andalso Uid >= 0, Cred),
    #{uid := Uid} = Cred,
    ?assertEqual(Uid, my_uid()),
    case Cred of
        #{user := User} ->
            ?assertEqual(true, is_list(User) andalso User =/= []);
        _ ->
            ok
    end,
    Client ! close,
    gen_tcp:close(A),
    gen_tcp:close(L),
    file:delete(Path).

permit_test() ->
    Tree = [#cmd{name = "show", desc = "Show", access = read},
            #cmd{name = "configure", desc = "Configure", access = write},
            #cmd{name = "exit", desc = "Exit", access = any}],
    Names = fun(Cs) -> [N || #cmd{name = N} <- Cs] end,
    ?assertEqual(["show", "configure", "exit"],
                 Names(ecli:permit(Tree, [any, read, write]))),
    ?assertEqual(["show", "exit"],
                 Names(ecli:permit(Tree, [any, read]))),
    ?assertEqual(["configure"],
                 Names(ecli:permit(Tree, [write]))).

permit_untagged_schema_test() ->
    Tree = [#{role => schema, name => "port", node_type => leaf},
            #cmd{name = "set", access = write}],
    [#{name := "port"}] = ecli:permit(Tree, [any, read]).

my_uid() ->
    list_to_integer(string:trim(os:cmd("id -u"))).
