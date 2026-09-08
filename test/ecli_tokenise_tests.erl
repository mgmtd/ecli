-module(ecli_tokenise_tests).

-include_lib("eunit/include/eunit.hrl").

tokenise_string_test() ->
    Str = "set config joe \"Str\" ",
    Expect = {ok, [{token,"set"},
                    space,
                    {token,"config"},
                    space,
                    {token,"joe"},
                    space,
                    {string,"Str"},
                    space]},
    ?assertEqual(Expect, ecli_tokenise:string(Str)).

tokenise_pipe_spaced_test() ->
    Str = "show configuration | display xml",
    Expect = {ok, [{token,"show"},
                   space,
                   {token,"configuration"},
                   space,
                   pipe,
                   space,
                   {token,"display"},
                   space,
                   {token,"xml"}]},
    ?assertEqual(Expect, ecli_tokenise:string(Str)).

tokenise_pipe_no_spaces_test() ->
    Str = "show configuration|display xml",
    Expect = {ok, [{token,"show"},
                   space,
                   {token,"configuration"},
                   pipe,
                   {token,"display"},
                   space,
                   {token,"xml"}]},
    ?assertEqual(Expect, ecli_tokenise:string(Str)).

tokenise_pipe_trailing_test() ->
    ?assertEqual({ok, [{token,"show"}, space, pipe]},
                 ecli_tokenise:string("show |")).

tokenise_pipe_inside_quotes_test() ->
    Str = "show | match \"a|b\"",
    Expect = {ok, [{token,"show"},
                   space,
                   pipe,
                   space,
                   {token,"match"},
                   space,
                   {string,"a|b"}]},
    ?assertEqual(Expect, ecli_tokenise:string(Str)).

tokenise_chained_pipes_test() ->
    Str = "show | display xml | match foo",
    Expect = {ok, [{token,"show"},
                   space,
                   pipe,
                   space,
                   {token,"display"},
                   space,
                   {token,"xml"},
                   space,
                   pipe,
                   space,
                   {token,"match"},
                   space,
                   {token,"foo"}]},
    ?assertEqual(Expect, ecli_tokenise:string(Str)).

tokenise_mid_token_test() ->
    Str = "set conf",
    ?assertEqual({ok,[{token,"set"},space,{token,"conf"}]}, ecli_tokenise:string(Str)).

tokenise_mid_string_test() ->
    Str = "set conf \"some",
    ?assertEqual({ok,[{token,"set"}, space, {token,"conf"}, space,{part_string,"some"}]},
                 ecli_tokenise:string(Str)).

tokenise_quote_in_cmd_test() ->
    Str = "set conf serv\"er",
    ?assertEqual(no, ecli_tokenise:string(Str)).