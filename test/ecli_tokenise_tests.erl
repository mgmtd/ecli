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
                    {token,[]},
                    space]},
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