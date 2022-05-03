-module(ecli_tests).

-include_lib("eunit/include/eunit.hrl").

format_table_test() ->
    Data = [#{ pubkey  => <<"pp_2HkjvwVypj5p4rEssCvM222iNUQpWA5vjwmgv8gMvzj9YaEJ23">> 
             , host    => <<"bobdylanistheman">>
             , port    => 1234}],
    ?assertEqual(<<"host             port pubkey\r\n----             ---- ------\r\nbobdylanistheman 1234 pp_2HkjvwVypj5p4rEssCvM222iNUQpWA5vjwmgv8gMvzj9YaEJ23\r\n">>, list_to_binary(ecli:format_table(Data))).