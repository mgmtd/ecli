%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Implementation of cli callbacks emulating a juniper like CLI
%%%
%%% @end
%%% Created : 31 Aug 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cli_juniper).

-export([init/0,
         prompt/1,
         expand/2
        ]).

-record(cli_juniper,
        {
          mode = operational
        }).

init() ->
    {ok, #cli_juniper{}}.

prompt(#cli_juniper{mode = operational}) ->
    case inet:gethostname() of
        {ok, Hostname} ->
            H = unicode:characters_to_binary(Hostname, utf8),
            {ok, <<H/binary, "> ">>};
        _ ->
            {ok, <<"> ">>}
    end.


expand(Chars, #cli_juniper{} = J) ->
    {no, [], [], J}.
