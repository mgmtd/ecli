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
         banner/1,
         prompt/1,
         expand/2
        ]).

-record(cli_juniper,
        {
          mode = operational
        }).

init() ->
    {ok, #cli_juniper{}}.

banner(#cli_juniper{}) ->
    {ok, "\r\nWelcome to the Juniper style CLI\r\n
Hit TAB, SPC or ? at any time to see available commands\r\n"}.

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

