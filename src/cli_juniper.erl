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

%%--------------------------------------------------------------------
%% CLI behaviour mandatory callbacks
%%--------------------------------------------------------------------
init() ->
    {ok, #cli_juniper{}}.

banner(#cli_juniper{}) ->
    {ok, "\r\nWelcome to the Juniper style CLI\r\n
Hit TAB, SPC or ? at any time to see available commands\r\n"}.

prompt(#cli_juniper{mode = operational}) ->
    case inet:gethostname() of
        {ok, Hostname} ->
            {ok, Hostname ++  "> "};
        _ ->
            {ok, "> "}
    end.


expand([], #cli_juniper{mode = operational} = J) ->
    {no, [], operational_menu(), J};
expand(Chars, #cli_juniper{} = J) ->
    {no, [], [], J}.

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------
operational_menu() ->
    Menu = [{"show", "Show commands"},
            {"configure", "Enter configuration mode"}],
    format_menu(Menu).

format_menu(Items) ->
    MaxCmdLen = max_cmd_len(Items),
    Menu = lists:map(fun({Cmd, Desc}) ->
                             [pad(Cmd, MaxCmdLen + 1), Desc, "\r\n"]
                     end, Items),
    ["\r\n", Menu].

pad(Str, Len) ->
    Pad = lists:duplicate(Len - length(Str), $\s),
    [Str, Pad].


max_cmd_len(Items) ->
    lists:max(lists:map(fun({Cmd, _}) -> length(Cmd) end, Items)).
