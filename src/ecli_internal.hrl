%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Common record defs
%%%
%%% @end
%%% Created : 17 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------

%% -define(DEBUG, 1).

-ifdef(DEBUG).
-define(DBG(DATA), io:format(user, "[~p:~p] ~p~n",[?MODULE, ?LINE, DATA])).
-define(DBG(FORMAT, ARGS), io:format(user, "[~p:~p] " ++ FORMAT,[?MODULE, ?LINE] ++ ARGS)).
-else.
-define(DBG(DATA), ok).
-define(DBG(FORMAT, ARGS), ok).
-endif.

