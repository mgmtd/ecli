%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc External API
%%%
%%% @end
%%% Created : 11 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cli).

%% API
-export([open/1, close/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Open the server end of the CLI at the provided file system path
%%      One reasonably sane location could be: /var/tmp/
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec open(Path::string()) -> {ok, pid()} | {error, term()}.
open(Path) ->
    cli_sup:start_child(Path).

%%--------------------------------------------------------------------
%% @doc Close the server end of a previously opened CLI socket. Pid must
%%      be the return value from the previous open call.
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec close(pid()) -> ok | {error, running | restarting | not_found}.
close(Pid) ->
    cli_sup:stop_child(Pid).

%%%===================================================================
%%% Internal functions
%%%===================================================================
