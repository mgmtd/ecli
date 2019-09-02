%%%-------------------------------------------------------------------
%% @doc cli top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(cli_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy  => one_for_one, 
                 intensity =>    5,
                 period    => 10
                },
    Child = #{id => ?MODULE,
              start => {cli_unixdom_listen, start_link, []},
              restart => permanent},
    {ok, {SupFlags, [Child]} }.

%%====================================================================
%% Internal functions
%%====================================================================