%%%-------------------------------------------------------------------
%% @doc cli top level supervisor. simple_one_for_one for dynamic open / close
%% @end
%%%-------------------------------------------------------------------

-module(ecli_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_child/2, stop_child/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Path, CLIModule) ->
    supervisor:start_child(?MODULE, [Path, CLIModule]).

stop_child(Pid) when is_pid(Pid) ->
    supervisor:delete_child(?MODULE, Pid).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    SupFlags = #{strategy  => simple_one_for_one,
                 intensity =>    5,
                 period    => 10
                },
    Child = #{id => ?MODULE,
              start => {ecli_unixdom_listen, start_link, []},
              restart => permanent},
    {ok, {SupFlags, [Child]} }.

%%====================================================================
%% Internal functions
%%====================================================================
