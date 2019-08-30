%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc
%%%      Unix domain socket listener
%%% @end
%%% Created : 15 Aug 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cli_unixdom_listen).

-behaviour(gen_server).

%% API
-export([start_link/0, start_link/1, notify_connection_established/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {
          listen_socket,
          path,
          acceptor_pid
        }).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    start_link("/tmp/socket-server").

start_link(Path) ->
    ProcName = list_to_atom("cli_unixdom_" ++ filename:basename(Path)),
    gen_server:start_link({local, ProcName}, ?MODULE, [Path], []).

notify_connection_established(ListenPid) ->
    gen_server:cast(ListenPid, connected).


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Path]) ->
    process_flag(trap_exit, true),
    erlang:send_after(2000, self(), finish_startup),
    {ok, #state{path = Path}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(connected, #state{path = Path, listen_socket = Ls} = State) ->
    AcceptorPid = cli_unixdom_socket:start_link(self(), Ls),
    {noreply, State#state{acceptor_pid = AcceptorPid}};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(finish_startup, #state{path = Path} = State) ->
    file:delete(Path),
    case gen_tcp:listen(0, [{ifaddr, {local, Path}}, binary]) of
        {ok, Socket} ->
            AcceptorPid = cli_unixdom_socket:start_link(self(), Socket),
            {noreply, State#state{listen_socket = Socket,
                                  acceptor_pid = AcceptorPid}};
        {error, Reason} ->
            io:format("Failed to listen on ~p Reason = ~p\n", [ Path, Reason ]),
            erlang:send_after(2000, self(), finish_startup),
            {noreply, State}
    end;
handle_info({'EXIT', Pid, normal}, #state{acceptor_pid = Pid} = State) ->
    %% Normal close of current acceptor process, shouldn't  be able to happen
    {noreply, State};
handle_info({'EXIT', Pid, Reason}, #state{acceptor_pid = Pid, path = Path,
                                          listen_socket = Socket} = State) ->
    io:format("Acceptor process exited ~p~n",[Reason]),
    %% Abnormal close of current acceptor process, create a replacement
    AcceptorPid = cli_unixdom_socket:start_link(self(), Socket),
    {noreply, State#state{acceptor_pid = AcceptorPid}};
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
