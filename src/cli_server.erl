%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc socket handling process acting as a cli server.
%%%
%%% @end
%%% Created : 15 Aug 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cli_server).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,
        {
          socket,
          listen_pid,
          listen_socket,
          term,                                 % #cli_term{}
          edlin,                                % #cli_edlin{}
          cli_mod,         % User defined CLI callback implementation
          cli_state,          % State threaded through cli callbacks
          got_meta = false,
          buf = <<>>                     % Temp buf for UTF boundaries
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
start_link(ListenPid, ListenSocket, CliMod) ->
    gen_server:start_link(?MODULE, [ListenPid, ListenSocket, CliMod], []).

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
init([ListenPid, ListenSocket, CliMod]) ->
    self() ! start_accepting,
    {ok, #state{listen_pid = ListenPid,
                listen_socket = ListenSocket,
                cli_mod = CliMod
               }}.

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
handle_info(start_accepting, #state{listen_socket = Ls, listen_pid = Lp} = State) ->
    case gen_tcp:accept(Ls) of
        {ok, Socket} ->
            cli_unixdom_listen:notify_connection_established(Lp),
            inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{socket = Socket}};
        {error, _Reason} = Err ->
            {stop, Err, State}
    end;
handle_info({tcp, Socket, Data}, #state{got_meta = false,
                                        cli_mod = CliMod} = State) ->
    %% Initialise terminal with metadata from cli program
    {ok, Term} = cli_term:new(Data),

    %% Fetch any initial user defined state
    {ok, CliState} = CliMod:init(),

    %% Send user defined banner
    {ok, Banner} = CliMod:banner(CliState),
    ok = gen_tcp:send(Socket, Banner),

    %% Set up edlin with the intial prompt
    {ok, Prompt} = CliMod:prompt(CliState),
    {Edlin, InitialOps} = cli_edlin:start(Prompt),
    Term1 = send_drv(InitialOps, Socket, Term),

    %% Start Fetching user input from the cli program
    inet:setopts(Socket, [{active, once}]),

    {noreply, State#state{got_meta = true, term = Term1,
                          edlin = Edlin, cli_state = CliState}};
handle_info({tcp, _Socket, Data}, #state{buf = Buf} = State) ->
    io:format("GOT ~p~n",[Data]),

    %% We have one or more chars. Normal chars get appended to the
    %% current line, ctrl chars affect the current line in various
    %% ways.
    %%
    %% Convert Data from UTF-8 binary (FIXME - other charsets?)
    %% dealing with partial character boundaries
    Bin = case Buf of
              <<>> -> Data;
              _ -> <<Buf/binary, Data/binary>>
          end,
    {CharList, Buf1} = case unicode:characters_to_list(Bin, utf8) of
                           {error, Chars, _} ->
                               {Chars, <<>>};
                           {incomplete, Chars, Tail} ->
                               {Chars, Tail};
                           Chars ->
                               {Chars, <<>>}
                       end,

    {ok, Term, Edlin} = get_chars_loop(CharList, State),
    {noreply, State#state{buf = Buf1,
                          term = Term,
                          edlin = Edlin}};
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

get_chars_loop(CharList, #state{cli_mod = CliMod} = State) ->
    case cli_edlin:insert(CharList, State#state.edlin) of
        {more_chars, Edlin, Ops} ->
            io:format("Inserted ~p\r\n",[{more_chars, Edlin, Ops}]),
            Term = send_drv(Ops, State#state.socket, State#state.term),
            {ok, Term, Edlin};
        {expand, Before0, Cs0, Edlin} ->
            Before = lists:reverse(Before0),
            {Found, Add, Matches, CliState} = CliMod:expand(Before, State#state.cli_state),
            case Found of
                no ->
                    case whitespace_only(Before) of
                        false ->
                            send_drv([beep], State#state.socket, State#state.term);
                        true ->
                            ok
                    end;
                yes -> ok
            end,
            Cs1 = Add ++ Cs0,
            Cs =
                case Matches of
                    [] -> Cs1;
                    _ ->
                        ok = send_raw(Matches, State),
                        [$\^L | Cs1]
                end,
            get_chars_loop(Cs, State#state{edlin = Edlin, cli_state = CliState});
        {done, FullLine, Cs, Ops} ->
            io:format("CMD: ~p~n",[FullLine]),
            {ok, Output, CliState} = CliMod:execute(FullLine, State#state.cli_state),
            ok = send_raw(Output, State),
            Term = send_drv(Ops, State#state.socket, State#state.term),
            {ok, Prompt} = CliMod:prompt(State#state.cli_state),
            {Edlin, InitialOps} = cli_edlin:start(Prompt),
            Term1 = send_drv(InitialOps, State#state.socket, Term),
            get_chars_loop(Cs, State#state{term = Term1,
                                           edlin = Edlin,
                                           cli_state = CliState})
    end.


send_drv(Ops, Socket, Term0) ->
    io:format("Sending Ops~p\r\n",[Ops]),
    io:format("Term before ~s\r\n",[cli_term:print(Term0)]),
    {Bytes, Term} = cli_term:send_ops(Ops, Term0),
    io:format("Term after ~s\r\n",[cli_term:print(Term)]),
    ok = gen_tcp:send(Socket, Bytes),
    inet:setopts(Socket, [{active, once}]),
    Term.

send_raw(Bytes, #state{socket = Socket}) ->
    io:format("Sending raw ~p\r\n",[Bytes]),
    ok = gen_tcp:send(Socket, Bytes).

%% Assumes that arg is a string
%% Horizontal whitespace only.
whitespace_only([]) ->
    true;
whitespace_only([C|Rest]) ->
    case C of
	$\s ->
	    whitespace_only(Rest);
	$\t ->
	    whitespace_only(Rest);
	_ ->
	    false
    end.
