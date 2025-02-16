%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc socket handling process acting as a cli server.
%%%
%%% @end
%%% Created : 15 Aug 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(ecli_server).

-behaviour(gen_server).

%% API
-export([start_link/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


-record(state,
        {
         socket,
         listen_pid,
         listen_socket,
         term,                                 % #ecli_term{}
         edlin,                                % #ecli_edlin{}
         ecli_mod,         % User defined CLI callback implementation
         ecli_state,          % State threaded through cli callbacks
         history = [],     % command line history for this session
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
%% @end
%%--------------------------------------------------------------------
-spec start_link(pid(), inet:socket(), atom()) -> {ok, pid()}.
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
                ecli_mod = CliMod
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
            ecli_unixdom_listen:notify_connection_established(Lp),
            inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{socket = Socket}};
        {error, _Reason} ->
            %% Listen socket closed during normal shutdown
            {stop, normal, State}
    end;
handle_info({tcp, Socket, Data}, #state{got_meta = false,
                                        ecli_mod = CliMod} = State) ->
    %% Initialise terminal with metadata received from the cli C program
    {ok, Term} = ecli_term:new(Data),

    %% Fetch the initial callback module state
    {ok, CliState} = CliMod:init(),

    %% Send user defined banner
    {ok, Banner} = CliMod:banner(CliState),
    ok = gen_tcp:send(Socket, Banner),

    %% Set up edlin with the intial prompt
    {ok, Prompt} = CliMod:prompt(CliState),
    {Edlin, InitialOps} = ecli_edlin:start(Prompt),
    Term1 = send_drv(InitialOps, Socket, Term),

    %% Start Fetching user input from the cli program
    inet:setopts(Socket, [{active, once}]),

    {noreply, State#state{got_meta = true, term = Term1,
                          edlin = Edlin, ecli_state = CliState}};
handle_info({tcp, _Socket, Data}, #state{buf = Buf} = State) ->
    %% ?DBG("GOT ~p~n",[Data]),
    %% We received one or more chars. Normal chars get appended to the
    %% current line, ctrl chars affect the current line in various
    %% ways.


    Bin = case Buf of
              <<>> -> Data;
              _ -> <<Buf/binary, Data/binary>>
          end,
    %% SIGWINCH will cause the ecli program to send us 0 followed by the new
    %% terminal size. There may be multiple in a packet. Strip them all
    %% out before passing the user input to the next step
    {Bin0, Term1, PartSigwinch} = process_sigwinch(Bin, State#state.term),

    %% Convert Data from UTF-8 binary (FIXME - other charsets?)
    %% dealing with partial character boundaries
    {CharList, Buf1} = case unicode:characters_to_list(Bin0, utf8) of
                           {error, Chars, _} ->
                               {Chars, PartSigwinch};
                           {incomplete, Chars, Tail } ->
                               {Chars, <<Tail/binary, PartSigwinch/binary>>};
                           Chars ->
                               {Chars, PartSigwinch}
                       end,

    case get_chars_loop(CharList, State#state{term = Term1}) of
        {ok, State1} ->
            {noreply, State1#state{buf = Buf1}};
        stop ->
            {stop, normal, State}
    end;
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
%% Remove all sigwinch entries in the received data, keeping the final one
%% as the new terminal size. If we got part of a sigwinch entry return it
%% to be placed back in the read buffer.
process_sigwinch(Bin, Term) ->
    case binary:match(Bin, <<0>>) of
        nomatch ->
            {Bin, Term, <<>>};
        {Start, 1} ->
            if size(Bin) - Start >= 12 ->
                    %% We have a full sigwinch update
                    {PreData, SigWinchPre} = erlang:split_binary(Bin, Start),
                    {SigWinch, Trailing} = erlang:split_binary(SigWinchPre, 12),
                    {Rows, Cols} = parse_sigwinch(SigWinch),
                    process_sigwinch(<<PreData/binary, Trailing/binary>>, ecli_term:sigwinch(Rows, Cols, Term));
               true ->
                    %% Partial Sigwinch update
                    {PreData, PartSigWinch} = erlang:split_binary(Bin, Start),
                    {PreData, Term, PartSigWinch}
            end
    end.

parse_sigwinch(<<0, RowsBin:5/binary, ColsBin:5/binary, 0>>) ->
    {list_to_integer(binary_to_list(RowsBin)), list_to_integer(binary_to_list(ColsBin))}.

get_chars_loop(CharList, #state{ecli_mod = CliMod} = State) ->
    case ecli_edlin:insert(CharList, State#state.edlin) of
        {more_chars, Edlin, Ops} ->
            %% ?DBG("Inserted ~p\r\n",[{more_chars, Edlin, Ops}]),
            Term = send_drv(Ops, State#state.socket, State#state.term),
            {ok, State#state{edlin = Edlin, term = Term}};
        {expand, Before0, Cs0, Edlin} ->
            Before = lists:reverse(Before0),
            {Found, Add, Matches, CliState} = CliMod:expand(Before, State#state.ecli_state),
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
            get_chars_loop(Cs, State#state{edlin = Edlin, ecli_state = CliState});
        {done, FullLine, Cs, Ops} ->
            %% ?DBG("CMD: ~p~n",[FullLine]),
            case CliMod:execute(FullLine, State#state.ecli_state) of
                {ok, Output, CliState} ->
                    ok = send_raw("\r\n", State),
                    ok = send_raw(Output, State),
                    Term = send_drv(Ops, State#state.socket, State#state.term),
                    {ok, Prompt} = CliMod:prompt(CliState),
                    History = case FullLine of
                                  "" -> State#state.history;
                                  _-> [FullLine | State#state.history]
                              end,
                    {Edlin, InitialOps} = ecli_edlin:start(Prompt, History),
                    Term1 = send_drv(InitialOps, State#state.socket, Term),
                    get_chars_loop(Cs, State#state{term = Term1,
                                                   edlin = Edlin,
                                                   history = History,
                                                   ecli_state = CliState});
                stop ->
                    gen_tcp:close(State#state.socket),
                    stop
            end;
        {cancel, Ops} ->
            ok = send_raw("\r\n", State),
            Term = send_drv(Ops, State#state.socket, State#state.term),
            {ok, Prompt} = CliMod:prompt(State#state.ecli_state),
            {Edlin, InitialOps} = ecli_edlin:start(Prompt, State#state.history),
            Term1 = send_drv(InitialOps, State#state.socket, Term),
            get_chars_loop([], State#state{term = Term1,
                                           edlin = Edlin});
        stop ->
            case CliMod:mode_after_exit(State#state.ecli_state) of
                stop ->
                    stop;
                CliState2 ->
                    ok = send_raw("\r\n\r\n[ok]\r\n", State),
                    {ok, Prompt} = CliMod:prompt(CliState2),
                    {Edlin, InitialOps} = ecli_edlin:start(Prompt, State#state.history),
                    Term1 = send_drv(InitialOps, State#state.socket, State#state.term),
                    get_chars_loop([], State#state{term = Term1,
                                                   edlin = Edlin,
                                                   ecli_state = CliState2})
            end
    end.


send_drv(Ops, Socket, Term0) ->
    %% ?DBG("Sending Ops~p\r\n",[Ops]),
    %% ?DBG("Term before ~s\r\n",[ecli_term:print(Term0)]),
    {Bytes, Term} = ecli_term:send_ops(Ops, Term0),
    %% ?DBG("Term after ~s\r\n",[ecli_term:print(Term)]),
    case gen_tcp:send(Socket, Bytes) of
        ok -> ok;
        Err ->
            io:format("Err: ~p Sending ~p~n", [Err, Bytes])
    end,
    inet:setopts(Socket, [{active, once}]),
    Term.

send_raw(Bytes, #state{socket = Socket}) ->
    %% ?DBG("Sending raw ~p\r\n",[Bytes]),
    case gen_tcp:send(Socket, Bytes) of
        ok -> ok;
        _Err ->
            io:format("CLI Error sending bytes ~p~n",[Bytes]),
            ok
    end.

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
