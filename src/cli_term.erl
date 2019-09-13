%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Terminal handling. Operations to move cursor
%%%
%%% @end
%%% Created : 18 Aug 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cli_term).


-export([new/1, print/1, send_ops/2]).

-record(term,
        {
          rows :: integer(),
          cols :: integer(),
          term :: binary(),
          isatty :: boolean(),
          llen :: integer(),
          lpos :: integer(),

          %% tgetent props from here
          up :: binary(),             % Move cursor up 1
          down :: binary(),           % Move cursor down 1
          left :: binary(),           % Move cursor left 1
          right :: binary(),          % Move cursor right 1
          cl :: binary(),             % Clear screen and home cursor
          cb :: binary(),             % Clear to beginning of line
          ce :: binary(),             % Clear to end of line
          dc :: binary(),             % Delete Character
          dl :: binary(),             % Delete Line
          ic :: binary(),             % Insert Character
          ip :: binary(),             % Insert Padding after inserted
          nw :: binary(),             % Insert newline (like crlf)
          down_n :: fun((integer()) -> binary()),  % Down N lines
          left_n :: fun((integer()) -> binary()), % Left N Chars
          right_n :: fun((integer()) -> binary()), % Right N chars
          up_n :: fun((integer()) -> binary()), % Up N chars
          ed:: binary(),                        % Clear to end of line
          xn :: boolean()              % Newline ignored after 80 cols
        }).

print(#term{llen = Llen, lpos = Lpos}) ->
    lists:flatten(["Lpos = ", integer_to_list(Lpos), " Llen = ", integer_to_list(Llen)]).


-spec new(binary()) -> {ok, {iolist(), #term{}}} | {error, term()}.
new(MetaLine) ->
    case binary:split(MetaLine, <<",">>, [global]) of
        [<<"cli:1">>, <<Isatty>>, Rows, Cols, Term | Termcaps] ->
            CliTerm = #term{
                         rows = parse_int(Rows),
                         cols = parse_int(Cols),
                         isatty = parse_bool(Isatty),
                         llen = 0,           % current line length
                         lpos = 0,           % current cursor position
                         term = Term
                        },
            T = parse_termcaps(Termcaps, CliTerm),
            {ok, T};
        _ ->
            {error, {invalid, MetaLine}}
    end.


%% -------------------------------------------------------------------
%% Parse the termcap definitions provided in the data the cli program
%% sends immediately after it connects.
%%
%% Definitions are comma separated using two char termcap
%% names. e.g. up, down, left, right ops could look like:
%%
%% <<"up:\e[A,do:\n,le:\b,nd:\e[C">>
%% -------------------------------------------------------------------
parse_termcaps(Termcaps, CliTerm) ->
    lists:foldl(fun(Tc, CT) -> parse_termcap(Tc, CT) end, CliTerm, Termcaps).

parse_termcap(TC, T) when is_binary(TC) ->
    [Entry, Val] = binary:split(TC, <<":">>),
    parse_termcap({binary_to_list(Entry), Val}, T);
parse_termcap({_, <<>>}, T) -> T;
parse_termcap({"up", Cmd}, T) -> T#term{up = Cmd};
parse_termcap({"do", Cmd}, T) -> T#term{down = Cmd};
parse_termcap({"le", Cmd}, T) -> T#term{left = Cmd};
parse_termcap({"nd", Cmd}, T) -> T#term{right = Cmd};
parse_termcap({"cl", Cmd}, T) -> T#term{cl = Cmd};
parse_termcap({"cb", Cmd}, T) -> T#term{cb = Cmd};
parse_termcap({"ce", Cmd}, T) -> T#term{ce = Cmd};
parse_termcap({"dc", Cmd}, T) -> T#term{dc = Cmd};
parse_termcap({"dl", Cmd}, T) -> T#term{dl = Cmd};
parse_termcap({"ic", Cmd}, T) -> T#term{ic = Cmd};
parse_termcap({"ip", Cmd}, T) -> T#term{ip = Cmd};
parse_termcap({"nw", Cmd}, T) -> T#term{nw = Cmd};
parse_termcap({"DO", Cmd}, T) -> T#term{down_n = parse_param(Cmd)};
parse_termcap({"LE", Cmd}, T) -> T#term{left_n = parse_param(Cmd)};
parse_termcap({"RI", Cmd}, T) -> T#term{right_n = parse_param(Cmd)};
parse_termcap({"UP", Cmd}, T) -> T#term{up_n = parse_param(Cmd)};
parse_termcap({"ed", Cmd}, T) -> T#term{ed = Cmd};
parse_termcap({"xn", Int}, T) -> T#term{xn = parse_bool(Int)};
parse_termcap(_, T) -> T.


parse_param(<<"\e[%p1%d", Op>>) ->
    fun(C) -> [<<"\e[">>,integer_to_list(C), Op] end.

parse_int(Bin) -> list_to_integer(binary_to_list(Bin)).

parse_bool(<<"0">>) -> false;
parse_bool($0) -> false;
parse_bool(<<"1">>) -> true;
parse_bool($1) -> true.

%%%-------------------------------------------------------------------
%% All op handling code below converted to erlang from
%% erts/emulator/drivers/unix/ttsl_drv.c. Not a great match because in
%% our case we have ready access to the line and don't need a separate buffer we need to track , but proven operation
%% sequences for common operations.
%% %-------------------------------------------------------------------

%% Send out the symbolic operations generated by cli_edlin.erl or other.
%%
%% We need the characters after the cursor, before these operations
%% are applied passed in here because some ops require us to re-write
%% the whole part of the line after the cursor

% -spec send_ops(ops(), list(char()), #term{}) -> #term{}.
send_ops(Ops, #term{} = Term) ->
    lists:mapfoldl(fun(Op, #term{} = T) -> send_op(Op, T) end, Term, Ops).

send_op({move_rel, N}, T) ->
    move_rel(N, T);
send_op({put_chars, unicode, Chars}, T) ->
    put_chars(Chars, T);
send_op({insert_chars, unicode, Chars, Aft}, T) ->
    ins_chars(Chars, Aft, T);
send_op({delete_chars, N, Aft}, T) ->
    del_chars(N, Aft, T);
send_op(crnl, T) ->
    {["\r\n[ok]\r\n"], T#term{lpos = 0, llen = 0}};
send_op(beep, T) ->
    {[16#07], T}.

col(Pos, Cols) -> Pos rem Cols.

line(Pos, Cols) -> Pos div Cols.

put_chars(Chars, #term{lpos = Lpos, llen = Llen} = T) ->
    %% Put utf8 chars
    NumChars = length(Chars),
    {unicode:characters_to_binary(Chars, utf8), T#term{lpos = Lpos + NumChars,
                                                       llen = Llen + NumChars}}.

move_rel(N, #term{lpos = Lpos, llen = Llen} = T) ->
    %% Step forwards or backwards over the buffer.
    Npos = step_over_chars(N, Lpos, Llen),
    %% io:format("move_rel by:~p NewPos:~p\r\n",[N, Npos]),

    %% Calculate move, updates pointers and move the cursor.
    move_cursor(Lpos, Npos, T).

move_cursor(From_pos, To_pos, #term{cols = Cols} = Term) ->
    From_col = cp_pos_to_col(From_pos),
    To_col = cp_pos_to_col(To_pos),
    %% io:format("move_cursor From = ~p To = ~p ~p\r\n",[From_col, To_col, Cols]),

    Dc = col(To_col, Cols) - col(From_col, Cols),
    Dl = line(To_col, Cols) - line(From_col, Cols),
    %% io:format("move_cursor Dc = ~p Dl = ~p\r\n",[Dc, Dl]),

    Vert = if Dl > 0 ->
                   move_down(Dl, Term);
              Dl < 0 ->
                   move_up(-Dl, Term);
              true ->
                   []
           end,
    Horiz = if Dc > 0 ->
                    move_right(Dc, Term);
               Dc < 0 ->
                    move_left(-Dc, Term);
               true ->
                    []
            end,
    {[Vert, Horiz], Term#term{lpos = To_pos}}.

ins_chars(Chars, Aft, #term{lpos = Lpos, llen = Llen} = T) ->
    %% Write the characters then write the whole of after again and
    %% move the cursor
    NumChars = length(Chars),
    Lpos1 = Lpos + NumChars,
    {Move, _} = move_cursor(Lpos + NumChars + length(Aft), Lpos1, T),
    {[Chars, Aft, Move],
     T#term{lpos = Lpos1,
            llen = Llen + NumChars}}.

%% Delete characters in the buffer. Can delete characters before (N < 0)
%% and after (N > 0) the current position. Cursor left at beginning of
%% deleted block.
del_chars(N, Aft, #term{lpos = Lpos, llen = Llen} = T) ->
    Pos = step_over_chars(N, Lpos, Llen),
    %% io:format("stepped over N = ~p, Lpos = ~p, Llen = ~p, Pos = ~p~n",
   %%           [N, Lpos, Llen, Pos]),
    if Pos > Lpos ->
            %% Deleting after cursor (forward delete)
            L = Pos - Lpos,
            Llen1 = Llen - L,
            Blanks = lists:duplicate(L, $\s),
            {MoveBack, T1} = move_cursor(Llen, Lpos, T),
            {[Aft, Blanks, MoveBack], T1#term{llen = Llen1}};
       Pos < Lpos ->
            %% Deleting before cursor
            L = Lpos - Pos,
            Lpos1 = Lpos - L,
            Llen1 = Llen - L,
            %% Write out characters after, blank the tail and jump back to lpos.
            Blanks = lists:duplicate(L, $\s),
            {MoveBack, T1} = move_cursor(Lpos, Pos, T),
            {MoveFromEnd, T2} = move_cursor(Llen, Lpos1, T1),
            {[MoveBack, Aft, Blanks, MoveFromEnd],
             T2#term{llen = Llen1, lpos = Pos}};
       true ->
            {[], T}
    end.

%% Step over N logical characters, check for overflow
step_over_chars(N, Lpos, _Llen) when N < 0 ->
    max(0, Lpos + N);
step_over_chars(N, Lpos, Llen) when N > 0 ->
    min(Llen, Lpos + N);
step_over_chars(_N, Lpos, _Llen) ->
    Lpos.

move_left(N, #term{left = Left}) ->
    tputs(N, Left).

move_right(N, #term{right = Right}) ->
    tputs(N, Right).

move_up(N, #term{up = Up}) ->
    tputs(N, Up).

move_down(N, #term{down = Down}) ->
    tputs(N, Down).


%% Fixme - figure out whether we need to do this from ttsl_drv.c
cp_pos_to_col(Cp_pos) ->
    Cp_pos.

tputs(N, Ops) ->
    lists:duplicate(N, Ops).
