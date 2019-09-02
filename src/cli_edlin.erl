%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Line editing. Based mostly on erlang edlin.erl
%%% @end
%%% Created : 16 Aug 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cli_edlin).

-export([start/1, insert/2]).

-record(edlin,
        {
          line = {[], []},       % {Chars_before_cursor, Chars_after}
          state = none,
          prompt,
          requests = []
        }).

%%--------------------------------------------------------------------
%% Create a new instance of edlin.
%% --------------------------------------------------------------------
-spec start(Prompt::string()) -> #edlin{}.
start(Prompt) when is_list(Prompt) ->
    {#edlin{prompt = Prompt}, [{put_chars,unicode,Prompt}]}.

insert([C|Cs], #edlin{state = State, line = {Bef, Aft},
                      requests = Rs0, prompt = Prompt} = Ed) ->
    case key_map(C, State) of
        meta ->
            insert(Cs, Ed#edlin{state = meta});
        meta_o ->
            insert(Cs, Ed#edlin{state = meta_o});
        meta_csi ->
            insert(Cs, Ed#edlin{state = meta_csi});
        meta_meta ->
            insert(Cs, Ed#edlin{state = meta_meta});
        {csi, _} = Csi ->
            insert(Cs, Ed#edlin{state = Csi});
        meta_left_sq_bracket ->
            insert(Cs, Ed#edlin{state = meta_left_sq_bracket});
        new_line ->
            {done, get_line(Bef, Aft), Cs,
             lists:reverse(Rs0, [{move_rel,cp_len(Aft)}, crnl])};
        redraw_line ->
	    Rs1 = erase(Prompt, Bef, Aft, Rs0),
	    Rs = redraw(Prompt, Bef, Aft, Rs1),
	    insert(Cs, Ed#edlin{state = none, requests = Rs});
        tab_expand ->
            {expand, Bef, Cs, Ed#edlin{state = none}};
        {undefined,C} ->
            insert(Cs, Ed#edlin{state = none});
        Op ->
            case do_op(Op, Bef, Aft, Rs0) of
                {Line, Rs, Mode} -> % allow custom modes from do_op
                    insert(Cs, Ed#edlin{line = Line, state = Mode,
                                           requests = Rs});
                {Line, Rs} ->
                    insert(Cs, Ed#edlin{line = Line, state = none, requests = Rs})
            end
    end;
insert([], #edlin{requests = Rs} = Ed) ->
    {more_chars,Ed#edlin{requests = [], state = none},lists:reverse(Rs)};
insert(eof, #edlin{line = {Bef, Aft}, requests = Rs} = Ed) ->
    {done, get_line(Bef, Aft), Ed#edlin{line = {[],[]}}, lists:reverse(Rs, [{move_rel,cp_len(Aft)}])}.

%% prefix_arg(Argument)
%%  Take a prefix argument and return its numeric value.
prefix_arg(none) -> 1;
prefix_arg({ctlu,N}) -> N;
prefix_arg(N) -> N.

%% key_map(Char, Prefix)
%%  Map a character and a prefix to an action.
key_map($\^A, none) -> beginning_of_line;
key_map($\^B, none) -> backward_char;
key_map($\^D, none) -> forward_delete_char;
key_map($\^E, none) -> end_of_line;
key_map($\^F, none) -> forward_char;
key_map($\^H, none) -> backward_delete_char;
key_map($\t, none) -> tab_expand;
key_map($\^L, none) -> redraw_line;
key_map($\n, none) -> new_line;
key_map($\^K, none) -> kill_line;
key_map($\r, none) -> new_line;
key_map($\^T, none) -> transpose_char;
key_map($\^U, none) -> ctlu;
key_map($\^], none) -> auto_blink;
key_map($\^X, none) -> ctlx;
key_map($\^Y, none) -> yank;
key_map($\^W, none) -> backward_kill_word;
key_map($\e, none) -> meta;
key_map($), Prefix) when Prefix =/= meta,
                         Prefix =/= search,
                         Prefix =/= search_meta -> {blink,$),$(};
key_map($}, Prefix) when Prefix =/= meta,
                         Prefix =/= search,
                         Prefix =/= search_meta -> {blink,$},${};
key_map($], Prefix) when Prefix =/= meta,
                         Prefix =/= search,
                         Prefix =/= search_meta -> {blink,$],$[};
key_map($B, meta) -> backward_word;
key_map($D, meta) -> kill_word;
key_map($F, meta) -> forward_word;
key_map($T, meta) -> transpose_word;
key_map($Y, meta) -> yank_pop;
key_map($b, meta) -> backward_word;
key_map($d, meta) -> kill_word;
key_map($f, meta) -> forward_word;
key_map($t, meta) -> transpose_word;
key_map($y, meta) -> yank_pop;
key_map($O, meta) -> meta_o;
key_map($H, meta_o) -> beginning_of_line;
key_map($F, meta_o) -> end_of_line;
key_map($\177, none) -> backward_delete_char;
key_map($\177, meta) -> backward_kill_word;
key_map($[, meta) -> meta_left_sq_bracket;
key_map($H, meta_left_sq_bracket) -> beginning_of_line;
key_map($F, meta_left_sq_bracket) -> end_of_line;
key_map($D, meta_left_sq_bracket) -> backward_char;
key_map($C, meta_left_sq_bracket) -> forward_char;
% support a few <CTRL>+<CURSOR LEFT|RIGHT> combinations...
%  - forward:  \e\e[C, \e[5C, \e[1;5C
%  - backward: \e\e[D, \e[5D, \e[1;5D
key_map($\e, meta) -> meta_meta;
key_map($[, meta_meta) -> meta_csi;
key_map($C, meta_csi) -> forward_word;
key_map($D, meta_csi) -> backward_word;
key_map($1, meta_left_sq_bracket) -> {csi, "1"};
key_map($3, meta_left_sq_bracket) -> {csi, "3"};
key_map($5, meta_left_sq_bracket) -> {csi, "5"};
key_map($5, {csi, "1;"}) -> {csi, "1;5"};
key_map($~, {csi, "3"}) -> forward_delete_char;
key_map($C, {csi, "5"}) -> forward_word;
key_map($C, {csi, "1;5"}) -> forward_word;
key_map($D, {csi, "5"})  -> backward_word;
key_map($D, {csi, "1;5"}) -> backward_word;
key_map($;, {csi, "1"}) -> {csi, "1;"};
key_map(C, none) when C >= $\s ->
    {insert,C};
%% for search, we need smarter line handling and so
%% we cheat a bit on the dispatching, and allow to
%% return a mode.
key_map($\^H, search) -> {search, backward_delete_char};
key_map($\177, search) -> {search, backward_delete_char};
key_map($\^R, search) -> {search, skip_up};
key_map($\^S, search) -> {search, skip_down};
key_map($\n, search) -> {search, search_found};
key_map($\r, search) -> {search, search_found};
key_map($\^A, search) -> {search, search_quit};
key_map($\^B, search) -> {search, search_quit};
key_map($\^D, search) -> {search, search_quit};
key_map($\^E, search) -> {search, search_quit};
key_map($\^F, search) -> {search, search_quit};
key_map($\t,  search) -> {search, search_quit};
key_map($\^L, search) -> {search, search_quit};
key_map($\^T, search) -> {search, search_quit};
key_map($\^U, search) -> {search, search_quit};
key_map($\^], search) -> {search, search_quit};
key_map($\^X, search) -> {search, search_quit};
key_map($\^Y, search) -> {search, search_quit};
key_map($\e,  search) -> search_meta;
key_map($[,  search_meta) -> search_meta_left_sq_bracket;
key_map(_, search_meta) -> {search, search_quit};
key_map(_C, search_meta_left_sq_bracket) -> {search, search_quit};
key_map(C, search) -> {insert_search,C};
key_map(C, _) -> {undefined,C}.

%% do_op(Action, Before, After, Requests)
%% Before and After are of lists of type string:grapheme_cluster()
do_op({insert,C}, [], [], Rs) ->                % 1st char empty line
    {{[C],[]},[{put_chars, unicode,[C]}|Rs]};
do_op({insert,C}, [Bef|Bef0], [], Rs) ->        % Append at end of line
    case string:to_graphemes([Bef,C]) of
        [GC] -> {{[GC|Bef0],[]},[{put_chars, unicode,[C]}|Rs]};
        _ -> {{[C,Bef|Bef0],[]},[{put_chars, unicode,[C]}|Rs]}
    end;
do_op({insert,C}, [], Aft, Rs) ->               % insert at start of line
    {{[C],Aft},[{insert_chars, unicode,[C], Aft}|Rs]};
do_op({insert,C}, [Bef|Bef0], Aft, Rs) ->       % insert in middle of line
    case string:to_graphemes([Bef,C]) of
        [GC] -> {{[GC|Bef0],Aft},[{insert_chars, unicode,[C], Aft}|Rs]};
        _ -> {{[C,Bef|Bef0],Aft},[{insert_chars, unicode,[C], Aft}|Rs]}
    end;
do_op(forward_delete_char, Bef, [GC|Aft], Rs) ->
    {{Bef,Aft},[{delete_chars,gc_len(GC), Aft}|Rs]};
do_op(backward_delete_char, [GC|Bef], Aft, Rs) ->
    {{Bef,Aft},[{delete_chars,-gc_len(GC), Aft}|Rs]};
do_op(transpose_char, [C1,C2|Bef], [], Rs) ->
    Len = gc_len(C1)+gc_len(C2),
    {{[C2,C1|Bef],[]},[{put_chars, unicode,[C1,C2]},{move_rel,-Len}|Rs]};
do_op(transpose_char, [C2|Bef], [C1|Aft], Rs) ->
    Len = gc_len(C2),
    {{[C2,C1|Bef],Aft},[{put_chars, unicode,[C1,C2]},{move_rel,-Len}|Rs]};
do_op(kill_word, Bef, Aft0, Rs) ->
    {Aft1,Kill0,N0} = over_non_word(Aft0, [], 0),
    {Aft,Kill,N} = over_word(Aft1, Kill0, N0),
    put(kill_buffer, lists:reverse(Kill)),
    {{Bef,Aft},[{delete_chars,N, Aft}|Rs]};
do_op(backward_kill_word, Bef0, Aft, Rs) ->
    {Bef1,Kill0,N0} = over_non_word(Bef0, [], 0),
    {Bef,Kill,N} = over_word(Bef1, Kill0, N0),
    put(kill_buffer, Kill),
    {{Bef,Aft},[{delete_chars,-N, Aft}|Rs]};
do_op(kill_line, Bef, Aft, Rs) ->
    put(kill_buffer, Aft),
    {{Bef,[]},[{delete_chars,cp_len(Aft), []}|Rs]};
do_op(yank, Bef, [], Rs) ->
    Kill = get(kill_buffer),
    {{lists:reverse(Kill, Bef),[]},[{put_chars, unicode,Kill}|Rs]};
do_op(yank, Bef, Aft, Rs) ->
    Kill = get(kill_buffer),
    {{lists:reverse(Kill, Bef),Aft},[{insert_chars, unicode,Kill, Aft}|Rs]};
do_op(forward_char, Bef, [C|Aft], Rs) ->
    {{[C|Bef],Aft},[{move_rel,gc_len(C)}|Rs]};
do_op(backward_char, [C|Bef], Aft, Rs) ->
    {{Bef,[C|Aft]},[{move_rel,-gc_len(C)}|Rs]};
do_op(forward_word, Bef0, Aft0, Rs) ->
    {Aft1,Bef1,N0} = over_non_word(Aft0, Bef0, 0),
    {Aft,Bef,N} = over_word(Aft1, Bef1, N0),
    {{Bef,Aft},[{move_rel,N}|Rs]};
do_op(backward_word, Bef0, Aft0, Rs) ->
    {Bef1,Aft1,N0} = over_non_word(Bef0, Aft0, 0),
    {Bef,Aft,N} = over_word(Bef1, Aft1, N0),
    {{Bef,Aft},[{move_rel,-N}|Rs]};
do_op(beginning_of_line, [_|_]=Bef, Aft, Rs) ->
    {{[],lists:reverse(Bef, Aft)},[{move_rel,-(cp_len(Bef))}|Rs]};
do_op(beginning_of_line, [], Aft, Rs) ->
    {{[],Aft},Rs};
do_op(end_of_line, Bef, [_|_]=Aft, Rs) ->
    {{lists:reverse(Aft, Bef),[]},[{move_rel,cp_len(Aft)}|Rs]};
do_op(end_of_line, Bef, [], Rs) ->
    {{Bef,[]},Rs};
do_op(ctlu, Bef, Aft, Rs) ->
    put(kill_buffer, lists:reverse(Bef)),
    {{[], Aft}, [{delete_chars, -cp_len(Bef), Aft} | Rs]};
do_op(beep, Bef, Aft, Rs) ->
    {{Bef,Aft},[beep|Rs]};
do_op(_, Bef, Aft, Rs) ->
    {{Bef,Aft},[beep|Rs]}.


over_word(Cs, Stack, N) ->
    L = length([1 || $\' <- Cs]),
    case L rem 2 of
	0 ->
	    over_word1(Cs, Stack, N);
	1 ->
	    until_quote(Cs, Stack, N)
    end.

until_quote([$\'|Cs], Stack, N) ->
    {Cs, [$\'|Stack], N+1};
until_quote([C|Cs], Stack, N) ->
    until_quote(Cs, [C|Stack], N+gc_len(C)).

over_word1([$\'=C|Cs], Stack, N) ->
    until_quote(Cs, [C|Stack], N+1);
over_word1(Cs, Stack, N) ->
    over_word2(Cs, Stack, N).

over_word2([C|Cs], Stack, N) ->
    case word_char(C) of
	true -> over_word2(Cs, [C|Stack], N+gc_len(C));
	false -> {[C|Cs],Stack,N}
    end;
over_word2([], Stack, N) when is_integer(N) ->
    {[],Stack,N}.

over_non_word([C|Cs], Stack, N) ->
    case word_char(C) of
	true -> {[C|Cs],Stack,N};
	false -> over_non_word(Cs, [C|Stack], N+gc_len(C))
    end;
over_non_word([], Stack, N) ->
    {[],Stack,N}.

word_char(C) when C >= $A, C =< $Z -> true;
word_char(C) when C >= $À, C =< $Þ, C =/= $× -> true;
word_char(C) when C >= $a, C =< $z -> true;
word_char(C) when C >= $ß, C =< $ÿ, C =/= $÷ -> true;
word_char(C) when C >= $0, C =< $9 -> true;
word_char(C) when C =:= $_ -> true;
word_char([_|_]) -> true; %% Is grapheme
word_char(_) -> false.

erase_line({line,Pbs,{Bef,Aft},_}) ->
    lists:reverse(erase(Pbs, Bef, Aft, [])).

erase_inp({line,_,{Bef,Aft},_}) ->
    lists:reverse(erase([], Bef, Aft, [])).

erase(Pbs, Bef, Aft, Rs) ->
    [{delete_chars,-cp_len(Pbs)-cp_len(Bef),[]},{delete_chars,cp_len(Aft),[]}|Rs].

redraw_line({line,Pbs,{Bef,Aft},_}) ->
    lists:reverse(redraw(Pbs, Bef, Aft, [])).

redraw(Pbs, Bef, Aft, Rs) ->
    [{move_rel,-cp_len(Aft)},{put_chars, unicode,lists:reverse(Bef, Aft)},{put_chars, unicode,Pbs}|Rs].

length_before({line,Pbs,{Bef,_Aft},_}) ->
    cp_len(Pbs) + cp_len(Bef).

length_after({line,_,{_Bef,Aft},_}) ->
    cp_len(Aft).

prompt({line,Pbs,_,_}) ->
    Pbs.

current_line({line,_,{Bef, Aft},_}) ->
    get_line(Bef, Aft ++ "\n").

current_chars({line,_,{Bef,Aft},_}) ->
    get_line(Bef, Aft).

get_line(Bef, Aft) ->
    unicode:characters_to_list(lists:reverse(Bef, Aft)).

%% Grapheme length in codepoints
gc_len(CP) when is_integer(CP) -> 1;
gc_len(CPs) when is_list(CPs) -> length(CPs).

%% String length in codepoints
cp_len(Str) ->
    cp_len(Str, 0).

cp_len([GC|R], Len) ->
    cp_len(R, Len + gc_len(GC));
cp_len([], Len) -> Len.

