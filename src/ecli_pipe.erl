%%%-------------------------------------------------------------------
%%% @doc Pipe catalogs and the post-command pipe pipeline.
%%%
%%% `|` switches from the command tree into a catalog of pipe commands
%%% declared on `#cmd.pipes`. Format pipes (`display xml`) select a
%%% serializer for `{data, Tree}` results. Line pipes (`match`, `except`,
%%% `count`) run left-to-right on the rendered text.
%%%
%%% `display` is a renderer choice (last one wins), not a stream filter.
%%% @end
%%%-------------------------------------------------------------------
-module(ecli_pipe).

-include("../include/ecli.hrl").

-export([show_pipes/0, config_show_pipes/0, set_pipes/0]).
-export([catalog/1, apply/2, wants_defaults/1, wants_compare/1, compare_against/1,
         insert_where/1]).

%%--------------------------------------------------------------------
%% Catalogs
%%--------------------------------------------------------------------

%% @doc Pipes for operational show commands.
show_pipes() ->
    [display_cmd(operational), match_cmd(), except_cmd(), count_cmd()].

%% @doc Pipes for configuration show; adds `display set` and `compare`.
config_show_pipes() ->
    [display_cmd(config), compare_cmd(), match_cmd(), except_cmd(), count_cmd()].

%% @doc Pipes for `set`. Position modifiers for `ordered-by user` lists
%% and leaf-lists. Consumed by the set action (`insert_where/1`), not
%% by the output pipeline. Names match RFC 8040 `insert`.
set_pipes() ->
    [first_cmd(), last_cmd(), before_cmd(), after_cmd()].

first_cmd() ->
    #cmd{name = "first",
         desc = "Insert at the start of the user-ordered list",
         action = {pipe, {insert, first}}}.

last_cmd() ->
    #cmd{name = "last",
         desc = "Insert at the end of the user-ordered list",
         action = {pipe, {insert, last}}}.

before_cmd() ->
    #{role => cmd,
      node_type => leaf,
      name => "before",
      desc => "Insert before an existing list entry",
      type => string,
      action => {pipe, {insert, before}}}.

after_cmd() ->
    #{role => cmd,
      node_type => leaf,
      name => "after",
      desc => "Insert after an existing list entry",
      type => string,
      action => {pipe, {insert, 'after'}}}.

%% @doc Expand a `#cmd.pipes` field to a list of maps.
catalog(undefined) ->
    [];
catalog([]) ->
    [];
catalog(Fun) when is_function(Fun, 0) ->
    [to_map(C) || C <- Fun()];
catalog(Fun) when is_function(Fun, 1) ->
    [to_map(C) || C <- Fun([])];
catalog(List) when is_list(List) ->
    [to_map(C) || C <- List].

to_map(#cmd{} = C) ->
    ecli_util:cmd_to_map(C);
to_map(M) when is_map(M) ->
    M.

display_cmd(Kind) ->
    #cmd{name = "display",
         desc = "Display options",
         children = fun() -> display_children(Kind) end}.

display_children(operational) ->
    display_xml_json();
display_children(config) ->
    display_xml_json() ++
        [#cmd{name = "set",
              desc = "Display as set commands",
              action = {pipe, {display, set}}}].

display_xml_json() ->
    [#cmd{name = "xml",
          desc = "Display as XML",
          action = {pipe, {display, xml}}},
     #cmd{name = "json",
          desc = "Display as JSON",
          action = {pipe, {display, json}}},
     #cmd{name = "defaults",
          desc = "Include schema default values",
          action = {pipe, {display, defaults}}}].

match_cmd() ->
    #{role => cmd,
      node_type => leaf,
      name => "match",
      desc => "Show only text that matches a pattern",
      type => string,
      action => {pipe, match}}.

except_cmd() ->
    #{role => cmd,
      node_type => leaf,
      name => "except",
      desc => "Show only text that does not match a pattern",
      type => string,
      action => {pipe, except}}.

count_cmd() ->
    #cmd{name = "count",
         desc = "Count the number of lines in the output",
         action = {pipe, count}}.

compare_cmd() ->
    #cmd{name = "compare",
         desc = "Show changes in the current configuration session",
         action = {pipe, compare},
         children = fun compare_children/0}.

compare_children() ->
    [#{role => cmd,
       node_type => leaf,
       name => "rollback",
       desc => "Compare against a numbered rollback snapshot",
       type => integer,
       action => {pipe, {compare, rollback}}}].

%%--------------------------------------------------------------------
%% Apply
%%--------------------------------------------------------------------

%% @doc Run pipe stages against an action result.
%% Result is `{data, Tree}` or an iolist/string.
%% Returns an iolist or `{error, Reason}`.
-spec apply(term(), list()) -> iodata() | {error, string()}.
apply(Result, Stages) ->
    {Format, Filters} = classify(Stages, curly, []),
    case render(Result, Format) of
        {error, _Reason} = Err ->
            Err;
        Text ->
            filter_text(Text, Filters)
    end.

%% @doc True if any pipe stage is `display defaults`.
-spec wants_defaults(list()) -> boolean().
wants_defaults(Stages) ->
    lists:any(fun(Stage) -> stage_op(Stage) =:= {display, defaults} end,
              Stages).

%% @doc True if any pipe stage is `compare` (with or without `rollback N`).
%% The action should return session-diff text (an iolist), not `{data, Tree}`.
-spec wants_compare(list()) -> boolean().
wants_compare(Stages) ->
    compare_against(Stages) =/= false.

%% @doc Baseline for a `compare` pipe.
%% `false` if the command is not a compare; `session` for bare `compare`;
%% `{rollback, N}` for `compare rollback N`.
-spec compare_against(list()) -> false | session | {rollback, integer()}.
compare_against([]) ->
    false;
compare_against([Stage | Rest]) ->
    case stage_op(Stage) of
        compare ->
            session;
        {compare, Against} ->
            Against;
        _ ->
            compare_against(Rest)
    end.

%% @doc Position for an `ordered-by user` `set`. `undefined` if the
%% command has no insert pipe (RFC default: append / last).
-spec insert_where(list()) ->
          undefined | first | last | {before, tuple()} | {'after', tuple()}.
insert_where([]) ->
    undefined;
insert_where([Stage | Rest]) ->
    case stage_op(Stage) of
        {insert, first} ->
            first;
        {insert, last} ->
            last;
        {insert, before} ->
            {before, insert_point(Stage)};
        {insert, 'after'} ->
            {'after', insert_point(Stage)};
        {insert, Where} ->
            Where;
        _ ->
            insert_where(Rest)
    end.

insert_point(Stage) ->
    {insert_point_value(lists:reverse(Stage))}.

insert_point_value([#{value := V} | _]) when is_list(V); is_binary(V); is_integer(V) ->
    point_str(V);
insert_point_value([#{name := Name, action := {pipe, {insert, _}}} | _]) ->
    Name;
insert_point_value([#{name := Name} | _]) ->
    Name;
insert_point_value(_) ->
    "".

point_str(V) when is_list(V) -> V;
point_str(V) when is_binary(V) -> unicode:characters_to_list(V);
point_str(V) when is_integer(V) -> integer_to_list(V).

classify([], Format, Filters) ->
    {Format, lists:reverse(Filters)};
classify([Stage | Rest], Format, Filters) ->
    case stage_op(Stage) of
        {display, defaults} ->
            %% Fill defaults in the action; render as curly unless a
            %% later display xml/json/set overrides.
            classify(Rest, curly, Filters);
        compare ->
            %% Action returns compare text; keep curly so iolist passes through.
            classify(Rest, curly, Filters);
        {compare, _} ->
            classify(Rest, curly, Filters);
        {insert, _} ->
            %% Edit modifier; the set action already applied it.
            classify(Rest, Format, Filters);
        {display, F} ->
            classify(Rest, F, Filters);
        {match, Pat} ->
            classify(Rest, Format, [{match, Pat} | Filters]);
        {except, Pat} ->
            classify(Rest, Format, [{except, Pat} | Filters]);
        count ->
            classify(Rest, Format, [count | Filters]);
        unknown ->
            classify(Rest, Format, Filters)
    end.

stage_op(Stage) when is_list(Stage) ->
    stage_op_nodes(lists:reverse(Stage)).

stage_op_nodes([#{action := {pipe, {display, F}}} | _]) ->
    {display, F};
stage_op_nodes([#{action := {pipe, {compare, {rollback, N}}}} | _])
  when is_integer(N) ->
    {compare, {rollback, N}};
stage_op_nodes([#{action := {pipe, {compare, rollback}}, value := N} | _])
  when is_integer(N) ->
    {compare, {rollback, N}};
stage_op_nodes([#{value := N}, #{action := {pipe, {compare, rollback}}} | _])
  when is_integer(N) ->
    {compare, {rollback, N}};
stage_op_nodes([#{action := {pipe, {insert, Where}}} | _]) ->
    {insert, Where};
stage_op_nodes([#{action := {pipe, compare}} | _]) ->
    compare;
stage_op_nodes([#{action := {pipe, match}, value := Pat} | _]) ->
    {match, Pat};
stage_op_nodes([#{action := {pipe, except}, value := Pat} | _]) ->
    {except, Pat};
stage_op_nodes([#{action := {pipe, count}} | _]) ->
    count;
stage_op_nodes([#{value := Pat}, #{action := {pipe, match}} | _]) ->
    {match, Pat};
stage_op_nodes([#{value := Pat}, #{action := {pipe, except}} | _]) ->
    {except, Pat};
stage_op_nodes(_) ->
    unknown.

render({data, Tree}, xml) ->
    format_xml_tree(Tree);
render({data, Tree}, json) ->
    format_json_tree(Tree);
render({data, Tree}, set) ->
    format_set_tree(Tree);
render({data, Tree}, curly) ->
    ecli:format_simple_tree(Tree);
render({data, Tree}, defaults) ->
    ecli:format_simple_tree(Tree);
render(_Text, Format) when Format =/= curly ->
    {error, "Command does not support this display format"};
render(Text, curly) ->
    Text.

filter_text(Text, []) ->
    Text;
filter_text(Text, Filters) ->
    case apply_filters(split_lines(Text), Filters) of
        {error, _Reason} = Err ->
            Err;
        Lines ->
            join_lines(Lines)
    end.

apply_filters(Lines, []) ->
    Lines;
apply_filters(Lines, [{match, Pat} | Rest]) ->
    case compile_re(Pat) of
        {error, _} = Err ->
            Err;
        {ok, RE} ->
            apply_filters([L || L <- Lines, re:run(L, RE, [{capture, none}]) =:= match], Rest)
    end;
apply_filters(Lines, [{except, Pat} | Rest]) ->
    case compile_re(Pat) of
        {error, _} = Err ->
            Err;
        {ok, RE} ->
            apply_filters([L || L <- Lines, re:run(L, RE, [{capture, none}]) =:= nomatch], Rest)
    end;
apply_filters(Lines, [count | Rest]) ->
    N = length(Lines),
    CountLine = ["Count: ", integer_to_list(N), " lines"],
    apply_filters([CountLine], Rest).

compile_re(Pat) ->
    case re:compile(Pat) of
        {ok, _} = Ok ->
            Ok;
        {error, _} ->
            {error, "Invalid regular expression"}
    end.

split_lines(Text) ->
    Bin = to_bin(Text),
    Parts = binary:split(Bin, [<<"\r\n">>, <<"\n">>], [global]),
    drop_trailing_empty(Parts).

drop_trailing_empty([]) ->
    [];
drop_trailing_empty(Parts) ->
    case lists:last(Parts) of
        <<>> ->
            lists:droplast(Parts);
        _ ->
            Parts
    end.

join_lines([]) ->
    "";
join_lines(Lines) ->
    lists:join("\r\n", [to_bin(L) || L <- Lines]) ++ ["\r\n"].

to_bin(B) when is_binary(B) ->
    B;
to_bin(Io) ->
    unicode:characters_to_binary(Io).

%%--------------------------------------------------------------------
%% Formatters — same tree shape as ecli:format_simple_tree/1
%% [{Name, {value, Val}} | {Name, {leaf_list, [Val]}} | {Name, Children}]
%% Hand-rolled JSON/XML so this builds on OTP 24 (no json:format/3,
%% no xmerl_xml_indent).
%%--------------------------------------------------------------------

format_xml_tree(Tree) ->
    crlf(["<config>\n", xml_elems(Tree, 1), "</config>\n"]).

xml_elems([{Name, {value, Val}} | Ts], Lvl) ->
    Tag = xml_name(Name),
    [indent(Lvl), $<, Tag, $>, xml_escape(fmt_value(Val)), $<, $/, Tag, $>, $\n
     | xml_elems(Ts, Lvl)];
xml_elems([{Name, {leaf_list, Vals}} | Ts], Lvl) ->
    Tag = xml_name(Name),
    [[indent(Lvl), $<, Tag, $>, xml_escape(fmt_value(V)), $<, $/, Tag, $>, $\n]
     || V <- Vals] ++ xml_elems(Ts, Lvl);
xml_elems([{Name, Children} | Ts], Lvl) ->
    Tag = xml_name(Name),
    [indent(Lvl), $<, Tag, $>, $\n,
     xml_elems(Children, Lvl + 1),
     indent(Lvl), $<, $/, Tag, $>, $\n
     | xml_elems(Ts, Lvl)];
xml_elems([], _) ->
    [].

xml_name(Name) ->
    Raw = fmt_name(Name),
    San = [sanitize_xml_char(C) || C <- Raw],
    case San of
        [C | _] when C >= $0, C =< $9 ->
            [$n | San];
        [] ->
            "node";
        _ ->
            San
    end.

sanitize_xml_char(C) when C >= $a, C =< $z -> C;
sanitize_xml_char(C) when C >= $A, C =< $Z -> C;
sanitize_xml_char(C) when C >= $0, C =< $9 -> C;
sanitize_xml_char($_) -> $_;
sanitize_xml_char($-) -> $-;
sanitize_xml_char($.) -> $.;
sanitize_xml_char(_) -> $_.

xml_escape([]) ->
    [];
xml_escape([$< | T]) ->
    "&lt;" ++ xml_escape(T);
xml_escape([$> | T]) ->
    "&gt;" ++ xml_escape(T);
xml_escape([$& | T]) ->
    "&amp;" ++ xml_escape(T);
xml_escape([$" | T]) ->
    "&quot;" ++ xml_escape(T);
xml_escape([C | T]) when is_integer(C) ->
    [C | xml_escape(T)].

format_json_tree(Tree) ->
    crlf([json_encode(json_object(Tree), 0), $\n]).

%% `{object, Pairs}` keeps tree order and distinguishes `{}` from `[]`.
json_object(Tree) ->
    {object, [json_pair(P) || P <- Tree]}.

json_pair({Name, {value, Val}}) ->
    {json_key(Name), json_encode_value(Val)};
json_pair({Name, {leaf_list, Vals}}) ->
    {json_key(Name), [json_encode_value(V) || V <- Vals]};
json_pair({Name, Children}) when is_list(Children) ->
    {json_key(Name), json_object(Children)}.

json_key(Name) ->
    unicode:characters_to_binary(fmt_name(Name)).

json_encode_value(Bin) when is_binary(Bin) ->
    Bin;
json_encode_value(Int) when is_integer(Int) ->
    Int;
json_encode_value(Float) when is_float(Float) ->
    Float;
json_encode_value(true) ->
    true;
json_encode_value(false) ->
    false;
json_encode_value(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);
json_encode_value(Val) ->
    unicode:characters_to_binary(fmt_value(Val)).

json_encode({object, []}, _Lvl) ->
    <<"{}">>;
json_encode({object, Pairs}, Lvl) ->
    Next = Lvl + 1,
    [${, $\n, json_encode_pairs(Pairs, Next), $\n, indent(Lvl), $}];
json_encode(List, _Lvl) when is_list(List) ->
    [$[, lists:join(", ", [json_encode(V, 0) || V <- List]), $]];
json_encode(Bin, _Lvl) when is_binary(Bin) ->
    [$", json_escape(Bin), $"];
json_encode(Int, _Lvl) when is_integer(Int) ->
    integer_to_binary(Int);
json_encode(Float, _Lvl) when is_float(Float) ->
    float_to_binary(Float, [{decimals, 15}, compact]);
json_encode(true, _Lvl) ->
    <<"true">>;
json_encode(false, _Lvl) ->
    <<"false">>.

json_encode_pairs([Pair], Lvl) ->
    json_encode_pair(Pair, Lvl);
json_encode_pairs([Pair | Rest], Lvl) ->
    [json_encode_pair(Pair, Lvl), $,, $\n, json_encode_pairs(Rest, Lvl)].

json_encode_pair({Key, Val}, Lvl) ->
    [indent(Lvl), $", json_escape(Key), "\": ", json_encode(Val, Lvl)].

json_escape(Bin) when is_binary(Bin) ->
    json_escape(Bin, <<>>).

json_escape(<<>>, Acc) ->
    Acc;
json_escape(<<$", Rest/binary>>, Acc) ->
    json_escape(Rest, <<Acc/binary, "\\\"">>);
json_escape(<<$\\, Rest/binary>>, Acc) ->
    json_escape(Rest, <<Acc/binary, "\\\\">>);
json_escape(<<$\n, Rest/binary>>, Acc) ->
    json_escape(Rest, <<Acc/binary, "\\n">>);
json_escape(<<$\r, Rest/binary>>, Acc) ->
    json_escape(Rest, <<Acc/binary, "\\r">>);
json_escape(<<$\t, Rest/binary>>, Acc) ->
    json_escape(Rest, <<Acc/binary, "\\t">>);
json_escape(<<C, Rest/binary>>, Acc) when C < 32 ->
    Hex = iolist_to_binary(io_lib:format("\\u~4.16.0b", [C])),
    json_escape(Rest, <<Acc/binary, Hex/binary>>);
json_escape(<<C, Rest/binary>>, Acc) ->
    json_escape(Rest, <<Acc/binary, C>>).

indent(Lvl) ->
    lists:duplicate(2 * Lvl, $\s).

crlf(Io) ->
    binary:replace(iolist_to_binary(Io), <<"\n">>, <<"\r\n">>, [global]).

format_set_tree(Tree) ->
    format_set_tree(Tree, []).

format_set_tree([{Name, {value, Val}} | Ts], Path) ->
    LinePath = Path ++ [fmt_name(Name)],
    ["set ", lists:join(" ", LinePath), " ", fmt_value(Val), "\r\n",
     format_set_tree(Ts, Path)];
format_set_tree([{Name, {leaf_list, Vals}} | Ts], Path) ->
    LinePath = Path ++ [fmt_name(Name)],
    [[ "set ", lists:join(" ", LinePath), " ", fmt_value(V), "\r\n"] || V <- Vals]
        ++ format_set_tree(Ts, Path);
format_set_tree([{Name, Children} | Ts], Path) ->
    [format_set_tree(Children, Path ++ [fmt_name(Name)]),
     format_set_tree(Ts, Path)];
format_set_tree([], _) ->
    [].

fmt_name(T) when is_tuple(T) ->
    lists:flatten(lists:join(" ", [fmt_name(X) || X <- tuple_to_list(T)]));
fmt_name(N) when is_atom(N) ->
    atom_to_list(N);
fmt_name(N) when is_binary(N) ->
    unicode:characters_to_list(N);
fmt_name(N) when is_integer(N) ->
    integer_to_list(N);
fmt_name(N) when is_list(N) ->
    N.

fmt_value(Bin) when is_binary(Bin) ->
    unicode:characters_to_list(Bin);
fmt_value(Int) when is_integer(Int) ->
    integer_to_list(Int);
fmt_value(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);
fmt_value({A, B, C, D}) when is_integer(A), is_integer(B),
                             is_integer(C), is_integer(D) ->
    lists:flatten(io_lib:format("~p.~p.~p.~p", [A, B, C, D]));
fmt_value(Else) when is_list(Else) ->
    Else;
fmt_value(Else) ->
    lists:flatten(io_lib:format("~p", [Else])).
