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

-export([show_pipes/0, config_show_pipes/0]).
-export([catalog/1, apply/2]).

%%--------------------------------------------------------------------
%% Catalogs
%%--------------------------------------------------------------------

%% @doc Pipes for operational show commands.
show_pipes() ->
    [display_cmd(operational), match_cmd(), except_cmd(), count_cmd()].

%% @doc Pipes for configuration show; adds `display set`.
config_show_pipes() ->
    [display_cmd(config), match_cmd(), except_cmd(), count_cmd()].

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
          action = {pipe, {display, json}}}].

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

classify([], Format, Filters) ->
    {Format, lists:reverse(Filters)};
classify([Stage | Rest], Format, Filters) ->
    case stage_op(Stage) of
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
%% JSON via json:format/3; XML via xmerl:export_simple/3.
%%--------------------------------------------------------------------

format_xml_tree(Tree) ->
    Simple = {config, [], xml_simple(Tree)},
    crlf([xmerl:export_simple([Simple], xmerl_xml_indent, [{prolog, ""}]), $\n]).

xml_simple([{Name, {value, Val}} | Ts]) ->
    [{xml_tag(Name), [], [fmt_value(Val)]} | xml_simple(Ts)];
xml_simple([{Name, {leaf_list, Vals}} | Ts]) ->
    Tag = xml_tag(Name),
    [{Tag, [], [fmt_value(V)]} || V <- Vals] ++ xml_simple(Ts);
xml_simple([{Name, Children} | Ts]) ->
    [{xml_tag(Name), [], xml_simple(Children)} | xml_simple(Ts)];
xml_simple([]) ->
    [].

xml_tag(Name) ->
    list_to_atom(xml_name(Name)).

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

format_json_tree(Tree) ->
    crlf(json:format(json_object(Tree), fun format_json_value/3, #{})).

%% `{object, Pairs}` keeps tree order and distinguishes `{}` from `[]`.
format_json_value({object, Pairs}, Encode, State) ->
    json:format_key_value_list(Pairs, Encode, State);
format_json_value(Value, Encode, State) ->
    json:format_value(Value, Encode, State).

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
