%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2022, Sean Hinde
%%% @doc CLI Command string tokeniser useful for completion and execution
%%%
%%% Created : 30 Apr 2022 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(ecli_tokenise).

-export([string/1]).

string(Str) ->
    Stripped = ecli_util:strip_ws(Str),
    tokenise(Stripped, [], []).

%% Empty input stays `{token, ""}` so lookup can treat it as a no-op.
tokenise([], [], []) ->
    {ok, [{token, ""}]};
tokenise([], [], Acc) ->
    {ok, lists:reverse(Acc)};
tokenise([], Current, Acc) ->
    {ok, lists:reverse([{token, lists:reverse(Current)} | Acc])};
tokenise([$\s], [], Acc) ->
    {ok, lists:reverse([space | Acc])};
tokenise([$\s], Current, Acc) ->
    {ok, lists:reverse([space, {token, lists:reverse(Current)} | Acc])};
tokenise([$\s | Cs], [], Acc) ->
    tokenise(Cs, "", [space | Acc]);
tokenise([$\s | Cs], Current, Acc) ->
    tokenise(Cs, "", [space, {token, lists:reverse(Current)} | Acc]);
tokenise([$| | Cs], [], Acc) ->
    tokenise(Cs, "", [pipe | Acc]);
tokenise([$| | Cs], Current, Acc) ->
    tokenise([$| | Cs], "", [{token, lists:reverse(Current)} | Acc]);
tokenise([$\" | Cs], "", Acc) ->
    tokenise_string(Cs, "", Acc);
tokenise([$\" | _Cs], _, _Acc) ->
    %% Hit a quote in the middle of the name of a tree entry. Nope
    no;
tokenise([C | Cs], Current, Acc) ->
    tokenise(Cs, [C|Current], Acc).

tokenise_string([$\" | Cs], Str, Acc) ->
    tokenise(Cs, "", [{string, lists:reverse(Str)} | Acc]);
tokenise_string([C|Cs], Str, Acc) ->
    tokenise_string(Cs, [C|Str], Acc);
tokenise_string([], Str, Acc) ->
    {ok, lists:reverse([{part_string, lists:reverse(Str)} | Acc])}.
