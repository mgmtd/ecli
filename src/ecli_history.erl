%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2022, Sean Hinde
%%% @doc Implementation of cli history
%%% 
%%% Expected Behaviour
%%% ==================
%%% 
%%% Initially prev/1 should give us the previous line, prev/1 again the line 
%%% previous to that. Then next/1 should give us the first prev line and next/1
%%% back to whatever the user had on the line before exploring the history.
%%% 
%%% If any line in the history is edited it should still appear edited after 
%%% moving away and back, but only until a command is executed. At that point
%%% all the original history should be restored with the executed command as
%%% the most recent
%%% 
%%% So during a single history session we don't need to care about the original
%%% state. We can just track edited lines in the history. Immutability is a
%%% superpower :)
%%%
%%% @end
%%% Created : 27 Apr 2022 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(ecli_history).

-export([new/0, new/1, prev/2, next/2]).

%% Store history as {Before, After}
new() ->
    {[], []}.

new(History) ->
    {History, []}.

%% Return the previous entry and place the provided current line in the history.
%% Return false if we are at the end
prev(Line, {[Bef | Before], After}) ->
    {Bef, {Before, [Line | After]}};
prev(_Line, {[], _After}) ->
    false.

%% Return the next entry and place the provided current line in the history.
%% Return false if we are at the end
next(Line, {Before, [Aft | After]}) ->
    {Aft, {[Line | Before], After}};
next(_Line, {_Before, []}) ->
    false.
