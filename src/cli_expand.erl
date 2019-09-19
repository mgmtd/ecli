%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2019, Sean Hinde
%%% @doc Implementation of a command expander
%%%
%%% @end
%%% Created : 17 Sep 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------
-module(cli_expand).

-export([expand/3]).

expand(Str, Tree, Getters) ->
    expand(Str, Tree, [], Getters, start).

expand([$\s|Cs], MenuItems, MatchedChars, Gs, start) ->
    expand(Cs, MenuItems, MatchedChars, Gs, start);
expand([$\t|Cs], MenuItems, MatchedChars, Gs, start) ->
    expand(Cs, MenuItems, MatchedChars, Gs, start);
expand([], MenuItems, _MatchedChars, Gs, start) ->
    io:format("expand start end ~p~n",[MenuItems]),
    case MenuItems of
        [] ->
            no;
        [_Item] ->
            {yes, "", MenuItems, Gs};
        _ ->
            {yes, "", MenuItems, Gs}
    end;
expand(Str, MenuItems, MatchedChars, Gs, start) ->
    io:format("expand change state ~p~n",[cmd]),
    expand(Str, MenuItems, MatchedChars, Gs, cmd);
expand([$\s|Cs], [Item], MatchedChars, Gs, cmd) ->
    Node = cli_util:name(Gs, Item),
    case MatchedChars of
        Node ->
            %% Matched a full level in the tree with a following space. Carry on to children
            io:format("expand matched full level ~p~p~n",[Node,Item]),
            {Gs1, Children} = menu_item_children(Item, Gs),
            expand(Cs, Children, [], Gs1, start);
        _ ->
            no
    end;
expand([C|Cs], MenuItems, Matched, Gs, cmd) ->
    io:format("expand normal chars ~p matched = ~p~n",[C, Matched]),
    SoFar = Matched ++ [C],
    Matches = match_cmds(SoFar, MenuItems, Gs),
    io:format("expand normal chars ~p matches = ~p~n",[SoFar, Matches]),
    case Matches of
        [] ->
            no;
        Items ->
            expand(Cs, Items, SoFar, Gs, cmd)
    end;
expand([], [], _, _, _) ->
    no;
expand([], MenuItems, MatchedStr, Gs, cmd) ->
    %% Reached the end of the input without getting to the end of a
    %% cmd with following space
    io:format("expand end ~p matched = ~p~n",[MenuItems, MatchedStr]),
    case MenuItems of
        [Item] ->
            Name = cli_util:name(Gs, Item),
            if Name == MatchedStr ->
                    {Gs1, Children} = menu_item_children(Item, Gs),
                    {yes, " ", Children, Gs1};
               true ->
                    {yes, chars_to_expand(MatchedStr, Name), [], Gs}
            end;
        MenuItems ->
            %% Still a number of matching menu items. auto fill up to the point they diverge
            Chars = expand_menus(MatchedStr, MenuItems, Gs),
            {yes, Chars, MenuItems, Gs}
    end.


match_cmds(_Str, [], _) ->
    [];
match_cmds(Str, Menu, Gs) ->
    lists:filter(fun(Item) ->
                         Name = cli_util:name(Gs, Item),
                         lists:prefix(Str, Name)
                 end, Menu).

chars_to_expand(Str, Match) ->
    lists:sublist(Match, length(Str) + 1, length(Match) - 1) ++ " ".

menu_item_children(Item, Gs) ->
    Cs = cli_util:children(Gs, Item),
    io:format("Children = ~p~n",[Cs]),
    case cli_util:eval_childspec(Cs) of
        {switch_getters, NewGs, Children} ->
            {NewGs, Children};
        Children ->
            {Gs, Children}
    end.

%% Find characters to add to fill up to where the node names diverge
%% e.g. names configure and contain given an input of "c" should return "on"
expand_menus(Str, Menus, Gs) ->
    StrLen = length(Str),
    Suffixes = lists:map(fun(Item) ->
                                 Name = cli_util:name(Gs, Item),
                                 lists:nthtail(StrLen, Name)
                         end, Menus),
    %% io:format("expand_menus ML = ~p~n",[Suffixes]),
    longest_common_prefix(Suffixes).

longest_common_prefix(Strings) ->
    longest_common_prefix(Strings, []).

longest_common_prefix(Strings, Result) ->
    {Prefixes, Tails} = lists:unzip(lists:map(
                                      fun([C|Cs]) -> {C, Cs};
                                         ([]) -> {empty, []}
                                      end, Strings)),
    case lists:member(empty, Prefixes) of
        true ->
            lists:reverse(Result);
        false ->
            case identical(Prefixes) of
                true ->
                    longest_common_prefix(Tails, [hd(Prefixes) | Result]);
                false ->
                    lists:reverse(Result)
            end
    end.

identical([A,A|As]) ->
    identical([A|As]);
identical([_,_|_]) ->
    false;
identical([_]) ->
    true;
identical([]) ->
    true.




