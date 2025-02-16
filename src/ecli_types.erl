%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2022, Sean Hinde
%%% @doc Standard leaf data types, parsing and printing
%%%  Yang types from https://www.rfc-editor.org/rfc/rfc6021.html
%%%
%%% @end
%%% Created : 18 Aug 2019 by Sean Hinde <sean@Seans-MacBook.local>
%%%-------------------------------------------------------------------

-module(ecli_types).

-export([parse/2]).

%% Built in types from Yang
%% counter32
%% zero-based-counter32
%% counter64
%% zero-based-counter64
%% gauge32
%% gauge64
%% object-identifier
%% object-identifier-128
%% date-and-time
%% timeticks
%% timestamp
%% phys-address
%% mac-address
%% xpath1.0

-type yang_type() :: counter32
                   | 'zero-based-counter32'
                   | counter64
                   | 'zero-based-counter64'
                   | gauge32
                   | gauge64
                   | 'object-identifier'
                   | 'object-identifier-128'
                   | 'date-and-time'
                   | timeticks
                   | timestamp
                   | 'phys-address'
                   | 'mac-address'
                   | 'xpath1.0'.

-spec parse(yang_type(), string()) -> {ok, any()} | {error, string()}.
parse(counter32, Value) ->
    case catch list_to_integer(Value) of
        {'EXIT', _} ->
            {error, "Invalid counter32"};
        Int when Int >= 0, Int =< 4294967295 ->
            Int;
        _ ->
            {error, "Counter32 out of range"}
    end.



%% Built in types from Yang ietf:
%% ip-version
%% dscp
%% ipv6-flow-label
%% port-number
%% as-number
%% ip-address
%% ipv4-address
%% ipv6-address
%% ip-prefix
%% ipv4-prefix
%% ipv6-prefix
%% domain-name
%% host
%% uri
