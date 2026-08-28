%%%-------------------------------------------------------------------
%%% @author Sean Hinde <sean@Seans-MacBook.local>
%%% @copyright (C) 2022, Sean Hinde
%%% @doc Leaf data types: parsing and completions.
%%%
%%% Type forms follow YANG more closely than JSON Schema:
%%%
%%% <ul>
%%%   <li>Built-in names as atoms: `uint32', `string', `boolean', ...</li>
%%%   <li>Restricted integers as `{uint32, Range}'</li>
%%%   <li>Enumerations as `{enum, Members}' (YANG `enumeration')</li>
%%%   <li>Imported types as atoms: `'inet:ip-address''</li>
%%%   <li>User callbacks as `{Mod, Type}' where Mod is <em>not</em> a
%%%       YANG type constructor. Callback: `Mod:parse_value(Type, Token)'</li>
%%% </ul>
%%%
%%% `{enum, Members}' must never be treated as a `{Module, Type}'
%%% callback: `enum' is a type constructor, like `union' or `bits'.
%%%
%%% Enum members (canonical stored value is the name string):
%%%   Name | {Name, Description} | #{name := Name, desc => ..., value => integer()}
%%%
%%% YANG types from https://www.rfc-editor.org/rfc/rfc7950.html
%%% IETF types from https://www.rfc-editor.org/rfc/rfc6021.html
%%% @end
%%%-------------------------------------------------------------------
-module(ecli_types).

-export([parse/2, completions/1]).

-type enum_member() :: string()
                     | {string(), string()}
                     | #{name := string(),
                         desc => string(),
                         value => integer()}.

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

-export_type([enum_member/0, yang_type/0]).

%% @doc Parse a CLI token against a schema type.
%% Returns `{ok, Value}' or `{error, Reason}' suitable to show the user.
-spec parse(term(), string()) -> {ok, term()} | {error, string()}.
parse({enum, Members}, Token) ->
    parse_enum(Token, Members);
parse({enumeration, Members}, Token) ->
    parse_enum(Token, Members);
parse({union, _Types}, _Token) ->
    {error, "union types are not supported yet"};
parse({bits, _Bits}, _Token) ->
    {error, "bits types are not supported yet"};
parse({leafref, _Path}, _Token) ->
    {error, "leafref types are not supported yet"};
parse({identityref, _Base}, _Token) ->
    {error, "identityref types are not supported yet"};
parse({Type, Range}, Token) when is_atom(Type), is_list(Range) ->
    case integer_range(Type) of
        undefined ->
            parse_tagged(Type, Range, Token);
        BaseRange ->
            parse_integer(Token, merge_ranges(BaseRange, Range))
    end;
parse(Type, Token) when is_atom(Type) ->
    parse_builtin(Type, Token);
parse({Mod, Type}, Token) when is_atom(Mod) ->
    parse_tagged(Mod, Type, Token);
parse(_Type, Token) ->
    {ok, Token}.

%% @doc Finite-choice completions for a type (enums, boolean).
%% Empty list means "free-form value, show the leaf description".
-spec completions(term()) -> [#{name := string(), desc := string()}].
completions(boolean) ->
    [#{name => "true", desc => "True"},
     #{name => "false", desc => "False"}];
completions({enum, Members}) ->
    enum_completions(Members);
completions({enumeration, Members}) ->
    enum_completions(Members);
completions(_) ->
    [].

%%--------------------------------------------------------------------
%% Built-in atoms
%%--------------------------------------------------------------------
parse_builtin(string, Token) ->
    {ok, Token};
parse_builtin(boolean, "true") ->
    {ok, true};
parse_builtin(boolean, "false") ->
    {ok, false};
parse_builtin(boolean, _) ->
    {error, "Expected true or false"};
parse_builtin('inet:port-number', Token) ->
    parse_integer(Token, uint16_range());
parse_builtin('inet:ip-address', Token) ->
    parse_ip_address(Token);
parse_builtin(integer, Token) ->
    parse_integer(Token, []);
parse_builtin(Type, Token) ->
    case integer_range(Type) of
        undefined ->
            {ok, Token};
        Range ->
            parse_integer(Token, Range)
    end.

%% `{Mod, Type}' user callback, unless Mod is a reserved YANG constructor.
parse_tagged(Mod, Type, Token) ->
    case is_yang_constructor(Mod) of
        true ->
            {error, "Unsupported type"};
        false ->
            case Mod:parse_value(Type, Token) of
                {ok, _Value} = Ok ->
                    Ok;
                {error, _Err} = Err ->
                    Err
            end
    end.

%% YANG type constructors that occupy the `{Tag, Payload}' slot.
%% These must never be dispatched as `Tag:parse_value(Payload, Token)'.
is_yang_constructor(enum) -> true;
is_yang_constructor(enumeration) -> true;
is_yang_constructor(union) -> true;
is_yang_constructor(bits) -> true;
is_yang_constructor(leafref) -> true;
is_yang_constructor(identityref) -> true;
is_yang_constructor(uint8) -> true;
is_yang_constructor(uint16) -> true;
is_yang_constructor(uint32) -> true;
is_yang_constructor(uint64) -> true;
is_yang_constructor(int8) -> true;
is_yang_constructor(int16) -> true;
is_yang_constructor(int32) -> true;
is_yang_constructor(int64) -> true;
is_yang_constructor(integer) -> true;
is_yang_constructor(decimal64) -> true;
is_yang_constructor(_) -> false.

%%--------------------------------------------------------------------
%% Enumeration
%%--------------------------------------------------------------------
parse_enum(Token, [Member | Rest]) ->
    case enum_member_name(Member) of
        Token ->
            {ok, Token};
        _ ->
            parse_enum(Token, Rest)
    end;
parse_enum(_Token, []) ->
    {error, "Unknown enum value"}.

enum_completions(Members) ->
    [#{name => enum_member_name(M), desc => enum_member_desc(M)} || M <- Members].

enum_member_name(#{name := Name}) ->
    Name;
enum_member_name({Name, _Desc}) ->
    Name;
enum_member_name(Name) when is_list(Name) ->
    Name.

enum_member_desc(#{desc := Desc}) ->
    Desc;
enum_member_desc({_Name, Desc}) when is_list(Desc) ->
    Desc;
enum_member_desc(_) ->
    "".

%%--------------------------------------------------------------------
%% Integers and ranges
%%--------------------------------------------------------------------
integer_range(uint64) -> uint64_range();
integer_range(uint32) -> uint32_range();
integer_range(uint16) -> uint16_range();
integer_range(uint8) -> uint8_range();
integer_range(int64) -> int64_range();
integer_range(int32) -> int32_range();
integer_range(int16) -> int16_range();
integer_range(int8) -> int8_range();
integer_range(integer) -> [];
integer_range(_) -> undefined.

merge_ranges([{min, Min} | Rs], UserRange) ->
    case lists:keyfind(min, 1, UserRange) of
        {min, UMin} when UMin > Min ->
            [{min, UMin} | merge_ranges(Rs, UserRange)];
        _ ->
            [{min, Min} | merge_ranges(Rs, UserRange)]
    end;
merge_ranges([{max, Max} | Rs], UserRange) ->
    case lists:keyfind(max, 1, UserRange) of
        {max, UMax} when UMax < Max ->
            [{max, UMax} | merge_ranges(Rs, UserRange)];
        _ ->
            [{max, Max} | merge_ranges(Rs, UserRange)]
    end;
merge_ranges([], _) ->
    [].

uint64_range() -> [{min, 0}, {max, 18446744073709551615}].
uint32_range() -> [{min, 0}, {max, 4294967295}].
uint16_range() -> [{min, 0}, {max, 65535}].
uint8_range() -> [{min, 0}, {max, 255}].

int64_range() -> [{min, -9223372036854775808}, {max, 9223372036854775807}].
int32_range() -> [{min, -2147483648}, {max, 2147483647}].
int16_range() -> [{min, -32768}, {max, 32767}].
int8_range() -> [{min, -128}, {max, 127}].

parse_integer(Token, Range) ->
    try list_to_integer(Token) of
        Int ->
            parse_integer_in_range(Int, Range)
    catch
        error:badarg ->
            {error, "Expected an integer value"}
    end.

parse_integer_in_range(Int, [{min, Min} | Rs]) when Int >= Min ->
    parse_integer_in_range(Int, Rs);
parse_integer_in_range(Int, [{max, Max} | Rs]) when Int =< Max ->
    parse_integer_in_range(Int, Rs);
parse_integer_in_range(Int, []) ->
    {ok, Int};
parse_integer_in_range(_Int, _) ->
    {error, "Value out of range"}.

parse_ip_address(Token) ->
    case inet:parse_address(Token) of
        {ok, _Addr} = Ok ->
            Ok;
        {error, _} ->
            {error, "Invalid IP Address"}
    end.
