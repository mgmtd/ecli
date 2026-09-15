%%%-------------------------------------------------------------------
%%% @doc Unix-domain peer identity for a connected CLI socket.
%%%
%%% The kernel reports the connecting process's credentials. That is
%%% the true uid/gid of the `ecli` client, not a value the client sent.
%%%
%%% Linux: `SO_PEERCRED` (`struct ucred`: pid, uid, gid).
%%% Darwin/FreeBSD: `LOCAL_PEERCRED` (`struct xucred`: uid + groups).
%%% @end
%%%-------------------------------------------------------------------
-module(ecli_unix).

-export([peercred/1]).

-spec peercred(inet:socket()) -> map().
peercred(Socket) ->
    Cred =
        case os:type() of
            {unix, linux} ->
                linux_peercred(Socket);
            {unix, darwin} ->
                bsd_peercred(Socket);
            {unix, freebsd} ->
                bsd_peercred(Socket);
            {unix, netbsd} ->
                bsd_peercred(Socket);
            _ ->
                #{}
        end,
    with_user(Cred).

%% SOL_SOCKET = 1, SO_PEERCRED = 17 on Linux.
linux_peercred(Socket) ->
    case raw_opt(Socket, 1, 17, 12) of
        {ok, <<Pid:32/native-unsigned,
               Uid:32/native-unsigned,
               Gid:32/native-unsigned>>} ->
            #{pid => Pid, uid => Uid, gid => Gid};
        _ ->
            #{}
    end.

%% SOL_LOCAL = 0, LOCAL_PEERCRED = 1. xucred is version, uid, ngroups,
%% then NGROUPS gids. Request extra bytes; trailing data is ignored.
bsd_peercred(Socket) ->
    case raw_opt(Socket, 0, 1, 128) of
        {ok, <<_Version:32/native-unsigned,
               Uid:32/native-unsigned,
               Ngroups:16/native-signed,
               _Pad:16,
               Rest/binary>>} when Ngroups >= 1, byte_size(Rest) >= 4 ->
            <<Gid:32/native-unsigned, _/binary>> = Rest,
            #{uid => Uid, gid => Gid};
        {ok, <<_Version:32/native-unsigned,
               Uid:32/native-unsigned, _/binary>>} ->
            #{uid => Uid};
        _ ->
            #{}
    end.

raw_opt(Socket, Proto, Opt, Size) ->
    case inet:getopts(Socket, [{raw, Proto, Opt, Size}]) of
        {ok, [{raw, Proto, Opt, Bin}]} when is_binary(Bin), byte_size(Bin) > 0 ->
            {ok, Bin};
        _ ->
            error
    end.

with_user(#{uid := Uid} = Cred) ->
    case lookup_user(Uid) of
        undefined ->
            Cred;
        Name ->
            Cred#{user => Name}
    end;
with_user(Cred) ->
    Cred.

lookup_user(Uid) when is_integer(Uid), Uid >= 0 ->
    case string:trim(os:cmd("id -un " ++ integer_to_list(Uid))) of
        "" ->
            undefined;
        Name ->
            Name
    end.
