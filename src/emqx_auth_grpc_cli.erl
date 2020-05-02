-module(emqx_auth_grpc_cli).

-include("emqx_auth_grpc.hrl").

-include_lib("grpcbox/include/grpcbox.hrl").
-include_lib("emqx/include/logger.hrl").

-logger_header("[GRPC]").

-export([init/0, auth_user/3, auth_acl/3]).

with_transport([]) -> http;
with_transport(_) -> https.

get_channel(undefined, _, _, _) -> {error, "no host"};
get_channel(Host, PortKey, SslOptsKey, ChannelName) ->
    SslOpts = application:get_env(?APP, SslOptsKey, []),
    Port = application:get_env(?APP, PortKey, 9090),
    {ok, {ChannelName, [{with_transport(SslOpts), Host, Port, SslOpts}], #{}}}.

get_channels() ->
    Channels = [],
    Host = application:get_env(?APP, host, undefined),
    C2 = case get_channel(Host, port, ssl_opts, default_channel) of
             {error, _} -> Channels;
             {ok, Ch1} -> [Ch1 | Channels]
         end,
    SuperHost = application:get_env(?APP, superuser_host, undefined),
    C3 = case get_channel(SuperHost, superuser_port, superuser_ssl_opts, super_channel) of
             {error, _} -> C2;
             {ok, Ch2} -> [Ch2 | C2]
         end,
    AclHost = application:get_env(?APP, acl_host, undefined),
    C4 = case get_channel(AclHost, acl_port, acl_ssl_opts, acl_channel) of
             {error, _} -> C3;
             {ok, Ch3} -> [Ch3 | C3]
         end,
    lists:reverse(C4).

init() ->
    application:stop(grpcbox),
    application:load(grpcbox),
    Channels = get_channels(),
    [fun (X) ->
        ?LOG(debug, "Add channel: ~p", [X])
     end (C)|| C <- Channels],
    Clients = #{channels => Channels},
    application:set_env(grpcbox, client, Clients),
    {ok, _Started } = application:ensure_all_started(grpcbox).


get_ctx(undefined) -> ctx:new();
get_ctx([]) -> ctx:new();
get_ctx(H) ->
    H2 = lists:map(fun ({K, V}) -> {list_to_binary(K), list_to_binary(V)} end, H),
    Meta = maps:from_list(H2),
    grpcbox_metadata:append_to_outgoing_ctx(ctx:new(), Meta).

get_retry_opts(ReqOpts) ->
    Times = maps:get(retry_times, ReqOpts, 1),
    Interval = maps:get(retry_interval, ReqOpts, 200),
    BackOff = maps:get(retry_backoff, ReqOpts, 1),
    {Times, Interval, BackOff}.

auth_user(ClientInfo, Headers, ReqOpts) ->
    Ctx = get_ctx(Headers),
    AuthReq = build_auth_req(ClientInfo),
    {Times, Interval, BackOff} = get_retry_opts(ReqOpts),

    ?LOG(debug, "Sending auth user grpc: ~p", [AuthReq]),
    case emqx_auth_authentication_client:auth_user(Ctx, AuthReq) of
        {error, {Code, Msg}} when Times > 0 ->
            ?LOG(error, "Call Auth user grpc error: Code=~p, Msg=~p", [Code, Msg]),
            timer:sleep(trunc(Interval)),
            ROpts1 = ReqOpts#{retry_times := Times - 1, retry_interval := Interval * BackOff},
            auth_user(ClientInfo, Headers, ROpts1);
        {error, Error} when Times > 0 ->
            ?LOG(error, "Call Auth user grpc error: ~p", [Error]),
            timer:sleep(trunc(Interval)),
            ROpts1 = ReqOpts#{retry_times := Times - 1, retry_interval := Interval * BackOff},
            auth_user(ClientInfo, Headers, ROpts1);
        Other -> Other
    end.

auth_acl(ClientInfo, Headers, ReqOpts) ->
    Ctx = get_ctx(Headers),
    AuthReq = build_auth_req(ClientInfo),
    {Times, Interval, BackOff} = get_retry_opts(ReqOpts),
    ?LOG(info, "Sending check acl grpc request: ~p", [AuthReq]),
    case emqx_auth_authentication_client:auth_acl(Ctx, AuthReq) of
        {error, {Code, Msg}} when Times > 0 ->
            ?LOG(error, "Call Auth ACL error: Code=~p, Msg=~p", [Code, Msg]),
            timer:sleep(trunc(Interval)),
            ROpts1 = ReqOpts#{retry_times := Times - 1, retry_interval := Interval * BackOff},
            auth_acl(ClientInfo, Headers, ROpts1);
        {error, Error} when Times > 0 ->
            ?LOG(error, "Call Auth ACL error: ~p", [Error]),
            timer:sleep(trunc(Interval)),
            ROpts1 = ReqOpts#{retry_times := Times - 1, retry_interval := Interval * BackOff},
            auth_acl(ClientInfo, Headers, ROpts1);
        Other ->
            Other
    end.


to_iodata(undefined) -> <<"">>;
to_iodata(V) when is_atom(V) -> atom_to_binary(V, utf8);
to_iodata(V) when is_list(V) -> list_to_binary(V);
to_iodata(_) -> <<"">>.
iodata_value(M, K) -> to_iodata(maps:get(K, M, undefined)).

to_int(V) when is_integer(V) -> V;
to_int(V) when is_float(V) -> trunc(V);
to_int(_) -> 0.
int_value(M, K) -> to_int(maps:get(K, M, undefined)).

to_ipaddr(undefined) -> <<"">>;
to_ipaddr(V) when is_list(V) -> list_to_binary(V);
to_ipaddr(_) -> <<"">>.
ipaddr_value(M, K) -> to_ipaddr(inet:ntoa(maps:get(K, M, undefined))).

to_bool(V) when is_boolean(V) -> V;
to_bool(_) -> false.
bool_value(M, K) -> to_bool(maps:get(K, M, undefined)).

build_auth_req(ClientInfo = #{clientid := ClientID, username := Username}) ->
    Req = #{
        client_id => ClientID,
        username => Username,
        password => iodata_value(ClientInfo, password),
        protocol => iodata_value(ClientInfo, protocol),
        peerhost => ipaddr_value(ClientInfo, peerhost),
        sockport => int_value(ClientInfo, sockport),
        peercert => iodata_value(ClientInfo, peercert),
        is_bridge => bool_value(ClientInfo, is_bridge),
        is_superuser => bool_value(ClientInfo, is_superuser),
        mountpoint => iodata_value(ClientInfo, mountpoint),
        zone => iodata_value(ClientInfo, zone),
        tls_common_name => iodata_value(ClientInfo, cn),
        tls_subject => iodata_value(ClientInfo, dn),
%%      acl specified
        access => iodata_value(ClientInfo, access),
        topic => iodata_value(ClientInfo, topic)
    },
    Req.