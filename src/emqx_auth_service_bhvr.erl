%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service emqx_auth.Service.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2020-04-30T08:45:04+00:00 and should not be modified manually

-module(emqx_auth_service_bhvr).

%% @doc Unary RPC
-callback auth_user(ctx:ctx(), auth_pb:auth_request()) ->
    {ok, auth_pb:auth_response(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc Unary RPC
-callback is_super(ctx:ctx(), auth_pb:auth_request()) ->
    {ok, auth_pb:auth_response(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc Unary RPC
-callback auth_acl(ctx:ctx(), auth_pb:acl_request()) ->
    {ok, auth_pb:acl_response(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

