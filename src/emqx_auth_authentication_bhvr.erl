%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service emqx_auth.Authentication.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2020-05-02T15:53:19+00:00 and should not be modified manually

-module(emqx_auth_authentication_bhvr).

%% @doc Unary RPC
-callback auth_user(ctx:ctx(), auth_pb:auth_request()) ->
    {ok, auth_pb:auth_response(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

%% @doc Unary RPC
-callback auth_acl(ctx:ctx(), auth_pb:auth_request()) ->
    {ok, auth_pb:auth_response(), ctx:ctx()} | grpcbox_stream:grpc_error_response().

