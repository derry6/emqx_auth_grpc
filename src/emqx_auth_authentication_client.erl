%%%-------------------------------------------------------------------
%% @doc Client module for grpc service emqx_auth.Authentication.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated on 2020-05-02T15:53:19+00:00 and should not be modified manually

-module(emqx_auth_authentication_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpcbox/include/grpcbox.hrl").

-define(is_ctx(Ctx), is_tuple(Ctx) andalso element(1, Ctx) =:= ctx).

-define(SERVICE, 'emqx_auth.Authentication').
-define(PROTO_MODULE, 'auth_pb').
-define(MARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL_FUN(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Input, Output, MessageType), #grpcbox_def{service=?SERVICE,
                                                      message_type=MessageType,
                                                      marshal_fun=?MARSHAL_FUN(Input),
                                                      unmarshal_fun=?UNMARSHAL_FUN(Output)}).

%% @doc Unary RPC
-spec auth_user(auth_pb:auth_request()) ->
    {ok, auth_pb:auth_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
auth_user(Input) ->
    auth_user(ctx:new(), Input, #{}).

-spec auth_user(ctx:t() | auth_pb:auth_request(), auth_pb:auth_request() | grpcbox_client:options()) ->
    {ok, auth_pb:auth_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
auth_user(Ctx, Input) when ?is_ctx(Ctx) ->
    auth_user(Ctx, Input, #{});
auth_user(Input, Options) ->
    auth_user(ctx:new(), Input, Options).

-spec auth_user(ctx:t(), auth_pb:auth_request(), grpcbox_client:options()) ->
    {ok, auth_pb:auth_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
auth_user(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/emqx_auth.Authentication/AuthUser">>, Input, ?DEF(auth_request, auth_response, <<"emqx_auth.AuthRequest">>), Options).

%% @doc Unary RPC
-spec auth_acl(auth_pb:auth_request()) ->
    {ok, auth_pb:auth_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
auth_acl(Input) ->
    auth_acl(ctx:new(), Input, #{}).

-spec auth_acl(ctx:t() | auth_pb:auth_request(), auth_pb:auth_request() | grpcbox_client:options()) ->
    {ok, auth_pb:auth_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
auth_acl(Ctx, Input) when ?is_ctx(Ctx) ->
    auth_acl(Ctx, Input, #{});
auth_acl(Input, Options) ->
    auth_acl(ctx:new(), Input, Options).

-spec auth_acl(ctx:t(), auth_pb:auth_request(), grpcbox_client:options()) ->
    {ok, auth_pb:auth_response(), grpcbox:metadata()} | grpcbox_stream:grpc_error_response().
auth_acl(Ctx, Input, Options) ->
    grpcbox_client:unary(Ctx, <<"/emqx_auth.Authentication/AuthACL">>, Input, ?DEF(auth_request, auth_response, <<"emqx_auth.AuthRequest">>), Options).

