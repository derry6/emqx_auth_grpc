%%--------------------------------------------------------------------
%% Copyright (c) 2020 EMQ Technologies Co., Ltd. All Rights Reserved.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%--------------------------------------------------------------------

-module(emqx_auth_grpc).

-include("emqx_auth_grpc.hrl").

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/types.hrl").
-include_lib("emqx/include/logger.hrl").
-include_lib("grpcbox/include/grpcbox.hrl").

-logger_header("[Auth-GRPC]").

%% Callbacks
-export([register_metrics/0
    , check/3
    , description/0
    , test_log/0
]).

description() -> "Authentication by GRPC API".

-spec(register_metrics() -> ok).
register_metrics() ->
    lists:foreach(fun emqx_metrics:new/1, ?AUTH_METRICS).



handle_rpc_error(AuthResult, _Error, _Client) ->
    emqx_metrics:inc(?AUTH_METRICS(failure)),
    {stop, AuthResult#{auth_result => server_unavailable, anonymous => false}}.


handle_rpc_ok(AuthResult, Rsp, Client) ->
    case maps:get(code, Rsp, 0) of
        0 ->
            case maps:get(ignore, Rsp, false) of
                true -> emqx_metrics:inc(?AUTH_METRICS(success)), ok;
                false ->
                    emqx_metrics:inc(?AUTH_METRICS(success)),
                    {stop,
                        AuthResult#{
                            is_superuser => maps:get(superuser, Rsp, false),
                            auth_result => success,
                            anonymous => false,
                            mountpoint => mountpoint(Rsp,Client)
                        }
                    }
            end;
        Code ->
            ?LOG(error,  "Deny connection grpc code: ~p", [Code]),
            emqx_metrics:inc(?AUTH_METRICS(failure)),
            {stop, AuthResult#{auth_result => auth_errcode(Code), anonymous => false}}
    end.


check(ClientInfo, AuthResult, _Opts = #{req_opts := ReqOpts, headers := Headers}) ->
%%    ?LOG(info, "Auth user: Client=~p, AuthResult=~p, Opts=~p",[ClientInfo, AuthResult, Opts]),
    case emqx_auth_grpc_cli:auth_user(ClientInfo, Headers, ReqOpts) of
        {ok, Rsp, _Meta} ->
            handle_rpc_ok(AuthResult, Rsp, ClientInfo);
        Error ->
            ?LOG(error, "Failed to auth user: ~p", [Error]),
            handle_rpc_error(AuthResult, Error, ClientInfo)
    end.

mountpoint(Rsp, #{mountpoint := Mountpoint}) ->
    case maps:get(mount_point, Rsp, undefined) of
        undefined -> Mountpoint;
        <<>> -> Mountpoint;
        Other -> Other
    end.

auth_errcode(?GRPC_STATUS_UNKNOWN) -> bad_username_or_password;
auth_errcode(?GRPC_STATUS_NOT_FOUND) -> bad_username_or_password;
auth_errcode(?GRPC_STATUS_PERMISSION_DENIED) -> not_authorized;
auth_errcode(?GRPC_STATUS_UNAUTHENTICATED) -> not_authorized;
auth_errcode(?GRPC_STATUS_DEADLINE_EXCEEDED) -> banned;
auth_errcode(?GRPC_STATUS_UNAVAILABLE) -> server_unavailable;
auth_errcode(?GRPC_STATUS_RESOURCE_EXHAUSTED) -> server_busy;
auth_errcode(?GRPC_STATUS_DATA_LOSS) -> not_exists;
auth_errcode(_) -> server_unavailable.

test_log() ->
    ?LOG(info, "testing logger").

%%-define(CHANNELS_TAB, channels_table).
%%
%%-define(GRPC_STATUS_OK, <<"0">>).
%%-define(GRPC_STATUS_CANCELLED, <<"1">>).
%%-define(GRPC_STATUS_UNKNOWN, <<"2">>).
%%-define(GRPC_STATUS_INVALID_ARGUMENT, <<"3">>).
%%-define(GRPC_STATUS_DEADLINE_EXCEEDED, <<"4">>).
%%-define(GRPC_STATUS_NOT_FOUND, <<"5">>).
%%-define(GRPC_STATUS_ALREADY_EXISTS , <<"6">>).
%%-define(GRPC_STATUS_PERMISSION_DENIED, <<"7">>).
%%-define(GRPC_STATUS_RESOURCE_EXHAUSTED, <<"8">>).
%%-define(GRPC_STATUS_FAILED_PRECONDITION, <<"9">>).
%%-define(GRPC_STATUS_ABORTED, <<"10">>).
%%-define(GRPC_STATUS_OUT_OF_RANGE, <<"11">>).
%%-define(GRPC_STATUS_UNIMPLEMENTED, <<"12">>).
%%-define(GRPC_STATUS_INTERNAL, <<"13">>).
%%-define(GRPC_STATUS_UNAVAILABLE, <<"14">>).
%%-define(GRPC_STATUS_DATA_LOSS, <<"15">>).
%%-define(GRPC_STATUS_UNAUTHENTICATED, <<"16">>).

%%-define(GRPC_ERROR(Status, Message), {grpc_error, {Status, Message}}).
%%-define(THROW(Status, Message), throw(?GRPC_ERROR(Status, Message))).
