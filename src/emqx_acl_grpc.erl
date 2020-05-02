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

-module(emqx_acl_grpc).

-include("emqx_auth_grpc.hrl").

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/logger.hrl").

-logger_header("[ACL-GRPC]").

%% ACL callbacks
-export([register_metrics/0
    , check/5
    , reload_acl/1
    , description/0
]).

-spec(register_metrics() -> ok).
register_metrics() ->
    lists:foreach(fun emqx_metrics:new/1, ?ACL_METRICS).

%%--------------------------------------------------------------------
%% ACL callbacks
%%--------------------------------------------------------------------

check(ClientInfo, PubSub, Topic, AclResult, State) ->
%%    ?LOG(info, "Checking ACL, Client=~p, PubSub=~p, Topic=~p, AuthResult=~p, State=~p", [ClientInfo, PubSub, Topic, AclResult, State]),
    return_with(fun inc_metrics/1,
        do_check_acl(ClientInfo, PubSub, Topic, AclResult, State)).

do_check_acl(#{username := <<$$, _/binary>>}, _PubSub, _Topic, _AclResult, _Config) ->
    ok;
do_check_acl(ClientInfo, PubSub, Topic, _AclResult, #{req_opts := ReqOpts, headers := Headers}) ->
    ClientInfo1 = ClientInfo#{access => access(PubSub), topic => Topic},
    case emqx_auth_grpc_cli:auth_acl(ClientInfo1, Headers, ReqOpts) of
        {ok, Rsp, _Meta} ->
            case maps:get(code, Rsp, 0) of
                0 -> {stop, allow};
                Code -> ?LOG(error, "Failed to check acl, Code=~p", [Code]), {stop, deny}
            end;
        Error -> ?LOG(error, "Failed to call grpc: ~p", [Error]), {stop, deny}
    end.

reload_acl(_State) -> ok.

description() -> "ACL with HTTP API".

%%--------------------------------------------------------------------
%% Internal functions
%%--------------------------------------------------------------------

inc_metrics(ok) ->
    emqx_metrics:inc(?ACL_METRICS(ignore));
inc_metrics({stop, allow}) ->
    emqx_metrics:inc(?ACL_METRICS(allow));
inc_metrics({stop, deny}) ->
    emqx_metrics:inc(?ACL_METRICS(deny)).

return_with(Fun, Result) ->
    Fun(Result), Result.

access(subscribe) -> 1;
access(publish) -> 2.

