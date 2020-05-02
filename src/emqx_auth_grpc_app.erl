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

-module(emqx_auth_grpc_app).

-behaviour(application).
-behaviour(supervisor).

-emqx_plugin(auth).

-include_lib("emqx/include/logger.hrl").
-include("emqx_auth_grpc.hrl").

-logger_header("[AUTH_GRPC_APP]").

-export([start/2, stop/1]).
-export([init/1]).

%%--------------------------------------------------------------------
%% Application Callbacks
%%--------------------------------------------------------------------
start(_StartType, _StartArgs) ->
    {ok, Sup} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    load_hooks(),
    {ok, Sup}.

load_hooks() ->
    emqx_auth_grpc_cli:init(),
    ReqOpts = application:get_env(?APP, req_opts, []),
    Headers = application:get_env(?APP, headers, []),
    Params = #{req_opts => maps:from_list(ReqOpts), headers => Headers},

    ok = emqx_auth_grpc:register_metrics(),
    emqx:hook('client.authenticate', fun emqx_auth_grpc:check/3, [Params]),
    ?LOG(debug, "Add client authenticate hook: ~p", [Params]),
    case application:get_env(?APP, acl_host, undefined) of
        undefined -> ok;
        _ ->
            ok = emqx_acl_grpc:register_metrics(),
            ?LOG(debug, "Add client acl hook: ~p", [Params]),
            emqx:hook('client.check_acl', fun emqx_acl_grpc:check/5, [Params])
    end.


stop(_State) ->
    emqx:unhook('client.authenticate', fun emqx_auth_grpc:check/3),
    emqx:unhook('client.check_acl', fun emqx_acl_grpc:check/5).

%%--------------------------------------------------------------------
%% Dummy supervisor
%%--------------------------------------------------------------------

init([]) ->
    {ok, {{one_for_all, 10, 100}, []}}.