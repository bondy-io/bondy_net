%% =============================================================================
%%  bondy_net_app.erl -
%%
%%  Copyright (c) 2023-2025 Leapsight. All rights reserved.
%%
%%  Licensed under the Apache License, Version 2.0 (the "License");
%%  you may not use this file except in compliance with the License.
%%  You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%%  Unless required by applicable law or agreed to in writing, software
%%  distributed under the License is distributed on an "AS IS" BASIS,
%%  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%  See the License for the specific language governing permissions and
%%  limitations under the License.
%% =============================================================================

-module(bondy_net_app).

-behaviour(application).

-export([start/2, stop/1]).


%% =============================================================================
%% API
%% =============================================================================



start(_StartType, _StartArgs) ->
    ok = bondy_net_gen:do(),
    bondy_net_sup:start_link().

stop(_State) ->
    ok.

