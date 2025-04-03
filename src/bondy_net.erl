-module(bondy_net).

-include("bondy_net.hrl").

-moduledoc #{format => "text/markdown"}.
?MODULEDOC("""
This modules provides a unified proxy module that provides a common interface
for:
* Node connection management
* Node list queries
* Message passing (e.g., !, rpc, gen_server:call, etc.)
* Monitoring / linking
* Node metadata (e.g., node(), nodes())

This module exists is source code form just for documentation purposes, as it
is be replaced in runtime by `c:bondy_net_gen:do/0`.
""").

-export([broadcast/2]).
-export([is_alive/0]).
-export([is_connected/1]).
-export([is_connected/2]).
-export([monitor_node/2]).
-export([monitor_node/3]).
-export([monitor_nodes/1]).
-export([monitor_nodes/2]).
-export([node/0]).
-export([node/1]).
-export([nodes/0]).
-export([nodes/1]).
-export([send/2]).
-export([send/3]).
-export([send_after/3]).
-export([send_after/4]).



%% =============================================================================
%% CALLBACKS FOR CUSTOM BACKEND.
%% =============================================================================


-callback broadcast(Msg :: any(), Handler :: module()) -> ok.
-optional_callbacks([broadcast/2]).

-callback is_alive() -> boolean().

-callback is_connected(Node :: node()) -> boolean().

-callback is_connected(Node :: node(), Channel :: atom()) -> boolean().

-callback monitor_node(Node :: node(), Flag :: boolean()) -> boolean().

-callback monitor_node(Node :: node(), Flag :: boolean(), Opts :: list()) ->
    true.

-callback monitor_nodes(Flag :: boolean()) -> ok | error | {error, term()}.

-callback monitor_nodes(
    Flag :: boolean(), Opts :: [erlang:monitor_nodes_opt()]) ->
    ok | error | {error, term()}.

-callback node() -> node().

-callback node(Arg :: pid() | port() | reference()) -> node().

-callback nodes() -> [node()].

-callback nodes(Arg :: any()) -> [node()].

-callback send(Dest :: any(), Msg :: any()) -> Msg :: any().

-callback send(Dest :: any(), Msg :: any(), Opts :: any()) ->
    ok | nosuspend | noconnect.

-callback send_after(Time :: integer(), Dest :: any(), Msg :: any()) ->
    TRef :: reference().

-callback send_after(
    Time :: integer(), Dest :: any(), Msg :: any(), Opts :: any()) ->
    TRef :: reference().


%% =============================================================================
%% PLACEHOLDER API
%% =============================================================================


broadcast(_, _) -> error(not_implemented).

is_alive() -> error(not_implemented).

is_connected(_) -> error(not_implemented).

is_connected(_, _) -> error(not_implemented).

monitor_node(_, _) -> error(not_implemented).

monitor_node(_, _, _) -> error(not_implemented).

monitor_nodes(_) -> error(not_implemented).

monitor_nodes(_, _) -> error(not_implemented).

node() -> error(not_implemented).

node(_) -> error(not_implemented).

nodes() -> error(not_implemented).

nodes(_) -> error(not_implemented).

send(_, _) -> error(not_implemented).

send(_, _, _) -> error(not_implemented).

send_after(_, _, _) -> error(not_implemented).

send_after(_, _, _, _) -> error(not_implemented).



