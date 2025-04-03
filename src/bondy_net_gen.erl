-module(bondy_net_gen).

-include("bondy_net.hrl").

-moduledoc #{format => "text/markdown"}.
?MODULEDOC("""
This module uses parse transforms to replace the `bondy_net` module with a concrete `backend` module,
""").

-export([do/0]).

-compile({parse_transform, parse_trans_codegen}).


do() ->
    BackendMod = backend(),
    BcastBackendMod = broadcast_backend(BackendMod),
    Forms = forms(BackendMod, BcastBackendMod),
    {ok, bondy_net, Bin} = compile:forms(Forms),
    code:load_binary(bondy_net, "nofile", Bin),
    ok.


%% =============================================================================
%% PRIVATE
%% =============================================================================

forms(erlang, BroadcastBackend) ->
    codegen:gen_module(
        bondy_net,
        [
            {broadcast, 2},
            {is_alive, 0},
            {is_connected, 1},
            {is_connected, 2},
            {monitor_node, 2},
            {monitor_node, 3},
            {monitor_nodes, 1},
            {monitor_nodes, 2},
            {node, 0},
            {node, 1},
            {nodes, 0},
            {nodes, 1},
            {send, 2},
            {send, 3},
            {send_after, 3},
            {send_after, 4}
        ],
        [
            {broadcast, fun(Msg, Handler) ->
                case {'$var', BroadcastBackend} of
                    undefined ->
                        error(no_broadcast_backend);
                    Mod ->
                        Mod:broadcast(Msg, Handler)
                end
            end},
            {is_alive, fun() ->
                erlang:is_alive()
            end},
            {is_connected, fun(Node) ->
                lists:member(Node, erlang:nodes())
            end},
            {is_connected, fun(Node, _Channel) ->
                lists:member(Node, erlang:nodes()) end
            },
            {monitor_node, fun(Node, Flag) ->
                erlang:monitor_node(Node, Flag)
            end},
            {monitor_node, fun(Node, Flag, Options) ->
                erlang:monitor_node(Node, Flag, Options)
            end},
            {monitor_nodes, fun(Flag) ->
                net_kernel:monitor_nodes(Flag)
            end},
            {monitor_nodes, fun(Flag, Options) ->
                net_kernel:monitor_nodes(Flag, Options)
            end},
            {node, fun() ->
                erlang:node()
            end},
            {node, fun(Arg) ->
                erlang:node(Arg)
            end},
            {nodes, fun() ->
                erlang:nodes()
            end},
            {nodes, fun(Arg) ->
                erlang:nodes(Arg)
            end},
            {send, fun(Dest, Msg) ->
                erlang:send(Dest, Msg)
            end},
            {send, fun(Dest, Msg, Opts) ->
                erlang:send(Dest, Msg, Opts)
            end},
            {send_after, fun(Time, Dest, Msg) ->
                erlang:send(Time, Dest, Msg)
            end},
            {send_after, fun(Time, Dest, Msg, Opts) ->
                erlang:send(Time, Dest, Msg, Opts)
            end}
        ]
    );

forms(Backend, BroadcastBackend) ->
    codegen:gen_module(
        bondy_net,
        [
            {broadcast, 2},
            {is_alive, 0},
            {is_connected, 1},
            {is_connected, 2},
            {monitor_node, 2},
            {monitor_node, 3},
            {monitor_nodes, 1},
            {monitor_nodes, 2},
            {node, 0},
            {node, 1},
            {nodes, 0},
            {nodes, 1},
            {send, 2},
            {send, 3},
            {send_after, 3},
            {send_after, 4}
        ],
        [
            {broadcast, fun(Msg, Handler) ->
                case {'$var', BroadcastBackend} of
                    undefined ->
                        error(no_broadcast_backend);

                    Mod ->
                        Mod:broadcast(Msg, Handler)
                end
            end},
            {is_alive, fun() ->
                {'$var', Backend}:is_alive()
            end},
            {is_connected, fun(Node) ->
                {'$var', Backend}:is_connected(Node)
            end},
            {is_connected, fun(Node, Channels) ->
                {'$var', Backend}:is_connected(Node, Channels)
            end},
            {monitor_node, fun(Node, Flag) ->
                {'$var', Backend}:monitor_node(Node, Flag)
            end},
            {monitor_node, fun(Node, Flag, Options) ->
                {'$var', Backend}:monitor_node(Node, Flag, Options)
            end},
            {monitor_nodes, fun(Flag) ->
                {'$var', Backend}:monitor_nodes(Flag)
            end},
            {monitor_nodes, fun(Flag, Options) ->
                {'$var', Backend}:monitor_nodes(Flag, Options)
            end},
            {node, fun() ->
                {'$var', Backend}:node()
            end},
            {node, fun(Arg) ->
                {'$var', Backend}:node(Arg)
            end},
            {nodes, fun() ->
                {'$var', Backend}:nodes()
            end},
            {nodes, fun(Arg) ->
                {'$var', Backend}:nodes(Arg)
            end},
            {send, fun(Dest, Msg) ->
                {'$var', Backend}:send(Dest, Msg)
            end},
            {send, fun(Dest, Msg, Opts) ->
                {'$var', Backend}:send(Dest, Msg, Opts)
            end},
            {send_after, fun(Time, Dest, Msg) ->
                {'$var', Backend}:send(Time, Dest, Msg)
            end},
            {send_after, fun(Time, Dest, Msg, Opts) ->
                {'$var', Backend}:send(Time, Dest, Msg, Opts)
            end}
        ]
    ).


%% @private
backend() ->
    Mod = application:get_env(bondy_net, backend, erlang),
    ok = validate_backend(Mod),
    Mod.

%% @private
validate_backend(erlang) ->
    ok;

validate_backend(Mod) ->
    ensure_loaded(
        backend,
        Mod,
        <<
            "Expected 'erlang', 'partisan' or a module implementing ",
            "bondy_net behaviour."
        >>
    ).

%% @private
broadcast_backend(BackendMod) ->
    Mod =
        case application:get_env(bondy_net, broadcast_backend, undefined) of
            undefined when BackendMod == partisan ->
                partisan;

            Other ->
                Other
        end,
    ok = validate_broadcast_backend(Mod),
    Mod.


%% @private
validate_broadcast_backend(undefined) ->
    ok;

validate_broadcast_backend(Mod) ->
    ensure_loaded(
        broadcast_backend,
        Mod,
        <<"Expected a module implementing 'broadcast/2'">>
    ).



%% @private
ensure_loaded(Key, Value, Expected) when is_atom(Key) ->
    ensure_loaded(atom_to_binary(Key), Value, Expected);

ensure_loaded(Key, undefined, Expected) when is_binary(Key) ->
    erlang:error(
        invalid_config,
        [undefined],
        [{error_info, #{
            cause => #{
                general => <<
                    "value for application configuration key '", Key/binary,
                    "' is missing. ", Expected/binary
                    >>
            }
        }}]
    );

ensure_loaded(Key, Mod, Expected) when is_binary(Key) ->
    case code:ensure_loaded(Mod) of
        {module, Mod} ->
            ok;

        {error, Reason} ->
            erlang:error(
                badbackend,
                [Mod],
                [{error_info, #{cause => #{
                    reason => Reason,
                    general =>
                        <<
                        "Failed to load module '",
                        (atom_to_binary(Mod))/binary, "'. ",
                        Expected/binary
                        >>
                }}}]
            )
    end.
