-module(bondy_net_gen).

-include("bondy_net.hrl").

-moduledoc #{format => "text/markdown"}.
?MODULEDOC("""
This module uses parse transforms to replace the `bondy_net` module with a concrete `backend` module,
""").

-export([do/0]).

-compile({parse_transform, parse_trans_codegen}).


do() ->
    Backend = application:get_env(bondy_net, backend, erlang),
    Expected = <<
        "Expected 'erlang', 'partisan' or a module implementing ",
        "bondy_net behaviour."
    >>,
    ok = ensure_loaded(backend, Backend, Expected),
    Forms = forms(Backend),
    {ok, bondy_net, Bin} = compile:forms(Forms),
    code:load_binary(bondy_net, "nofile", Bin),
    ok.


%% =============================================================================
%% PRIVATE
%% =============================================================================

forms(erlang) ->
    codegen:gen_module(
        bondy_net,
        [
            {broadcast, 1},
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
            {broadcast, fun(Msg) ->
                Mod = application:get_env(bondy_net, broadcast_backend),
                Mod:broadcast(Msg)
            end},
            {broadcast, fun(Msg, HandlerMod) ->
                Mod = application:get_env(bondy_net, broadcast_backend),
                Mod:broadcast(Msg, HandlerMod)
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

forms(partisan) ->
    codegen:gen_module(
        bondy_net,
        [
            {broadcast, 1},
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
            {broadcast, fun(Msg) ->
                case bondy_mst_config:get(broadcast_backend, undefined) of
                    undefined ->
                        error(bad_broadcast_backend);
                    Mod ->
                        Mod:broadcast(Msg)
                end
            end},
            {is_alive, fun() ->
                partisan:is_alive()
            end},
            {is_connected, fun(Node) ->
                lists:member(Node, partisan:nodes())
            end},
            {is_connected, fun(Node, _) ->
                %% No channels in Erlang
                lists:member(Node, partisan:nodes()) end
            },
            {monitor_node, fun(Node, Flag) ->
                partisan:monitor_node(Node, Flag)
            end},
            {monitor_node, fun(Node, Flag, Options) ->
                partisan:monitor_node(Node, Flag, Options)
            end},
            {monitor_nodes,
                fun(Flag) -> partisan:monitor_nodes(Flag)
            end},
            {monitor_nodes, fun(Flag, Options) ->
                partisan:monitor_nodes(Flag, Options)
            end},
            {node, fun() ->
                partisan:node()
            end},
            {node, fun(Arg) ->
                partisan:node(Arg)
            end},
            {nodes, fun() ->
                partisan:nodes()
            end},
            {nodes, fun(Arg) ->
                partisan:nodes(Arg)
            end},
            {send, fun(Dest, Msg) ->
                partisan:send(Dest, Msg)
            end},
            {send, fun(Dest, Msg, Opts) ->
                partisan:send(Dest, Msg, Opts)
            end},
            {send_after, fun(Time, Dest, Msg) ->
                partisan:send(Time, Dest, Msg)
            end},
            {send_after, fun(Time, Dest, Msg, Opts) ->
                partisan:send(Time, Dest, Msg, Opts)
            end}
        ]
    ).

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


ensure_loaded(_, erlang, _) ->
    %% Always loaded
    ok;

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
