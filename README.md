# bondy_net

`bondy_net` is a backend-agnostic Erlang application that provides a unified interface over either **Distributed Erlang**, **[Partisan](https://partisan.dev)** or other disterl alternatives. 

It allows your application to switch distribution backends **without modifying application code**.

Notice `bondy_net` doesn't use [Bondy](bondy.io) (the application networking platform). It is called `bondy_net` as it is part of the Bondy suite of libraries.

## Why Use `bondy_net`?
* Switch between distribution backends without rewriting your code
* Keep application logic decoupled from distribution mechanics

## Features
* Unified API for sending messages, monitor nodes and obtaining information
* Supports both **Distributed Erlang** and **Partisan**
* Backend selection via environment variable or application config
* Uses parse transforms for **zero-runtime dispatch cost**

## Installation
Add to your `rebar.config` dependencies:

{deps, [
    bondy_net
]}.

Make sure you also:
* include `bondy_net` as an application in your `app.src` file. At startup the application will generate the `bondy_net` module using parse transforms based on your configuration
* include `partisan` or your custom backend if you’re are not using disterl.


## Configuration
Set the backend distribution system via `sys.config`:

```erlang
{bondy_net, [
    {backend, partisan}
]}.
```

## Usage
Use `bondy_net` module just like you’d use `erlang` (`net_kernel`) and `partisan`.
Check the implemented functions in `bondy_net` as only a subset of functions is implemented.

## Broadcasting
Since there is no implementation for broadcasting natively in Erlang a call to 
`bondy_net:broadcast/2` will fail unless you also provide a `broadcast_backend` module that implements the callback e.g. `my_module` in the following example.

```erlang
{bondy_net, [
    {backend, erlang},
    {broadcast_backend, my_module}
]}.
```

