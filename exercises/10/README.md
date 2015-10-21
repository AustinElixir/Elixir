# GenServers and Agents

### Learning Objectives

#### GenServer

Take a quick look at the Erlang GenServer template [simple_gen.erl](code/simple_gen.erl) in the code directory.

Note that with the GenServer behavior in Erlang we are forced to maintain a minimum number of callbacks.

Elixir has abstracted this to the point where the following is a valid GenServer process:

```elixir
defmodule MyServer do
  use GenServer
end
```

The only thing not provided by default is a starting function. Adding one allows this code to be spawned into a registered process.

```elixir
defmodule MyServer do
  use GenServer

  def start_link(opts \\ []) do
    GenServer.start_link(__MODULE__, :ok, opts)
  end
end

iex> MyServer.start_link
{:ok, #PID<0.93.0>}
iex> 
```

The remainder of the [GenServer](http://elixir-lang.org/docs/stable/elixir/GenServer.html) functionality remains close to the Erlang GenServer.

The nice thing about this level of abstraction is you can keep your source very clean. All you need to do is add the callbacks you care about and rely on the default behavior for those you don't care about.

```elixir
defmodule MyServer do
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  # Callbacks
  def handle_call({:hello, name}, _from, state) do
    {:reply, "Hello #{name}", state}
  end
end

iex> MyServer.start_link
{:ok, #PID<0.63.0>}
iex> GenServer.call(MyServer, {:hello, "Tony"})
"Hello Tony"
iex> 
```

You can see above we passed an option to name the genserver. This will register the process and disallow you from calling `MyServer.start_link` again as long as the process is alive.

If you need more information see the Elixir guide for [GenServer](http://elixir-lang.org/getting-started/mix-otp/genserver.html).

#### Agents

Agents are very simplified state wrappers. It's very common in Erlang to create a genserver just to maintain some state. If that's all you require you may use an Agent in Elixir.

```elixir
iex> {:ok, agent} = Agent.start_link fn -> [] end
{:ok, #PID<0.57.0>}
iex> Agent.update(agent, fn list -> ["eggs"|list] end)
:ok
iex> Agent.get(agent, fn list -> list end)
["eggs"]
iex> Agent.stop(agent)
:ok
```

### Exercises

#### GenServer Implementation

Use the unbeam function to see the code that was compiled for the very basic "myserver.ex" module.

```elixir
unbeam = fn(module) ->
    {_, beam, _} = :code.get_object_code(module)
    {:ok,{_,[{:abstract_code,{_,ac}}]}} = :beam_lib.chunks(beam,[:abstract_code])
    :erl_syntax.form_list(ac) |>
    :erl_prettypr.format |>
    IO.puts
end
```

If you run `c("myserver_simple.ex")` and `unbeam.(MyServer_simple)` you should see:

```elixir
-compile(no_auto_import).

-file("myserver_simple.ex", 1).

-module('Elixir.MyServer_simple').

-behaviour(gen_server).

-export(['__info__'/1, code_change/3, handle_call/3,
         handle_cast/2, handle_info/2, init/1, terminate/2]).

-spec({{'__info__', 1},
       [{type, 1, 'fun',
         [{type, 1, product, [{type, 1, atom, []}]},
          {type, 1, union,
           [{type, 1, atom, []},
            {type, 1, list,
             [{type, 1, tuple,
               [{type, 1, atom, []}, {type, 1, any, []}]}]}]}]}]}).

'__info__'(functions) ->
    [{code_change, 3}, {handle_call, 3}, {handle_cast, 2},
     {handle_info, 2}, {init, 1}, {terminate, 2}];
'__info__'(macros) -> [];
'__info__'(info) ->
    erlang:get_module_info('Elixir.MyServer_simple', info).

-file("lib/gen_server.ex", 408).

init(_@1) -> {ok, _@1}.

-file("lib/gen_server.ex", 443).

code_change(_@1, _@2, _@3) -> {ok, _@2}.

-file("lib/gen_server.ex", 428).

handle_cast(_@1, _@2) ->
    _@3 = {bad_cast, _@1},
    case erlang:phash2(1, 1) of
      0 -> erlang:exit(_@3);
      1 -> {stop, _@3, _@2}
    end.

-file("lib/gen_server.ex", 423).

handle_info(_@1, _@2) -> {noreply, _@2}.

-file("lib/gen_server.ex", 438).

terminate(_@1, _@2) -> ok.

-file("lib/gen_server.ex", 413).

handle_call(_@1, _@2, _@3) ->
    _@4 = {bad_call, _@1},
    case erlang:phash2(1, 1) of
      0 -> erlang:exit(_@4);
      1 -> {stop, _@4, _@3}
    end.

```

- How does this match up against the default Erlang GenServer template?

#### Implementing Agents

Create your own agent and update its state. Use `:observer.start` to inspect the process.

- Do you notice anything interesting about the process information? Such as the current function.