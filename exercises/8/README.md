# Messaging

### Learning Objectives

#### Spawn

```elixir
iex> pid = spawn fn -> 1 + 2 end
#PID<0.44.0>
iex> Process.alive?(pid)
false
```

#### Send & Receive

```elixir
send self(), {:hello, "world"}
Process.info(self, :messages)

receive do
  {:hello, msg} -> msg
  {:world, msg} -> "won't match"
    after
      1_000 -> "nothing after 1s"
end

Process.info(self, :messages)
```

#### Tasks

```elixir
iex> pid = self                   
#PID<0.57.0>
iex> Task.start fn -> send pid, {:hello, "world"} end

Process.info(self, :messages) 
```

Tasks will provide better error logging than spawn.

```elixir
iex> spawn fn -> raise "oops" end
#PID<0.58.0>

[error] Error in process <0.58.0> with exit value: ...
```
VS.
```elixir
iex(1)> Task.start fn -> raise "oops" end
{:ok, #PID<0.55.0>}

15:22:33.046 [error] Task #PID<0.55.0> started from #PID<0.53.0> terminating
Function: #Function<20.90072148/0 in :erl_eval.expr/5>
    Args: []
** (exit) an exception was raised:
    ** (RuntimeError) oops
        (elixir) lib/task/supervised.ex:74: Task.Supervised.do_apply/2
        (stdlib) proc_lib.erl:239: :proc_lib.init_p_do_apply/3
```

Besides providing better error logging, there are a couple other differences: start/1 and start_link/1 return {:ok, pid} rather than just the PID. This is what enables Tasks to be used in supervision trees. Furthermore, tasks provides convenience functions, like Task.async/1 and Task.await/1, and functionality to ease distribution.

### Exercises

#### Translations

Translate the following Erlang code snippet into Elixir.

```erlang
Pid = self().

Pid ! {hello}.

receive
  {hello} -> ok;
  Other -> Other
after
  10 -> timeout
end.
```