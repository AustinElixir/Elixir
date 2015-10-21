# Defining Functions In a Module

### Learning Objectives

#### Defining a module

```elixir
defmodule MyModule do 
    ...
end
```

#### Defining a function

Erlang expressions are terminated with a dot . and comma , is used to evaluates multiple expressions within one context (in a function definition, for instance). In Elixir, expressions are delimited by a line break or a semicolon ;.

```elixir
defmodule MyModule do 
    def some_fun(:pattern1) do
        :do; :something
        :ok
    end

    def some_fun(:pattern2) do
        :other
    end

    def some_fun(arg1, arg2), do: [arg1: arg2]
    defp private_fun(), do: :ok
end
```

#### Functions with default argument values

In addition, Elixir allows for default values for arguments, whereas Erlang does not.

```elixir
defmodule Test do
  def sum( a \\ 2, b \\ 2) do
    a + b
  end
end
```

#### Anonymous functions

```elixir
sum = fn(a, b) -> a + b end
sum.(4, 3)
```

There is an extra period in the function call. This is to avoid issues such as:

```elixir
    defmodule Test do
      def hello do
        world = fn(a, b) -> a - b end
        world(1, 2)
      end

      def world(a, b) do
        a + b
      end
    end
```

In Erlang, this isn't an issue because vars always start with a capital letter so there is no confusion between calling a local function or an anonymous function assigned to a Var.

Anonymous functions can also be heavily abbreviated with the following syntax called captures:

```elixir
# Long
[1, 2, 3] |> Enum.map(fn(x) -> x * x end)

# Short
[1, 2, 3] |> Enum.map(&(&1 * &1))
```

#### Comments

Elixir supports heredocs and single line comments.

```elixir
# Single line comment
is_binary """
This is a binary
spanning several
lines.
"""
#=> true
```

#### Printing

```elixir
IO.puts "With a trailing newline"

IO.write "Without a trailing newline"

varname = "Some String"
IO.puts "Supports Interpolation, varname is set to #{varname}"
```

### Exercise

Translate the following Erlang code into Elixir

```erlang
-module(module).
-export([hello/0, hello/1]).

%% Some lines of documentation
%% For this really complex function
hello() -> hello_priv(), true.

hello(homer) ->
    io:format("From Greece or Simpsons?~n");

hello(Name) ->
    io:format("Hello ~p~n", [Name]),
    io:format("It's just wonderful to meet you here in this shell ...~n").

%% This is a private function
hello_priv() -> io:format("Hello There!~n").
```