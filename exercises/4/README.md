# Understandng Macro Expansion

### Prep

#### Open an Elixir shell and define the following function

```elixir
unbeam = fn(module) ->
    {_, beam, _} = :code.get_object_code(module)
    {:ok,{_,[{:abstract_code,{_,ac}}]}} = :beam_lib.chunks(beam,[:abstract_code])
    :erl_syntax.form_list(ac) |>
    :erl_prettypr.format |>
    IO.puts
end
```

### Exercises

Compile each of the following snippets in a file.

#### Default arguments in functions

arg_defaults.ex

```elixir
defmodule ArgDefaults do
  def sum( a \\ 3, b, c \\ 7) do
    a + b + c
  end
end

c("arg_defaults.ex")
unbeam.(ArgDefaults)
```

- What did the function definition expand into?
- Does `ArgDefaults.sum(0)` behave like you expect?

#### Variable rebinding

rebinding.ex

```elixir
defmodule Rebinding do
  def test do
    a = 1
    a = 2
    a
  end
end

c("rebinding.ex")
unbeam.(Rebinding)
```

- Why do you see the warning `rebinding.ex:3: warning: variable a is unused` upon compilation?
- What is really happening with variable rebinding?

#### and; or

and.ex

```elixir
defmodule And do
  def test do
    1 and 2 and 3 and 4
  end
end

c("and.ex")
unbeam.(And)
```

- How is and implemented in Elixir?
- Consider that `E1 andalso E2` in erlang works like:

```erlang
case E1 of
   false -> false;
   true  -> case E2 of
       false -> false;
       true  -> true
   end
end
```

#### Pipelines

pipelines.ex

```elixir
defmodule Pipelines do
  def test do
       [1, [2], 3]
    |> List.flatten
    |> Enum.map(fn x -> x * 2 end)
    |> IO.puts
  end
end

c("pipelines.ex")
unbeam.(Pipelines)
```

- How are pipelines implemented?
- Consider the following situation, it's important to be aware of such edge cases.

```elixir
defmodule Test do
  def f(x) do
    fn(y) -> x + y end
  end

  def f(x, y) do
    x - y
  end
end

3 |> Test.f(2)
# 5 or 1?
```

It's important to keep in mind you'll need to use `3 |> Test.f(2).()` if you want to pipe into the lambda.