# Control Flow

### Learning Objectives

#### Case

```elixir
iex> case {1, 2, 3} do
...>   {4, 5, 6} ->
...>     "This clause won't match"
...>   {1, x, 3} ->
...>     "This clause will match and bind x to 2 in this clause"
...>   _ ->
...>     "This clause would match any value"
...> end
```

#### Cond

```elixir
iex> cond do
...>   2 + 2 == 5 ->
...>     "This will not be true"
...>   2 * 2 == 3 ->
...>     "Nor this"
...>   1 + 1 == 2 ->
...>     "But this will"
...>   true ->
...>     "This is always true (equivalent to else)"
...> end
```

Note cond considers any value besides nil and false to be true:

```erlang
iex> cond do
...>   hd([1,2,3]) ->
...>     "1 is considered as true"
...> end
```

#### If and Unless

If in Elixir is used for testing one condition.

```elixir
iex> if nil do
...>   "This won't be seen"
...> else
...>   "This will"
...> end
"This will"

iex> unless true do
...>   "This will never be seen"
...> end
```

### Exercises

Translate the following Erlang code snippets into Elixir.

```erlang
case {X, Y} of
  {a, b} -> ok;
  {b, c} -> good;
  Else -> Else
end
```

```erlang
Test_fun = fun (X) ->
  if X > 10 ->
       greater_than_ten;
     X < 10, X > 0 ->
       less_than_ten_positive;
     X < 0; X =:= 0 ->
       zero_or_negative;
     true ->
       exactly_ten
  end
end.

Test_fun(11).
%=> greater_than_ten

Test_fun(-2).
%=> zero_or_negative

Test_fun(10).
%=> exactly_ten
```

```erlang
Num = 10.
if Num > 20 -> greater_than_20;
   true -> less_than_20
end.
```