# Operators

### Learning Objectives

#### Operator changes from Erlang -> Elixir

| Erlang | Elixir | Meaning |
| ------ | ------ | ------- |
| and | Not Available | Logical ‘and’, evaluates both arguments |
| andalso | and | Logical 'and’, short-circuits |
| or | Not Available | Logical 'or’, evaluates both arguments |
| orelse | or | Logical 'or’, short-circuits |
| =:= | === | A match operator |
| =/= | !== | A negative match |
| /= | != | Not equals |
| =< | <= | Less than or equals |

#### String Concatenation

```elixir
iex> "foo" <> "bar"
"foobar"
```

#### List Operators

Same as erlang

```elixir
iex> [1,2,3] ++ [4,5,6]
[1,2,3,4,5,6]
iex> [1,2,3] -- [2]
[1,3]
```

#### Booleans - and, or, not

In Elixir, and/or/not only accept boolean types :true and :false

```elixir
iex> true and true
true
iex> false or is_atom(:example)
true

iex> 1 and true
** (ArgumentError) argument error

iex> false and raise("This error will never be raised")
false

iex> true or raise("This error will never be raised")
true
```

Also note you can use :true and :false

#### Booleans - !, ||, &&

These special operators will treat all things not :false or :nil as true.

```elixir
# or
iex> 1 || true
1
iex> false || 11
11

# and
iex> nil && 13
nil
iex> true && 17
17

# !
iex> !true
false
iex> !1
false
iex> !nil
true
```

#### Data Type Comparison Precedence

`number < atom < reference < functions < port < pid < tuple < maps < list < bitstring`

#### Pipeline Operator

One of the nicest additions to Elixir is this pipeline operator that will eliminate staircasing and transitory variables.

In Erlang, these are two very common patterns:

```erlang
pp(X) ->
  list_to_binary(
    lists_flatten(
      li_lib:format("~p),[X])
    )
  ).
```

```erlang
pp(X) ->
  Var1 = li_lib:format("~p),[X]),
  Var2 = lists_flatten(Var1),
  list_to_binary(Var2).
```

It's a struggle to decide which is more readable sometimes. In Elixir, the pipeline operator solves this brilliantly.

```elixir
def pp(x) do 
    :io_lib.format("~p", [x])
    |> :lists.flatten
    |> :erlang.list_to_binary
end
```

It's very obvious where the data flows and there are no transitory variable bindings. The output from each call is passed as the first argument to the next.

### Exercises

Just play with these operators in the shell until comfortable.