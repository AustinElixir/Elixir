# Enum and Stream

### Learning Objectives

#### Enum map/reduce

```elixir
iex> Enum.map(1..3, fn x -> x * 2 end)
[2, 4, 6]
iex> Enum.reduce(1..3, 0, &+/2)
6

iex> dict = %{a: 1, b: 2}
iex> Enum.map(dict, fn {k, v} -> {k, v * 2} end)
[a: 2, b: 4]
```

Also consider this example where full iteration of the list in done at each map.

```elixir
1..3 |>
  Enum.map(&IO.inspect(&1)) |>
  Enum.map(&(&1 * 2)) |>
  Enum.map(&IO.inspect(&1))
1
2
3
2
4
6
#=> [2, 4, 6]
```

#### Stream 

If the previous map example is modified to use streams, you'll see that we process one element from the list at each step.

```elixir
stream = 1..3 |>
  Stream.map(&IO.inspect(&1)) |>
  Stream.map(&(&1 * 2)) |>
  Stream.map(&IO.inspect(&1))
Enum.to_list(stream)
1
2
2
4
3
6
#=> [2, 4, 6]
```

### Exercises

#### Translations

Translate the following Erlang code snippets into Elixir.

```erlang
> lists:map(fun({A,B}) -> A * B end, [{1,2}, {3,4}, {5,6}]).
[2,12,30]
```

```erlang
> lists:foldl(fun(X, Sum) -> X + Sum end, 0, [1,2,3,4,5]). 
15
```

#### Docs

Check out the Elixir documentation on the [Enum](http://elixir-lang.org/docs/stable/elixir/Enum.html) and [Stream](http://elixir-lang.org/docs/stable/elixir/Stream.html) modules and familiarize yourself with all the different functions.

#### Enum.map/2 Implementation

Take a look at the source for the Enum module on github.

- [Enum.map](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/enum.ex#L1077-L1090)
- [Enum.reduce](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/enum.ex#L1468-L1470)

It's frequently important to ensure you understand how library functions are implemented. Many times these Elixir library functions will be wrappers for Erlang library functions and their behavior needs to be understood.

#### Stream.map/2 Implementation

In your Elixir shell type `:observer.start` and familiarize yourself with the tabs.

Get your shell's PID with `self` and note the number. Locate the PID from the "Process" table, right click it, and select "Trace processes".

In the "Trace Overview" tab, click the "Add Trace Pattern" button at the bottom. Then select the Elixir.Stream module and click "OK". Then click "Check Visible", "OK", "Return Trace" and "OK".

Finally, click "Start Trace" in the bottom right and a blank window will appear.

In your shell, type: `stream = Stream.map(1..3, &(&1 * 2))`.

You should see something like this in your trace log:

```erlang
10:45:25:059747 (<0.57.0>) call 'Elixir.Stream':'__info__'(macros)
10:45:25:059755 (<0.57.0>) returned from 'Elixir.Stream':'__info__'/1 -> []
10:45:25:060168 (<0.57.0>) call 'Elixir.Stream':map(#{'__struct__' => 'Elixir.Range',first => 1,last => 3},#Fun<erl_eval.6.54118792>)
10:45:25:060179 (<0.57.0>) returned from 'Elixir.Stream':map/2 -> #{'__struct__' => 'Elixir.Stream',
                                                                    accs => [],
                                                                    done => nil,
                                                                    enum => #{'__struct__' => 'Elixir.Range',
                                                                      first => 1,
                                                                      last => 3},
                                                                    funs => [#Fun<Elixir.Stream.61.16257587>]}
```

- Notice how the return from Stream.map/2 is a struct but is presented as `#Stream<[enum: 1..3, funs: [#Function<61.16257587/1 in Stream.map/2>]]>` in the shell.

The following is the code from the [reduce.ex](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/stream/reducers.ex#L103-L109) and [stream.ex](https://github.com/elixir-lang/elixir/blob/master/lib/elixir/lib/stream.ex) modules.

```elixir
# Stream.Reduce
  defmacro map(callback, f \\ nil) do
    quote do
      fn(entry, acc) ->
        next(unquote(f), unquote(callback).(entry), acc)
      end
    end
  end

# Stream
  defmacrop next(f, entry, acc) do
    quote do: unquote(f).(unquote(entry), unquote(acc))
  end

  def map(enum, fun) do
    lazy enum, fn(f1) -> R.map(fun, f1) end
  end

  defp lazy(%Stream{done: nil, funs: funs} = lazy, fun),
    do: %{lazy | funs: [fun|funs] }
  defp lazy(enum, fun),
    do: %Stream{enum: enum, funs: [fun]}

  defp lazy(%Stream{done: nil, funs: funs, accs: accs} = lazy, acc, fun),
    do: %{lazy | funs: [fun|funs], accs: [acc|accs] }
  defp lazy(enum, acc, fun),
    do: %Stream{enum: enum, funs: [fun], accs: [acc]}

  defp lazy(%Stream{done: nil, funs: funs, accs: accs} = lazy, acc, fun, done),
    do: %{lazy | funs: [fun|funs], accs: [acc|accs], done: done}
  defp lazy(enum, acc, fun, done),
    do: %Stream{enum: enum, funs: [fun], accs: [acc], done: done}
end
```

- Can you follow what is happening when you call Stream.map/2?

Next run `Stream.take(stream, 1)` in the shell and check the trace log.

- What do you see in the %Stream{accs} field?
- What do you suppose the functions do that are stored in %Stream{funs}?

- TO DO:
  - Walk thru each step of the stream |> Enum.to_list trace