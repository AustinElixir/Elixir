# Data

### Learning Objectives

#### Data Types

| Type | Erlang | Elixir |
| ---- | ------ | ------ |
| Integer | 1 | 1 |
| Float | 1.0 | 1.0 |
| Atom | atom | :atom |
| Reference | #Ref<0.0.2.309> | #Reference<0.0.1.131> |
| Function | #Fun<erl_eval.20.54118792> | #Function<20.54118792/0 |
| Port | #Port<0.2356> | #Port<0.2356> |
| Pid | &lt;0.93.0&gt; | #PID&lt;0.57.0&gt; |
| Tuple | {} | {} |
| Map | #{key => 0} | %{:key => 0} |
| List | [] | [] |
| Binaries | <<"">> | "" or <<"">> |

#### Type Check Functions

- is_atom/1
- is_binary/1
- is_bitstring/1
- is_boolean/1
- is_float/1
- is_function/1
- is_function/2
- is_integer/1
- is_list/1
- is_map/1
- is_nil/1
- is_number/1
- is_pid/1
- is_port/1
- is_reference/1
- is_tuple/1

#### Variables

Variables can be rebound in Elixir. In order to avoid a rebinding operation preface your var with a pin operator ^

```elixir
a = 1
a = 2 # rebind
^a = 2 # pattern match
```

It's very important to understand when you want to rebind or not. Consider this:

```elixir
defmodule Case do 
    def test(var) do
        case {1, 2, 3} do
            {1, var, 3} -> var # not matching, rebinding var
            _ ->:not_found
        end
    end
end

iex(60)> Case.test(99)
2
```

We want it to return :not_found for that call. To do that you'll need to use ^var in the case clause.

```elixir
defmodule Case do 
    def test(var) do
        case {1, 2, 3} do
            {1, ^var, 3} -> var # now we're matching with the value of var
            _ ->:not_found
        end
    end
end

iex> Case.test(99)
:not_found
iex> Case.test(2)
2
```

Try to avoid function calls without parenthesis in your code as it makes it harder to reason about. e.g.

```elixir
iex> self
#PID<0.57.0>
iex> self = 1
1
iex> self
1
iex> self()
#PID<0.57.0>
```

In source code files it can be much more ambiguous when reading code containing function calls without parenthesis. Especially if same named vars are close to the scope of the function.

#### Binaries

In Elixir, a "string" is represented as a UTF-8 encoded binary. Erlang represents "strings" as char lists. When making calls to Erlang modules that need a string in a char list format, use 'string' instead of "string".

```elixir
iex> string = "hello"
"hello"
iex> is_binary string
true

iex> is_list('string')    
true
iex> is_list("string")
false
```

Check the [String](http://elixir-lang.org/docs/v1.1/elixir/String.html) module documentation for a list of functions useful for manipulating binary strings.

#### Charlists

A char list is nothing more than a list of characters:

```elixir
iex> 'hełło'
[104, 101, 322, 322, 111]
iex> is_list 'hełło'
true
iex> 'hello'
'hello'
```

You might need to use `to_char_list` to convert data types when calling erlang modules.

```elixir
iex> to_char_list "hełło"
[104, 101, 322, 322, 111]
iex> to_string 'hełło'
"hełło"
iex> to_string :hello
"hello"
iex> to_string 1
"1"
```

#### Keywords

Elixir offers a literal syntax for creating a list of two-item tuples where the first item in the tuple is an atom and calls them keyword lists. In Erlang, these are known as proplists.

```elixir
kw = [another_key: 20, key: 10]
kw[:another_key]
#=> 20
```

Something to note, this is syntactic sugar for [{:another_key, 20}, {:key, 10}]

This can lead to some confusion and just realize the lack of the colon before the :atoms in the sugary version doesn't mean they aren't :atoms.

```elixir
iex> kw = [key1: :one, key2: :two]
[key1: :one, key2: :two]
iex> 
nil
iex> kw[key1]
** (CompileError) iex:70: undefined function key1/0
    (stdlib) lists.erl:1353: :lists.mapfoldl/3
    (stdlib) lists.erl:1354: :lists.mapfoldl/3
iex> kw[:key1]
:one
```

You can easily add items to the Keyword list:

```elixir
iex> list ++ [c: 3]
[a: 1, b: 2, c: 3]
iex> [a: 0] ++ list
[a: 0, a: 1, b: 2]
```

Some Rules:

- Keys must be atoms.
- Keys are ordered, as specified by the you.
- You can have duplicate keys.

#### Maps

Whenever you need a key-value store, maps are the “go to” data structure in Elixir. A map is created using the %{} syntax:

```elixir
iex> map = %{:a => 1, 2 => :b}
%{2 => :b, :a => 1}
iex> map[:a]
1
iex> map[2]
:b
iex> map[:c]
nil
# syntax sugar to update a map
iex> %{map | :a => 2}
%{:a => 2, 2 => :b}

# Or use
iex> Map.put(map, :a, :bye)
%{2 => :b, :a => :bye}
iex> Map.put(map, :c, :bye)
%{2 => :b, :a => :hi, :c => :bye}
iex> 
```

Some Rules:

- Maps allow any value as a key.
- There can not be duplicate keys.
- Maps are completely unordered.

The [HashDict](http://elixir-lang.org/docs/stable/elixir/HashDict.html) module is also available. This was created to serve as a more robust key/value store than the maps on R17. If you are using R18 or greater I'm not sure if there is any reason to use a HashDict as the performance is extremely good.

#### Structs

Structs are extensions built on top of maps that provide compile-time checks and default values.

```elixir
iex> defmodule User do
...>   defstruct name: "John", age: 27
...> end

iex> %User{}
%User{age: 27, name: "John"}
iex> %User{name: "Meg"}
%User{age: 27, name: "Meg"}
```

Structs provide compile-time guarantees that only the fields (and all of them) defined through defstruct will be allowed to exist in a struct:

```elixir
iex> %User{oops: :field}
** (CompileError) iex:3: unknown key :oops for struct User
```

#### [Dicts](http://elixir-lang.org/getting-started/maps-and-dicts.html#dicts)

In Elixir, both keyword lists and maps are called dictionaries. In other words, a dictionary is like an interface (we call them behaviours in Elixir) and both keyword lists and maps modules implement this interface.

```elixir
iex> keyword = []
[]
iex> map = %{}
%{}
iex> Dict.put(keyword, :a, 1)
[a: 1]
iex> Dict.put(map, :a, 1)
%{a: 1}
```

### Exercises

Translate the following Erlang shell code snippets into Elixir using the appropriate data structures / types.

```erlang
Proplist = [{another_key, 20}, {key, 10}].
proplists:get_value(another_key, Proplist).
%=> 20
```

```erlang
Map = #{key => 0}.
Updated = Map#{key := 1}.
#{key := Value} = Updated.
Value =:= 1.
%=> true
```

```erlang
My_string = "chars".
My_binary = <<"chars">>.
is_list(My_string) and is_binary(My_binary).
%=> true
```