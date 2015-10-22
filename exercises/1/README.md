# Interactive Shell

### Task 1

Open the Elixir repl by typing `iex` in your shell.

If in a vagrant VM use `iex --erl "-smp enable"`, this will enable us to use the observer wx module later.

Explore the shell features by typing `h` and hitting enter.

Review all the helper functions defined by: [IEx.Helpers](http://elixir-lang.org/docs/v1.0/iex/IEx.Helpers.html)

Pull up documentation of a module's function with: `h(Enum.map/2)`

Review the documentation on the [IEx](http://elixir-lang.org/docs/stable/iex/IEx.html)

### Task 2

Define a simple module in the shell.

```elixir
defmodule ModuleName do
  def hello do
    IO.puts "Hello World"
  end
end
```

Call the code with: `ModuleName.hello`

### Task 3

Type `[{:atom, 1}, {:atom2, 2}]` in the shell. 

Note that shell output is formated by Kernel.inspect and will transform this to a Keyword list.

Kill the shell with CTRL-C / CTRL-D or with `System.halt`

### Task 4

Type an incomplete mutliline expression in the shell. Then stop the shell from pending the final expression with `#iex:break`