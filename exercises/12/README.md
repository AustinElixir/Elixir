# Mix and Hex.pm

### Learning Objectives

#### Creating a Project

When you create a new project using mix all the default files needed will be generated for you to get started.

```bash
$ mix new hello --module Hello
* creating README.md
* creating .gitignore
* creating mix.exs
* creating config
* creating config/config.exs
* creating lib
* creating lib/hello.ex
* creating test
* creating test/test_helper.exs
* creating test/hello_test.exs

Your Mix project was created successfully.
You can use "mix" to compile it, test it, and more:

    cd hello
    mix test

Run "mix help" for more commands.
```

Type `mix help` for a full list of commands you can use. Typing `mix help new` will provide an explanation of all the different options.

#### mix.exs

Mix will create an Elixir Script file for you called "mix.esx".

```elixir
defmodule KV.Mixfile do
  use Mix.Project

  def project do
    [app: :kv,
     version: "0.0.1",
     elixir: "~> 1.0",
     deps: deps]
  end

  def application do
    [applications: [:logger]]
  end

  defp deps do
    []
  end
end
```

When you compile your project mix will use this script to generate things like the .app file you manually created in the last exercise.

You can build your project with `mix compile`.

#### Hex.pm

Hex is the package management utility and is fully integrated with mix. You can search for packages and get information about them from the command line.

```bash
$ mix hex.search "jsx"
exjsx
jsx
jsxd
jsxn

$ mix hex.info exjsx
exjsx
  Releases: 3.2.0, 3.1.0, 3.0.2, 3.0.1, 3.0.0

  Contributors: alisdair sullivan, devin torres, eduardo gurgel, d0rc, igor kapkov, parroty, yurii rashkovskii
  Licenses: MIT
  Links:
    GitHub: https://github.com/talentdeficit/exjsx

json for elixir

$ 
```

Once you locate a package you'd like included in your project, add it to the deps function in your mix.exs file.

```elixir
  defp deps do
    [ {:exjsx, "3.2.0"} ]
  end
```

The format for each entry is {:packageName, "Version / Regex"}.

Some commands to use are:

- mix deps.get
- mix deps.compile
- mix deps.update

### Exercises

#### Hex.pm search

It's not very convenient to use the shell to locate packages in the hex.pm repository. Visit [https://hex.pm](https://hex.pm) and search for some packages. You'll see it searched by description whereas on the cli it does not. You also can see the version numbers, number of downloads, and more.

Try adding something to your project. Usually there will be a link back to the github page for the project containing usage info.

#### Create an Project

Create your own mix project and bring the GenServer, Supervisor, and Application files you created in the last exercise over. Get them integrated into your project so you can have mix build them and generate the .app file for you.