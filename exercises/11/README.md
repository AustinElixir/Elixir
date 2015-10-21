# Applications and Supervisors

### Learning Objectives

#### Supervisors

Refamiliarize yourself with the Erlang supervisor template [simple_sup.erl](code/simple_sup.erl)

There are some similarities with the following Elixir template.

```elixir
defmodule MyApp.Supervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok)
  end

  @manager_name MyApp.EventManager
  @registry_name MyApp.Registry

  def init(:ok) do
    children = [
      worker(GenEvent, [[name: @manager_name]]),
      worker(MyApp.Registry, [@manager_name, [name: @registry_name]])
    ]

    supervise(children, strategy: :one_for_one)
  end
end
```

You can see that it's much more clear how to define the list of children.

Here is an explanation of the different restart strategies.

| Name | Meaning |
| ---- | ------- |
| one_for_one | If a child process terminates, only that process is restarted. |
| one_for_all | If a child process terminates, all other child processes are terminated, and then all children are restarted together. |
| rest_for_one | If a child process terminates, the rest of the children (that is, the children after the terminated process in the start order) are terminated. Then the terminated child process and the rest of the children in the start order or restarted. |

#### .app file

Take a quick look at the Erlang .app file template [simple.app](code/simple.app) in the code directory.

Recall that the .app file is used by the OTP application library to correctly start your callback module.

The format has not changed in Elixir as that would make it impossible for the Erlang OTP application library to start Elixir apps.

```elixir
{application,myapp,
             [{registered,[]},
              {description,"myapp"},
              {applications,[kernel,stdlib,elixir,logger]},
              {vsn,"0.0.1"},
              {modules,['Elixir.MyApp']}]}.
```

Here is a definition of the configuration options we're most interested in:

| Option | Meaning |
| ------ | ------- |
| application | Application is the name of the application. |
| registered | All names of registered processes started in this application. systools uses this list to detect name clashes between different applications. |
| description | A one-line description of the application. |
| applications | All applications which must be started before this application is allowed to be started. systools uses this list to generate correct start scripts. Defaults to the empty list, but note that all applications have dependencies to (at least) kernel and stdlib. |
| vsn | The version of the application. |
| modules | All modules introduced by this application. |

#### Applications

Take a quick look at the Erlang Application template [simple_app.erl](code/simple_app.erl) in the code directory.

There isn't much to learn with the Elixir application template. It is very simple to understand and extend.

```elixir
defmodule MyApp do
  use Application

  def start(_type, _args) do
    MyApp.Supervisor.start_link
  end
end
```

In the start/2 function you can easily add other tasks if necessary. For the most part, you'll just want to start the supervisor.

### Exercise

#### Create an Application

1. Use the example .app file to create your own.
2. Create a supervisor from the above template.
3. Create your application callback module and add a call to start your supervisor.
4. Create a GenServer and add it to your supervision tree.