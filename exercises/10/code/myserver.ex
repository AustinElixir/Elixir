defmodule MyServer do
  use GenServer

  def start_link() do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  # Callbacks
  def handle_call({:hello, name}, _from, state) do
    {:reply, "Hello #{name}", state}
  end
end