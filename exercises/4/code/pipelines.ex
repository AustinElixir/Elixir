defmodule Pipelines do
  def test do
       [1, [2], 3]
    |> List.flatten
    |> Enum.map(fn x -> x * 2 end)
    |> IO.puts
  end
end