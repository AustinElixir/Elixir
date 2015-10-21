defmodule MyModule do 
    @doc """
    Some lines of documentation
    For this really complex function
    """
    def hello do hello_priv; :true end
    
    # This is a private function
    defp hello_priv do IO.puts "Hello There!" end

    # Interesting there is no warning if this is moved to the second clause
    def hello(:homer) do
        IO.puts "From Greece or Simpsons?"
    end

    def hello(name) do
        IO.write """
            Hello #{name}
            It's just wonderful to meet you here in this shell ...
            """
            # Heredocs are smart, the amount of whitespace preceding
            # this terminator is removed from all preceeding line.
    end

end