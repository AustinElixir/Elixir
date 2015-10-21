defmodule Case do 
    def test(var) do
        case {1, 2, 3} do
            {1, var, 3} -> var # not matching, rebinding var
            _ ->:not_found
        end
    end
end