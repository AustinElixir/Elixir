-compile(no_auto_import).

-file("test.ex", 1).

-module('Elixir.Test').

-export(['__info__'/1, sum/1, sum/2, sum/3]).

-spec({{'__info__', 1},
       [{type, 1, 'fun',
         [{type, 1, product, [{type, 1, atom, []}]},
          {type, 1, union,
           [{type, 1, atom, []},
            {type, 1, list,
             [{type, 1, tuple,
               [{type, 1, atom, []}, {type, 1, any, []}]}]}]}]}]}).

'__info__'(functions) -> [{sum, 1}, {sum, 2}, {sum, 3}];
'__info__'(macros) -> [];
'__info__'(info) ->
    erlang:get_module_info('Elixir.Test', info).

sum(x0@1) -> sum(3, x0@1, 7).

sum(x0@1, x1@1) -> sum(x0@1, x1@1, 7).

sum(a@1, b@1, c@1) -> a@1 + b@1 + c@1.
