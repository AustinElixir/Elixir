-module(module).
-export([hello/0, hello/1]).

%% Some lines of documentation
%% For this really complex function
hello() -> hello_priv(), true.

hello(homer) ->
    io:format("From Greece or Simpsons?~n");

hello(Name) ->
    io:format("Hello ~p~n", [Name]),
    io:format("It's just wonderful to meet you here in this shell ...~n").

%% This is a private function
hello_priv() -> io:format("Hello There!~n").