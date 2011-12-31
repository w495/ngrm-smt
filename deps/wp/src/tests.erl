%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% ФУНКЦИИ ТЕСТИРОВАНИЯ
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(tests).
-export([
            print_speed/2,
            print_speed/3,
            utime/0,
            test/0
        ]
).


iter_test(0, _) ->
    ok;
iter_test(X, F) ->
    F(),
    iter_test(X-1, F).

utime() ->
    {_M,S,Mi} = now(), S*1000000+Mi.


print_speed(Fun, X) ->
    print_speed("", Fun, X).

print_speed(Name, Fun, X) ->
    Start = utime(),
    iter_test(X, Fun),
    Stop = utime(),
    io:format("Test::: ~p (~p X ~s}~n", [Stop - Start, X, Name]).


% ===========================================================================

-include_lib("eunit/include/eunit.hrl").
test()->
    ok.
