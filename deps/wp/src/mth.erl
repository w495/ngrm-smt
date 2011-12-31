-module(mth).
-export([
        adiv/2, adiv/3,
        amul/2, amul/3,
        test/1
    ]).


-define(BASE, 576460752303423488). %% <= 2^59

-define(DIV(X, Y), round(?BASE / Y * X)).
-define(MUL(X, Y), round(X / ?BASE * Y)).


adiv(X, Y, Base)->   round(Base / Y * X).
amul(X, Y, Base)->   round(X / Base * Y).

adiv(X, Y)->   adiv(X, Y, ?BASE).
amul(X, Y)->   amul(X, Y, ?BASE).

test(speed)->
    io:format("~nDIV TEST ~n~n", []),

    lists:map(fun(X)->
        tests:print_speed("?DIV",
            fun() ->
                ?DIV(1, X)
            end, 100000),

        tests:print_speed("adiv",
            fun() ->
                adiv(1, X)
            end, 100000),

        tests:print_speed("float div",
            fun() ->
                1 / X
            end, 100000),

        tests:print_speed("sum float div",
            fun() ->
                X + 1 / X
            end, 100000),

        tests:print_speed("float_to_list(sum float div)",
            fun() ->
                erlang:float_to_list(X + 1 / X)
            end, 100000),

        tests:print_speed("term_to_binary(sum float div)",
            fun() ->
                erlang:term_to_binary(X + 1 / X)
            end, 100000),
        io:format("~n--------------------~n", []),
        ok
        end,
        lists:seq(100, 110)
    ),

    io:format("~nMACRO TEST ~n~n", []),

    lists:map(fun(X)->
        tests:print_speed("?DIV",
            fun() ->
                ?DIV(1, X)
            end, 10000000),

        tests:print_speed("adiv",
            fun() ->
                adiv(1, X)
            end, 10000000),
        io:format("~n--------------------~n", []),
        ok
        end,
        lists:seq(100, 110)
    ),

    lists:map(fun(X)->
        tests:print_speed("?MUL",
            fun() ->
                ?MUL(1, X)
            end, 10000000),

        tests:print_speed("amul",
            fun() ->
                amul(1, X)
            end, 10000000),
        io:format("~n--------------------~n", []),
        ok
        end,
        lists:seq(100, 110)
    ),

    ok.
