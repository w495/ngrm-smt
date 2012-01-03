-module(binary_test).
-export([
        test/1
    ]).

-define(LIST_ITEM_SEP, " ").
-define(BASE, 576460752303423488).

test(speed)->

    Big_num_list = [
        1, 2, 3, 4, 5, 6, 7, 8,
        9,
        99,
        100,
        999,
        9999,
        99999,
        999999,
        9999999,
        99999999,
        999999999,
        9999999999,
        99999999999,
        999999999999,
        9999999999999,
        99999999999999,
        999999999999999,
        9999999999999999,
        99999999999999999,
        999999999999999999,
        9999999999999999999,
        99999999999999999999,
        999999999999999999999,
        9999999999999999999999,
        99999999999999999999999
    ] ++ [?BASE div Val|| Val <- lists:seq(1, 10)],


    Big_str_list = [erlang:integer_to_list(Int) || Int <- Big_num_list ],

    %% erlang:list_to_binary(string:join > erlang:term_to_binary
    %% erlang:term_to_binary is better
    lists:map(fun(List)->
        io:format("~nList Str = ~p --------------------~n", [List]),
        tests:print_speed("erlang:list_to_binary",
            fun() ->
                erlang:list_to_binary(string:join(List, ?LIST_ITEM_SEP))
            end, 10000000, {mem, {total, 1024}}),
        tests:print_speed("erlang:term_to_binary",
            fun() ->
                erlang:term_to_binary(List)
            end, 10000000, {mem, {total, 1024}}),
        io:format("List Str  = ~p --------------------~n", [List]),
        ok
        end,
        [[Big_str_list]] ++
        [
            [erlang:integer_to_list(Int1), erlang:integer_to_list(Int2)]
            ||  Int1 <- Big_num_list, Int2 <- Big_num_list
        ]
    ),

    %% erlang:list_to_binary < erlang:term_to_binary
    %% erlang:list_to_binary is better
    lists:map(fun(Str)->
        io:format("~nStr = ~p --------------------~n", [Str]),
        tests:print_speed("erlang:list_to_binary",
            fun() ->
                Bin = erlang:list_to_binary(Str),
                Str = erlang:binary_to_list(Bin)
            end, 10000000, {mem, {total, 1024}}),
        tests:print_speed("erlang:term_to_binary",
            fun() ->
                Bin = erlang:term_to_binary(Str),
                Str = erlang:binary_to_term(Bin)
            end, 10000000, {mem, {total, 1024}}),
        io:format("Str = ~p --------------------~n", [Str]),
        ok
        end, Big_str_list ),

    %% erlang:list_to_binary(erlang:integer_to_list > erlang:term_to_binary
    %% erlang:term_to_binary is better
    lists:map(fun(Int)->
        io:format("~nInt = ~p --------------------~n", [Int]),
        tests:print_speed("erlang:list_to_binary(erlang:integer_to_list",
            fun() ->
                Bin = erlang:list_to_binary(erlang:integer_to_list(Int)),
                Int = erlang:list_to_integer(erlang:binary_to_list(Bin))
            end, 10000000, {mem, {total, 1024}}),
        tests:print_speed("erlang:term_to_binary",
            fun() ->
                Bin = erlang:term_to_binary(Int),
                Int = erlang:binary_to_term(Bin)
            end, 10000000, {mem, {total, 1024}}),
        io:format("Int = ~p --------------------~n", [Int]),
        ok
        end, Big_num_list ),
    ok.
