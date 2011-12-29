%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% АЛЬТЕРНАТИВНАЯ, ДОПОЛНЕННАЯ РЕАЛИЗАЦИЯ ОПЕРАЦИЙ СО СПИСКАМИ
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(alists).

 -export([
             max/1,
             max/2,
             zip/2,
             lzip/2,
             rzip/2,
             times/2,
             times_diag/3,
             times_diago/3,
             times_diag_umerged/3,
             heads/1,
             test/0,
             test/1,
             test/2,
            list_test/1
         ]
 ).


max([H|T]) ->
    max(H, T).

max(M, []) ->
    M;

max(M, [H|L]) when M > H ->
    max(M, L);

max(_M, [H|L]) ->
    max(H,L).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% склейка (диагональ декартова произведение)
%%% zip:
%%%       a     b     c
%%% x   {a,x}   _     _
%%% y     _   {b,y}   _
%%% z     _     _    {c,z}
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

zip(List_1, List_2)->
    lzip(List_1, List_2).

% -----------------------------------------------------------------------

lzip([], _) -> [];
lzip(_, []) -> [];
lzip([Elem_1|Rest_1], [Elem_2|Rest_2]) ->
    [{Elem_1, Elem_2}|lzip(Rest_1, Rest_2)].

% -----------------------------------------------------------------------

rzip(List_1, List_2) ->
    rzip(List_1, List_2, []).
rzip(_, [], Result) ->   Result;
rzip([], _, Result) ->   Result;
rzip([Elem_1|Rest_1], [Elem_2|Rest_2], Result) ->
    rzip(Rest_1, Rest_2, [{Elem_1, Elem_2}|Result]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Декартово произвеление двух списков
%%% times([a, b, c], [x, y, z]):
%%%
%%%       x     y     z
%%% a   {a,x} {a,y} {a,z}
%%% b   {b,x} {b,y} {b,z}
%%% c   {c,x} {c,y} {c,z}
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

times(List_1, List_2)->
    [{X,Y} || X<-List_1, Y<-List_2].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% (диагонали декартова произведение)
%%% times_diag:
%%%
%%% ------------------------------- 2
%%%       x     y     z
%%% a   {a,x} {a,y}   _
%%% b   {b,x} {b,y} {b,z}
%%% c     _   {c,y} {c,z}
%%%
%%% ------------------------------- O = 2 w = 3 \ O = 1 w = 1
%%%       x     y     z     w
%%% a   {a,x} {a,y}   _     _
%%% b   {b,x} {b,y} {b,z}   _
%%% c     _   {c,y} {c,z} {c,w}
%%% d     _     _   {d,z} {d,w}
%%%
%%% ------------------------------- O = 3, w = 5
%%%       x     y     z     w
%%% a   {a,x} {a,y} {a,z}   _
%%% b   {b,x} {b,y} {b,z} {b,w}
%%% c   {c,x} {c,y} {c,z} {c,w}
%%% d     _   {d,y} {d,z} {d,w}
%%%
%%% -------------------------------
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% -------------------------------------------------------------------------
%%% Самый быстрый вариант
%%%     Но порядок не соблюден
%%%

times_diag([], _, _offset) -> [];
times_diag(_, [], _offset) -> [];
times_diag(_list_1, _list_2, 0) -> [];
times_diag([Elem_1|Rest_1] = List_1, [Elem_2|Rest_2] = List_2, Offset) ->
    lists:concat([
        [{Elem_1, Elem_2}],
        zip(Rest_1, Rest_2),
        times_diag_right(heads(List_1), Rest_2, Offset-1),
        times_diag_left(Rest_1, heads(List_2), Offset-1)
    ]).

times_diag_left([], _, _offset) -> [];
times_diag_left(_, [], _offset) -> [];
times_diag_left(_list_1, _list_2, 0) -> [];
times_diag_left([Elem_1|Rest_1], [Elem_2|Rest_2] = List_2, Offset) ->
    lists:concat([
        [{Elem_1, Elem_2}],
        zip(Rest_1, Rest_2),
        times_diag_left(Rest_1, heads(List_2), Offset-1)
    ]).

times_diag_right([], _, _offset) -> [];
times_diag_right(_, [], _offset) -> [];
times_diag_right(_list_1, _list_2, -0) -> [];
times_diag_right([Elem_1|Rest_1] = List_1, [Elem_2|Rest_2], Offset) ->
    lists:concat([
        [{Elem_1, Elem_2}],
        zip(Rest_1, Rest_2),
        times_diag_right(heads(List_1), Rest_2, Offset-1)
    ]).

%%% -------------------------------------------------------------------------
%%% Нормальный вариант, однако и самый медленный.
%%%     Сохраняем порядок.
%%%

times_diago([], _, _offset) -> [];
times_diago(_, [], _offset) -> [];
times_diago(_list_1, _list_2, 0) -> [];
times_diago(List_1, List_2, Offset) ->
    times_diagoc(List_1, List_2, Offset, 1).

times_diagoc([], _, _, _)->[];
times_diagoc([Elem_1|Rest_1], List_2, Offset, Counter) ->
    case (Offset > Counter) of
        true  ->
            Start = 1,
            Len = Counter + Offset - 1;
        false ->
            Start = Counter - Offset + 1,
            Len = (Offset*2) - 1 % Width
    end,
    lists:concat([
        [ {Elem_1, E_2} || E_2 <- lists:sublist(List_2, Start, Len)],
        times_diagoc(Rest_1, List_2, Offset, Counter + 1)
    ]).

%%% -------------------------------------------------------------------------
%%% Не самый эффективный вариант,
%%%     1) делаем лишние обороты рекурсии.
%%%     2) выполняем сортировку и удаление повторов umerge
%%%     3) теряем порядок
%%%
%%% Плохо себя проявляет на длинных списках.
%%%

times_diag_umerged([], _, _offset) -> [];
times_diag_umerged(_, [], _offset) -> [];
times_diag_umerged(_list_1, _list_2, 0) -> [];
times_diag_umerged([_elem_1|Rest_1] = List_1, [_elem_2|Rest_2] = List_2, Offset) ->
    lists:umerge([
        zip(List_1, List_2),
        times_diag_umerged(Rest_1, heads(List_2), Offset-1),
        times_diag_umerged(heads(List_1), Rest_2, Offset-1)
    ]).

%%% -------------------------------------------------------------------------
%%% Все эелементы списка кроме последнего
%%%

heads(List)->
    lists:delete(lists:last(List), List).


%%% -------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

test()->

    % LZIP \ ZIP
    % ----------------------------------
    ?assertEqual([{a, b}], lzip([a], [b])),
    ?assertEqual([{a, b}, {x, y}], lzip([a, x], [b, y])),
    ?assertEqual([{a, b}], lzip([a, x], [b])),
    ?assertEqual([{a, b}, {x, y}, {1, 2}], lzip([a, x, 1], [b, y, 2])),
    ?assertEqual([{a, b}, {x, y}, {1, 2}], lzip([a, x, 1, "a"], [b, y, 2])),


    % RZIP
    % ----------------------------------
    ?assertEqual([{a, b}], rzip([a], [b])),
    ?assertEqual([{x, y}, {a, b}], rzip([a, x], [b, y])),
    ?assertEqual([{a, b}], rzip([a, x], [b])),
    ?assertEqual([{1, 2}, {x, y}, {a, b}], rzip([a, x, 1, "a"], [b, y, 2])),


    % TIMES
    % ----------------------------------
    ?assertEqual([
                    {a,x},{a,y},{a,z},
                    {b,x},{b,y},{b,z},
                    {c,x},{c,y},{c,z}
                ],
        times([a, b, c], [x, y, z])),

    ?assertEqual([{a,x},{a,y},{b,x},{b,y},{c,x},{c,y}],
        times([a, b, c], [x, y])),

    % TIMES_DIAG порядок не учитывается
    % ----------------------------------
    ?assertEqual(lzip([a, b, c], [x, y, z]),
        times_diag([a, b, c], [x, y, z], 1)),
    ?assertEqual([
                     {a,x},{b,y},{c,z},
                        {a,y},{b,z},
                        {b,x},{c,y}
                ],
        times_diag([a, b, c], [x, y, z], 2)),

    ?assertEqual([
                     {a,x},{b,y},{c,z},
                        {a,y},{b,z},
                           {a,z},
                        {b,x},{c,y},
                           {c,x}
                ],
        times_diag([a, b, c], [x, y, z], 3)),
    ?assertEqual([
                     {a,x},{b,y},{c,z},
                        {a,y},{b,z},
                           {a,z},
                        {b,x},{c,y},
                           {c,x}
                ],
        times_diag([a, b, c], [x, y, z], 100)),
    ?assertEqual([
                    {a,x},{b,y},{c,z},{d,w},
                        {a,y},{b,z},{c,w},
                        {b,x},{c,y},{d,z}
        ],
        times_diag([a, b, c, d], [x, y, z, w], 2)),
    ?assertEqual([
                    {a,x},{b,y},{c,z},{d,w},
                        {a,y},{b,z},{c,w},
                            {a,z}, {b,w},
                        {b,x},{c,y},{d,z},
                            {c,x},{d,y}
        ],
        times_diag([a, b, c, d], [x, y, z, w], 3)),

    ?assertEqual([
                    {a,x},{b,y},{c,z},{d,w},
                        {a,y},{b,z},{c,w},
                            {a,z},{b,w},
                               {a,w},
                        {b,x},{c,y},{d,z},
                            {c,x},{d,y},
                               {d,x}
        ],
        times_diag([a, b, c, d], [x, y, z, w], 4)),

    ?assertEqual([
                    {a,x},{b,y},{c,z},{d,w},
                        {a,y},{b,z},{c,w},
                            {a,z},{b,w},
                               {a,w},
                        {b,x},{c,y},{d,z},
                            {c,x},{d,y},
                               {d,x}
        ],
        times_diag([a, b, c, d], [x, y, z, w], 100)),

    % TIMES_DIAGO порядок учитывается
    % ----------------------------------
    ?assertEqual(lzip([a, b, c], [x, y, z]),
        times_diago([a, b, c], [x, y, z], 1)),
    ?assertEqual([
                    {a,x},{a,y},
                    {b,x},{b,y},{b,z},
                          {c,y},{c,z}
                ],
        times_diago([a, b, c], [x, y, z], 2)),
    ?assertEqual([
                    {a,x},{a,y},
                    {b,x},{b,y},{b,z},
                          {c,y},{c,z},{c,w},
                                {d,z},{d,w}
                ],
        times_diago([a, b, c, d], [x, y, z, w], 2)),
    ?assertEqual([
                    {a,x},{a,y},{a,z},
                    {b,x},{b,y},{b,z},{b,w},
                    {c,x},{c,y},{c,z},{c,w},
                          {d,y},{d,z},{d,w}
                ],
        times_diago([a, b, c, d], [x, y, z, w], 3)),
    % 
    % times(L_1, L_2)
    %       = times_diago(L_1, L_2, Len(L_1))
    %       = times_diago(L_1, L_2, Len(L_2))
    % 
    ?assertEqual(times([a, b, c], [x, y, z]),
         times_diago([a, b, c], [x, y, z], 3)),
    ?assertEqual(times([a, b, c], [x, y, z]),
         times_diago([a, b, c], [x, y, z], 100)),
    ok.

test(speed) ->
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% 1000 раз 100 элементов
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    % ========================================================================
    io:format("~nTIMES_DIAG VS TIMES_DIAGO VS TIMES_DIAG_UMERGED (2)~n~n", []),

    tests:print_speed("times_diag([1..100],[1..100],2)",
        fun() -> times_diag(
            lists:seq(1, 100),
            lists:seq(1, 100), 2)
        end, 1000),
    tests:print_speed("times_diago([1..100],[1..100],2)",
        fun() -> times_diago(
            lists:seq(1, 100),
            lists:seq(1, 100), 2)
        end, 1000),
    tests:print_speed("times_diag_umerged([1..100],[1..100],2)",
        fun() -> times_diag_umerged(
            lists:seq(1, 100),
            lists:seq(1, 100), 2)
        end, 1000),

    % ========================================================================
    io:format("~nTIMES_DIAG VS TIMES_DIAGO (50)~n~n", []),

    tests:print_speed("times_diag([1..100],[1..100],50)",
        fun() -> times_diag(
            lists:seq(1, 100),
            lists:seq(1, 100), 50)
        end, 1000),
    tests:print_speed("times_diago([1..100],[1..100],50)",
        fun() -> times_diago(
            lists:seq(1, 100),
            lists:seq(1, 100), 50)
        end, 1000),

    %% --------------------------------------
    %% слишком медленно
    % tests:print_speed(fun() -> times_diag_umerged(
    %     lists:seq(1, 100),
    %     lists:seq(1, 100), 50)
    %         end, 10),
    %% --------------------------------------

    % ========================================================================
    io:format("~nTIMES_DIAG VS TIMES (>40)~n~n", []),

    tests:print_speed("times_diag([1..40],[1..40],101)",
        fun() -> times_diag(
            lists:seq(1, 40),
            lists:seq(1, 40), 101)
        end, 1000),
    tests:print_speed("times([1..40], [1..40])",
        fun() -> times(
            lists:seq(1, 40),
            lists:seq(1, 40))
        end, 1000),

    % ========================================================================
    io:format("~nTIMES_DIAG (>100) VS TIMES ~n~n", []),

    tests:print_speed("times_diag([1..100],[1..100],101)",
        fun() -> times_diag(
            lists:seq(1, 100),
            lists:seq(1, 100), 101)
        end, 1000),
    tests:print_speed("times([1..100], [1..100])",
        fun() -> times(
            lists:seq(1, 100),
            lists:seq(1, 100))
        end, 1000),

    % ========================================================================
    io:format("~nTIMES_DIAG (1) VS ZIP~n~n", []),

    tests:print_speed("times_diag([1..100],[1..100],1)",
        fun() -> times_diag(
                lists:seq(1, 100),
                lists:seq(1, 100),
            1)
        end, 1000),

    tests:print_speed("zip([1..100],[1..100])",
        fun() -> zip(
                lists:seq(1, 100),
                lists:seq(1, 100)
            )
        end, 1000),

    % ========================================================================
    io:format("~nTIMES VS TIMES_DIAG (50) VS ZIP~n~n", []),

    tests:print_speed("times([1..100], [1..100])",
        fun() -> times(
            lists:seq(1, 100),
            lists:seq(1, 100))
        end, 1000),
    tests:print_speed("times_diag([1..100],[1..100],50)",
        fun() -> times_diag(
            lists:seq(1, 100),
            lists:seq(1, 100), 50)
        end, 1000),
    tests:print_speed("zip([1..100],[1..100])",
        fun() -> zip(
                lists:seq(1, 100),
                lists:seq(1, 100)
            )
        end, 1000),
    % ========================================================================
    io:format("~nTIMES VS TIMES_DIAG (20) VS ZIP~n~n", []),

    tests:print_speed("times([1..100], [1..100])",
        fun() -> times(
            lists:seq(1, 100),
            lists:seq(1, 100))
        end, 1000),
    tests:print_speed("times_diag([1..100],[1..100],20)",
        fun() -> times_diag(
            lists:seq(1, 100),
            lists:seq(1, 100), 20)
        end, 1000),
    tests:print_speed("zip([1..100],[1..100])",
        fun() -> zip(
                lists:seq(1, 100),
                lists:seq(1, 100)
            )
        end, 1000),
    % ========================================================================
    io:format("~nTIMES VS TIMES_DIAG (10) VS ZIP~n~n", []),

    tests:print_speed("times([1..100], [1..100])",
        fun() -> times(
            lists:seq(1, 100),
            lists:seq(1, 100))
        end, 1000),
    tests:print_speed("times_diag([1..100],[1..100],10)",
        fun() -> times_diag(
            lists:seq(1, 100),
            lists:seq(1, 100), 10)
        end, 1000),
    tests:print_speed("zip([1..100],[1..100])",
        fun() -> zip(
                lists:seq(1, 100),
                lists:seq(1, 100)
            )
        end, 1000),
    % ========================================================================
    io:format("~nTIMES VS TIMES_DIAG (5) VS ZIP~n~n", []),

    tests:print_speed("times([1..100], [1..100])",
        fun() -> times(
            lists:seq(1, 100),
            lists:seq(1, 100))
        end, 1000),
    tests:print_speed("times_diag([1..100],[1..100],5)",
        fun() -> times_diag(
            lists:seq(1, 100),
            lists:seq(1, 100), 5)
        end, 1000),
    tests:print_speed("zip([1..100],[1..100])",
        fun() -> zip(
                lists:seq(1, 100),
                lists:seq(1, 100)
            )
        end, 1000),
    % ========================================================================
    io:format("~nTIMES VS TIMES_DIAG (3) VS ZIP~n~n", []),

    tests:print_speed("times([1..100], [1..100])",
        fun() -> times(
            lists:seq(1, 100),
            lists:seq(1, 100))
        end, 1000),
    tests:print_speed("times_diag([1..100],[1..100],5)",
        fun() -> times_diag(
            lists:seq(1, 100),
            lists:seq(1, 100), 3)
        end, 1000),
    tests:print_speed("zip([1..100],[1..100])",
        fun() -> zip(
                lists:seq(1, 100),
                lists:seq(1, 100)
            )
        end, 1000),
    ok.

test(Times, Elems) ->
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    %%% Elems0 раз Elems элементов
    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

   % ========================================================================
    io:format("~nTIMES_DIAG (1) VS ZIP~n~n", []),

    tests:print_speed("times_diag([1..Elems],[1..Elems],1)",
        fun() -> times_diag(
                lists:seq(1, Elems),
                lists:seq(1, Elems),
            1)
        end, Times),

    tests:print_speed("zip([1..Elems],[1..Elems])",
        fun() -> zip(
                lists:seq(1, Elems),
                lists:seq(1, Elems)
            )
        end, Times),

    % ========================================================================
    io:format("~nTIMES VS TIMES_DIAG (50) VS ZIP~n~n", []),

    tests:print_speed("times([1..Elems], [1..Elems])",
        fun() -> times(
            lists:seq(1, Elems),
            lists:seq(1, Elems))
        end, Times),
    tests:print_speed("times_diag([1..Elems],[1..Elems],50)",
        fun() -> times_diag(
            lists:seq(1, Elems),
            lists:seq(1, Elems), 50)
        end, Times),
    tests:print_speed("zip([1..Elems],[1..Elems])",
        fun() -> zip(
                lists:seq(1, Elems),
                lists:seq(1, Elems)
            )
        end, Times),
    % ========================================================================
    io:format("~nTIMES VS TIMES_DIAG (20) VS ZIP~n~n", []),

    tests:print_speed("times([1..Elems], [1..Elems])",
        fun() -> times(
            lists:seq(1, Elems),
            lists:seq(1, Elems))
        end, Times),
    tests:print_speed("times_diag([1..Elems],[1..Elems],20)",
        fun() -> times_diag(
            lists:seq(1, Elems),
            lists:seq(1, Elems), 20)
        end, Times),
    tests:print_speed("zip([1..Elems],[1..Elems])",
        fun() -> zip(
                lists:seq(1, Elems),
                lists:seq(1, Elems)
            )
        end, Times),
    % ========================================================================
    io:format("~nTIMES VS TIMES_DIAG (10) VS ZIP~n~n", []),

    tests:print_speed("times([1..Elems], [1..Elems])",
        fun() -> times(
            lists:seq(1, Elems),
            lists:seq(1, Elems))
        end, Times),
    tests:print_speed("times_diag([1..Elems],[1..Elems],10)",
        fun() -> times_diag(
            lists:seq(1, Elems),
            lists:seq(1, Elems), 10)
        end, Times),
    tests:print_speed("zip([1..Elems],[1..Elems])",
        fun() -> zip(
                lists:seq(1, Elems),
                lists:seq(1, Elems)
            )
        end, Times),
    % ========================================================================
    io:format("~nTIMES VS TIMES_DIAG (5) VS ZIP~n~n", []),

    tests:print_speed("times([1..Elems], [1..Elems])",
        fun() -> times(
            lists:seq(1, Elems),
            lists:seq(1, Elems))
        end, Times),
    tests:print_speed("times_diag([1..Elems],[1..Elems],5)",
        fun() -> times_diag(
            lists:seq(1, Elems),
            lists:seq(1, Elems), 5)
        end, Times),
    tests:print_speed("zip([1..Elems],[1..Elems])",
        fun() -> zip(
                lists:seq(1, Elems),
                lists:seq(1, Elems)
            )
        end, Times),
    % ========================================================================
    io:format("~nTIMES VS TIMES_DIAG (3) VS ZIP~n~n", []),

    tests:print_speed("times([1..Elems], [1..Elems])",
        fun() -> times(
            lists:seq(1, Elems),
            lists:seq(1, Elems))
        end, Times),
    tests:print_speed("times_diag([1..Elems],[1..Elems],5)",
        fun() -> times_diag(
            lists:seq(1, Elems),
            lists:seq(1, Elems), 3)
        end, Times),
    tests:print_speed("zip([1..Elems],[1..Elems])",
        fun() -> zip(
                lists:seq(1, Elems),
                lists:seq(1, Elems)
            )
        end, Times),

    ok.

list_test(speed)->

    L_100 = lists:seq(1, 100),
    L_1000 = lists:seq(1, 1000),

    io:format("~nL_100 ~n~n", []),

    tests:print_speed("lists:concat to head",
        fun() ->
            lists:concat([ [1], L_100])
        end, 100000),

    tests:print_speed("lists:concat",
        fun() ->
            lists:concat([ L_100, L_100])
        end, 100000),

    tests:print_speed("lists:concat to end",
        fun() ->
            lists:concat([ L_100, [1]])
        end, 100000),

    tests:print_speed("lists:append to head",
        fun() ->
            lists:append([ [1], L_100])
        end, 100000),

    tests:print_speed("lists:append",
        fun() ->
            lists:append([ L_100, L_100])
        end, 100000),

    tests:print_speed("lists:append to end",
        fun() ->
            lists:append([ L_100, [1]])
        end, 100000),

    tests:print_speed("string:join to head",
        fun() ->
            string:join([[1], L_100], [])
        end, 100000),

    tests:print_speed("string:join",
        fun() ->
            string:join([L_100, L_100], [])
        end, 100000),

    tests:print_speed("string:join",
        fun() ->
            string:join([L_100, [1]], [])
        end, 100000),

    io:format("~nL_1000 ~n~n", []),

    tests:print_speed("lists:concat to head",
        fun() ->
            lists:concat([ [1], L_1000])
        end, 100000),

    tests:print_speed("lists:concat",
        fun() ->
            lists:concat([ L_1000, L_1000])
        end, 100000),

    tests:print_speed("lists:concat to end",
        fun() ->
            lists:concat([ L_1000, [1]])
        end, 100000),

    tests:print_speed("lists:append to head",
        fun() ->
            lists:append([ [1], L_1000])
        end, 100000),

    tests:print_speed("lists:append",
        fun() ->
            lists:append([ L_1000, L_1000])
        end, 100000),

    tests:print_speed("lists:append to end",
        fun() ->
            lists:append([ L_1000, [1]])
        end, 100000),

    tests:print_speed("string:join to head",
        fun() ->
            string:join([[1], L_1000], [])
        end, 100000),

    tests:print_speed("string:join",
        fun() ->
            string:join([L_1000, L_1000], [])
        end, 100000),

    tests:print_speed("string:join",
        fun() ->
            string:join([L_1000, [1]], [])
        end, 100000),
    ok.