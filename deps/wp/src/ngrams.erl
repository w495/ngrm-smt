-module(ngrams).

-vsn(0.3).
-author('w-495').
-decription(
    "n-gramm extraction tool"
).

%-compile(export_all).

-export([
    ngram/2,
    ngram_from_string/2,
    ngram_list/2,
    ngram_list_e1/2,
    ngram_list_e1_tail/2,
    ngram_list_from_string/2,
    ngram_list_n/2,
    ngram_list_tail/2,
    ngram_lists/2,
    ngram_lists_from_string/2,
    ngram_lists_tail/2,
    ngram_proplist/2,
    ngram_proplist_from_string/2,
    ngram_proplist_tail/2,
    optimized_ngram/2,
    optimized_ngram_from_string/2,
    optimized_ngram_list/2,
    optimized_ngram_e1_list/2,
    optimized_ngram_e1_list/2,
    optimized_ngram_list_from_string/2,
    optimized_ngram_list_n/2,
    optimized_ngram_list_tail/2,
    optimized_ngram_lists/2,
    optimized_ngram_lists_from_string/2,
    optimized_ngram_lists_tail/2,
    optimized_ngram_proplist/2,
    optimized_ngram_proplist_from_string/2,
    optimized_ngram_proplist_tail/2,
    optimized_strict_ngram/2,
    strict_ngram/2,
    tailed_ngram/2,
    tailed_ngram/3,
    test/0,
    test/1
]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% N-ГРАММЫ
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%% ==========================================================================
%%%

ngram(List, Lenth) when is_list(List) ->
    strict_ngram(List, Lenth).

ngram_from_string(String, Lenth) when is_list(String) ->
    strict_ngram(words:list(String), Lenth).

%%% ==========================================================================
%%%

ngram_proplist_from_string(String, Lenth) ->
    ngram_proplist(words:list(String), Lenth).

ngram_lists_from_string(String, Lenth) ->
    ngram_lists(words:list(String), Lenth).


ngram_list_from_string(String, Lenth) ->
    ngram_list(words:list(String), Lenth).


%%% ==========================================================================
%%% Возвращает словарь списков n-грамм.
%%% Ключ --- длинна, значание --- список n-грамм соответсвующей длинны.
%%%
%%% ngrams:ngram_proplist([1, 2, 3], 1).
%%% [{1,[[1],[2],[3]]}]
%%% ngrams:ngram_proplist([1, 2, 3], 2).
%%% [{2,[[1,2],[2,3]]},{1,[[1],[2],[3]]}]
%%% ngrams:ngram_proplist([1, 2, 3], 3).
%%% [{3,[[1,2,3]]},{2,[[1,2],[2,3]]},{1,[[1],[2],[3]]}]
%%% ngrams:ngram_proplist([1, 2, 3], 4).
%%% [{4,[]},{3,[[1,2,3]]},{2,[[1,2],[2,3]]},{1,[[1],[2],[3]]}]
%%% ngrams:ngram_proplist([1, 2, 3], 5).
%%% [{5,[]},
%%%  {4,[]},
%%%  {3,[[1,2,3]]},
%%%  {2,[[1,2],[2,3]]},
%%%  {1,[[1],[2],[3]]}]
%%%

ngram_proplist(_List, 0) -> [];
ngram_proplist(List, Lenth) ->
    case ngram(List, Lenth) of
        [] ->
            [{Lenth, []} | ngram_proplist(List, Lenth-1)];
        L ->
            [{Lenth, L} |ngram_proplist_tail(L, Lenth)]
    end.

%%% -------------------------------------------------------------------------

ngram_proplist_tail(_InputList, 1) -> [];
ngram_proplist_tail([H | T], Lenth) ->
    Res = lists:append(strict_ngram(H, Lenth-1), [ Ntail || [ _ | Ntail] <- T]),
    [{Lenth-1, Res} | ngram_proplist_tail(Res, Lenth-1)].

%%% ==========================================================================
%%% Возвращает список списков n-грамм длинны меньше Lenth.
%%% В каждом списке находятся n-граммы заданной длинны.
%%%
%%% ngrams:ngram_lists([1, 2, 3], 1).
%%% [[[1],[2],[3]]]
%%% ngrams:ngram_lists([1, 2, 3], 2).
%%% [[[1,2],[2,3]],[[1],[2],[3]]]
%%% ngrams:ngram_lists([1, 2, 3], 3).
%%% [[[1,2,3]],[[1,2],[2,3]],[[1],[2],[3]]]
%%% ngrams:ngram_lists([1, 2, 3], 4).
%%% [[],[[1,2,3]],[[1,2],[2,3]],[[1],[2],[3]]]
%%% ngrams:ngram_lists([1, 2, 3], 5).
%%% [[],[],[[1,2,3]],[[1,2],[2,3]],[[1],[2],[3]]]
%%%

ngram_lists(_List, 0) -> [];
ngram_lists(List, Lenth) ->
    case ngram(List, Lenth) of
        [] ->
            [[]| ngram_lists(List, Lenth-1)];
        L ->
            [L | ngram_lists_tail(L, Lenth)]
    end.

%%% -------------------------------------------------------------------------

ngram_lists_tail( _InputList, 1) -> [];
ngram_lists_tail([H | T], Lenth) ->
    Res = lists:append(strict_ngram(H, Lenth-1), [ Ntail || [ _ | Ntail] <- T]),
    [Res | ngram_lists_tail(Res, Lenth-1)].

%%% ==========================================================================
%%% Возвращает список всех n-грамм длинны меньше Lenth.
%%%
%%% ngrams:ngram_list([1, 2, 3], 1).
%%% [[1],[2],[3]]
%%% ngrams:ngram_list([1, 2, 3], 2).
%%% [[1,2],[2,3],[1],[2],[3]]
%%% ngrams:ngram_list([1, 2, 3], 3).
%%% [[1,2,3],[1,2],[2,3],[1],[2],[3]]
%%% ngrams:ngram_list([1, 2, 3], 4).
%%% [[1,2,3],[1,2],[2,3],[1],[2],[3]]
%%% ngrams:ngram_list([1, 2, 3], 5).
%%% [[1,2,3],[1,2],[2,3],[1],[2],[3]]
%%%

%%% O(n)
ngram_list_n(_List, 0) -> [];
ngram_list_n(List, Lenth) ->
    case strict_ngram(List, Lenth) of
        [] ->
            ngram_list_n(List, Lenth-1);
        L ->
            lists:append(L, ngram_list_n(List, Lenth-1))
    end.

%%% -------------------------------------------------------------------------

%%% O(log(n))
ngram_list(_List, 0) -> [];
ngram_list(List, Lenth) ->
    case ngram(List, Lenth) of
        [] ->
            ngram_list(List, Lenth-1);
        L ->
            lists:append(L, ngram_list_tail(L, Lenth))
    end.

%%% -------------------------------------------------------------------------

ngram_list_tail( _InputList, 1) -> [];
ngram_list_tail([H | T], Lenth) ->
    Res = lists:append(strict_ngram(H, Lenth-1), [ Ntail || [ _ | Ntail] <- T]),
    lists:append(Res, ngram_list_tail(Res, Lenth-1)).


%%% ==========================================================================
%%% Возвращает список всех n-грамм длинны больше 1 и меньше Lenth.
%%%

ngram_list_e1(_List, 1) -> [];
ngram_list_e1(List, Lenth) ->
    case ngram(List, Lenth) of
        [] ->
            ngram_list_e1(List, Lenth-1);
        L ->
            lists:append(L, ngram_list_e1_tail(L, Lenth))
    end.

%%% -------------------------------------------------------------------------

ngram_list_e1_tail(_InputList, 2) -> [];
ngram_list_e1_tail([H | T], Lenth) ->
    Res = lists:append(strict_ngram(H, Lenth-1), [ Ntail || [ _ | Ntail] <- T]),
    lists:append(Res, ngram_list_e1_tail(Res, Lenth-1)).

%%% ==========================================================================
%%%
%%% ngrams:tailed_ngram([1, 2, 3, 4, 5, 6, 7], 1).
%%% [[1],[2],[3],[4],[5],[6],[7]]
%%%
%%% ngrams:tailed_ngram([1, 2, 3, 4, 5, 6, 7], 2).
%%% [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7],[7]]
%%%
%%% ngrams:tailed_ngram([1, 2, 3, 4, 5, 6, 7], 3).
%%% [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7],[6,7],[7]]
%%%
%%% ngrams:tailed_ngram([1, 2, 3, 4, 5, 6, 7], 4).
%%% [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7],[5,6,7],[6,7],[7]]
%%%
%%% ngrams:tailed_ngram([1, 2, 3, 4, 5, 6, 7], 5).
%%% [[1,2,3,4,5],
%%% [2,3,4,5,6],
%%% [3,4,5,6,7],
%%% [4,5,6,7],
%%% [5,6,7],
%%% [6,7],
%%% [7]]
%%%

tailed_ngram(List, Lenth) ->
    tailed_ngram(List, 1, Lenth).

tailed_ngram(List, S, Lenth) when length(List) > S ->
    [lists:sublist(List, S, Lenth) | tailed_ngram(List, S+1, Lenth) ];

tailed_ngram(List, S, Lenth) ->
    [lists:sublist(List, S, Lenth)].

%%% -------------------------------------------------------------------------

%%% ==========================================================================
%%%
%%% ngrams:strict_ngram([1, 2, 3, 4, 5, 6, 7], 0).
%%% [[],[],[],[],[],[],[]]
%%%
%%% ngrams:strict_ngram([1, 2, 3, 4, 5, 6, 7], 1).
%%% [[1],[2],[3],[4],[5],[6],[7]]
%%%
%%% ngrams:strict_ngram([1, 2, 3, 4, 5, 6, 7], 2).
%%% [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7]]
%%%
%%% ngrams:strict_ngram([1, 2, 3, 4, 5, 6, 7], 3).
%%% [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7]]
%%%
%%% ngrams:strict_ngram([1, 2, 3, 4, 5, 6, 7], 4).
%%% [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7]]
%%%
%%% ngrams:strict_ngram([1, 2, 3, 4, 5, 6, 7], 5).
%%% [[1,2,3,4,5],[2,3,4,5,6],[3,4,5,6,7]]
%%%
%%% ngrams:strict_ngram([1, 2, 3, 4, 5, 6, 7], 6).
%%% [[1,2,3,4,5,6],[2,3,4,5,6,7]]
%%%
%%% ngrams:strict_ngram([1, 2, 3, 4, 5, 6, 7], 7).
%%% [[1,2,3,4,5,6,7]]
%%%
%%% ngrams:strict_ngram([1, 2, 3, 4, 5, 6, 7], 8).
%%% []
%%%
%%% ngrams:strict_ngram([1, 2, 3, 4, 5, 6, 7], 9).
%%% []
%%%

strict_ngram([ _ | Tail] = List, Lenth) when length(List) >=  Lenth  ->
    {Ngram, _ } = lists:split(Lenth, List),
    [Ngram | strict_ngram(Tail, Lenth)];


strict_ngram(_,_) -> [].


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% ОПТИМИЗИРОВАННЫЕ N-ГРАММЫ
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%% -------------------------------------------------------------------------

optimized_ngram(List, Lenth) when is_list(List) ->
    optimized_strict_ngram(List, Lenth).

optimized_ngram_from_string(String, Lenth) when is_list(String) ->
    optimized_strict_ngram(words:list(String), Lenth).

%%% -------------------------------------------------------------------------

optimized_ngram_proplist_from_string(String, Lenth) ->
    optimized_ngram_proplist(words:list(String), Lenth).

optimized_ngram_lists_from_string(String, Lenth) ->
    optimized_ngram_lists(words:list(String), Lenth).

optimized_ngram_list_from_string(String, Lenth) ->
    optimized_ngram_list(words:list(String), Lenth).


%%% ==========================================================================

optimized_ngram_proplist(_List, 0) -> [];
optimized_ngram_proplist(List, Lenth) ->
    case optimized_strict_ngram(List, Lenth) of
        [] ->
            [{Lenth, []} | optimized_ngram_proplist(List, Lenth-1)];
        L ->
            [{Lenth, L} |optimized_ngram_proplist_tail(L, Lenth)]
    end.

%%% -------------------------------------------------------------------------

optimized_ngram_proplist_tail(_InputList, 1) -> [];
optimized_ngram_proplist_tail([H | T], Lenth) ->
    Res = lists:append(optimized_strict_ngram(H, Lenth-1),
        [ Ntail || [ _ | Ntail] <- T]),
    [{Lenth-1, Res} | optimized_ngram_proplist_tail(Res, Lenth-1)].


%%% ==========================================================================

optimized_ngram_lists(_List, 0) -> [];
optimized_ngram_lists(List, Lenth) ->
    case optimized_strict_ngram(List, Lenth) of
        [] ->
            [[]| optimized_ngram_lists(List, Lenth-1)];
        L ->
            [L | optimized_ngram_lists_tail(L, Lenth)]
    end.

%%% -------------------------------------------------------------------------

optimized_ngram_lists_tail( _InputList, 1) -> [];
optimized_ngram_lists_tail([H | T], Lenth) ->
    Res = lists:append(optimized_strict_ngram(H, Lenth-1),
        [ Ntail || [ _ | Ntail] <- T]),
    [Res | optimized_ngram_lists_tail(Res, Lenth-1)].

%%% ==========================================================================

%%% O(n)
optimized_ngram_list_n(_List, 0) -> [];
optimized_ngram_list_n(List, Lenth) ->
    case optimized_strict_ngram(List, Lenth) of
        [] ->
            optimized_ngram_list_n(List, Lenth-1);
        L ->
            lists:append(L, optimized_ngram_list_n(List, Lenth-1))
    end.

%%% -------------------------------------------------------------------------

%%% O(log(n))
optimized_ngram_list(_List, 0) -> [];
optimized_ngram_list(List, Lenth) ->
    case optimized_strict_ngram(List, Lenth) of
        [] ->
            optimized_ngram_list(List, Lenth-1);
        L ->
            lists:append(L, optimized_ngram_list_tail(L, Lenth))
    end.

%%% -------------------------------------------------------------------------

optimized_ngram_list_tail(_InputList, 1) -> [];
optimized_ngram_list_tail([Head | Tail], Lenth) ->
    Res = lists:append(optimized_strict_ngram(Head, Lenth-1),
        [ Ntail || [ _ | Ntail] <- Tail]),
    lists:append(Res, optimized_ngram_list_tail(Res, Lenth-1)).


%%% ==========================================================================

optimized_ngram_list_e1_tail(_List, 0) -> [];
optimized_ngram_list_e1_tail(List, Lenth) ->
    case optimized_strict_ngram(List, Lenth) of
        [] ->
            optimized_ngram_list_e1_tail(List, Lenth-1);
        L ->
            lists:append(L, optimized_ngram_list_e1_tail(L, Lenth))
    end.

%%% -------------------------------------------------------------------------

optimized_ngram_list_e1_tail(_InputList, 2) -> [];
optimized_ngram_list_e1_tail([Head | Tail], Lenth) ->
    Res = lists:append(optimized_strict_ngram(Head, Lenth-1),
        [ Ntail || [ _ | Ntail] <- Tail]),
    lists:append(Res, optimized_ngram_list_e1_tail(Res, Lenth-1)).

%%% ==========================================================================

optimized_strict_ngram([_ | R], 0) ->
    [[] | optimized_strict_ngram(R, 0)];
optimized_strict_ngram([H1 | R], 1) ->
    [[H1]
        | optimized_strict_ngram(R, 1)];
optimized_strict_ngram([H1, H2 | R], 2) ->
    [[H1, H2]
        | optimized_strict_ngram([H2 | R], 2)];
optimized_strict_ngram([H1, H2, H3 | R], 3) ->
    [[H1, H2, H3]
        | optimized_strict_ngram([H2, H3 | R], 3)];
optimized_strict_ngram([H1, H2, H3, H4 | R], 4) ->
    [[H1, H2, H3, H4]
        | optimized_strict_ngram([H2, H3, H4 | R], 4)];
optimized_strict_ngram([H1, H2, H3, H4, H5 | R], 5) ->
    [[H1, H2, H3, H4, H5]
        | optimized_strict_ngram([H2, H3, H4, H5 | R], 5)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6 | R], 6) ->
    [[H1, H2, H3, H4, H5, H6]
        | optimized_strict_ngram([H2, H3, H4, H5, H6 | R], 6)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6, H7 | R], 7) ->
    [[H1, H2, H3, H4, H5, H6, H7]
        | optimized_strict_ngram([H2, H3, H4, H5, H6, H7  | R], 7)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6, H7, H8 | R], 8) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8 ]
        | optimized_strict_ngram([H2, H3, H4, H5, H6, H7, H8  | R], 8)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6, H7, H8, H9 | R], 9) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9 ]
        | optimized_strict_ngram([ H2, H3, H4, H5, H6, H7, H8, H9  | R], 9)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10 | R], 10) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9, H10 ]
        | optimized_strict_ngram([H2, H3, H4, H5, H6, H7, H8, H9, H10 |R], 10)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11 | R], 11) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11 ]
        | optimized_strict_ngram([H2, H3, H4, H5, H6, H7, H8, H9, H10, H11 |R], 12)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12 | R], 12) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12 ]
        | optimized_strict_ngram([H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12 | R], 12)];
optimized_strict_ngram([H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13 | R], 13) ->
    [[H1, H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13 ]
        | optimized_strict_ngram([H2, H3, H4, H5, H6, H7, H8, H9, H10, H11, H12, H13 | R], 13)];

optimized_strict_ngram(_,_) -> [].


%%% -------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").

test()->

    %% TAILED_NGRAM
    %% --------------------------------------
    ?assertEqual(
        [[1],[2],[3],[4],[5],[6],[7]],
        tailed_ngram([1, 2, 3, 4, 5, 6, 7], 1)
    ),
    ?assertEqual(
        [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7],[7]],
        tailed_ngram([1, 2, 3, 4, 5, 6, 7], 2)
    ),
    ?assertEqual(
        [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7],[6,7],[7]],
        tailed_ngram([1, 2, 3, 4, 5, 6, 7], 3)
    ),
    ?assertEqual(
        [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7],[5,6,7],[6,7],[7]],
        tailed_ngram([1, 2, 3, 4, 5, 6, 7], 4)
    ),
    ?assertEqual(
        [[1,2,3,4,5], [2,3,4,5,6], [3,4,5,6,7], [4,5,6,7], [5,6,7], [6,7], [7]],
        tailed_ngram([1, 2, 3, 4, 5, 6, 7], 5)
    ),

    %% STRICT_NGRAM
    %% --------------------------------------
    ?assertEqual(
        [[],[],[],[],[],[],[]],
        strict_ngram([1, 2, 3, 4, 5, 6, 7], 0)
    ),
    ?assertEqual(
        [[1],[2],[3],[4],[5],[6],[7]],
        strict_ngram([1, 2, 3, 4, 5, 6, 7], 1)
    ),
    ?assertEqual(
        [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7]],
        strict_ngram([1, 2, 3, 4, 5, 6, 7], 2)
    ),
    ?assertEqual(
        [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7]],
        strict_ngram([1, 2, 3, 4, 5, 6, 7], 2)
    ),
    ?assertEqual(
        [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7]],
        strict_ngram([1, 2, 3, 4, 5, 6, 7], 3)
    ),
    ?assertEqual(
        [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7]],
        strict_ngram([1, 2, 3, 4, 5, 6, 7], 4)
    ),
    ?assertEqual(
        [[1,2,3,4,5],[2,3,4,5,6],[3,4,5,6,7]],
        strict_ngram([1, 2, 3, 4, 5, 6, 7], 5)
    ),
    ?assertEqual(
        [[1,2,3,4,5,6],[2,3,4,5,6,7]],
        strict_ngram([1, 2, 3, 4, 5, 6, 7], 6)
    ),
    ?assertEqual(
        [[1,2,3,4,5,6,7]],
        strict_ngram([1, 2, 3, 4, 5, 6, 7], 7)
    ),
    ?assertEqual([], strict_ngram([1, 2, 3, 4, 5, 6, 7], 8)),
    ?assertEqual([], strict_ngram([1, 2, 3, 4, 5, 6, 7], 9)),

    %% OPTIMIZED_STRICT_NGRAM
    %% --------------------------------------
    ?assertEqual(
        [[],[],[],[],[],[],[]],
        optimized_strict_ngram([1, 2, 3, 4, 5, 6, 7], 0)
    ),
    ?assertEqual(
        [[1],[2],[3],[4],[5],[6],[7]],
        optimized_strict_ngram([1, 2, 3, 4, 5, 6, 7], 1)
    ),
    ?assertEqual(
        [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7]],
        optimized_strict_ngram([1, 2, 3, 4, 5, 6, 7], 2)
    ),
    ?assertEqual(
        [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7]],
        optimized_strict_ngram([1, 2, 3, 4, 5, 6, 7], 2)
    ),
    ?assertEqual(
        [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7]],
        optimized_strict_ngram([1, 2, 3, 4, 5, 6, 7], 3)
    ),
    ?assertEqual(
        [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7]],
        optimized_strict_ngram([1, 2, 3, 4, 5, 6, 7], 4)
    ),
    ?assertEqual(
        [[1,2,3,4,5],[2,3,4,5,6],[3,4,5,6,7]],
        optimized_strict_ngram([1, 2, 3, 4, 5, 6, 7], 5)
    ),
    ?assertEqual(
        [[1,2,3,4,5,6],[2,3,4,5,6,7]],
        optimized_strict_ngram([1, 2, 3, 4, 5, 6, 7], 6)
    ),
    ?assertEqual(
        [[1,2,3,4,5,6,7]],
        optimized_strict_ngram([1, 2, 3, 4, 5, 6, 7], 7)
    ),
    ?assertEqual([], optimized_strict_ngram([1, 2, 3, 4, 5, 6, 7], 8)),
    ?assertEqual([], optimized_strict_ngram([1, 2, 3, 4, 5, 6, 7], 9)),

    %% NGRAM_LIST
    %% --------------------------------------
    ?assertEqual([[1],[2],[3]], ngram_list([1, 2, 3], 1)),
    ?assertEqual([[1,2],[2,3],[1],[2],[3]], ngram_list([1, 2, 3], 2)),
    ?assertEqual([[1,2,3],[1,2],[2,3],[1],[2],[3]], ngram_list([1, 2, 3], 3)),
    ?assertEqual([[1,2,3],[1,2],[2,3],[1],[2],[3]], ngram_list([1, 2, 3], 4)),
    ?assertEqual([[1,2,3],[1,2],[2,3],[1],[2],[3]], ngram_list([1, 2, 3], 5)),

    %% NGRAM_PROPLIST
    %% --------------------------------------
    ?assertEqual([{1,[[1],[2],[3]]}],
        ngram_proplist([1, 2, 3], 1)),
    ?assertEqual([{2,[[1,2],[2,3]]},{1,[[1],[2],[3]]}],
        ngram_proplist([1, 2, 3], 2)),
    ?assertEqual([{3,[[1,2,3]]},{2,[[1,2],[2,3]]},{1,[[1],[2],[3]]}],
        ngram_proplist([1, 2, 3], 3)),
    ?assertEqual([{4,[]},{3,[[1,2,3]]},{2,[[1,2],[2,3]]},{1,[[1],[2],[3]]}],
        ngram_proplist([1, 2, 3], 4)),
    ?assertEqual([{5,[]},{4,[]},
            {3,[[1,2,3]]},{2,[[1,2],[2,3]]},{1,[[1],[2],[3]]}],
        ngram_proplist([1, 2, 3], 5)),

    %% NGRAM_LISTS
    %% --------------------------------------
    ?assertEqual([[[1],[2],[3]]],
        ngram_lists([1, 2, 3], 1)),
    ?assertEqual([[[1,2],[2,3]],[[1],[2],[3]]],
        ngram_lists([1, 2, 3], 2)),
    ?assertEqual([[[1,2,3]],[[1,2],[2,3]],[[1],[2],[3]]],
        ngram_lists([1, 2, 3], 3)),
    ?assertEqual([[],[[1,2,3]],[[1,2],[2,3]],[[1],[2],[3]]],
        ngram_lists([1, 2, 3], 4)),
    ?assertEqual([[],[],[[1,2,3]],[[1,2],[2,3]],[[1],[2],[3]]],
        ngram_lists([1, 2, 3], 5)),

    %% OPTIMIZED_NGRAM_LIST
    %% --------------------------------------
    ?assertEqual([[1],[2],[3]],
        optimized_ngram_list([1, 2, 3], 1)),
    ?assertEqual([[1,2],[2,3],[1],[2],[3]],
        optimized_ngram_list([1, 2, 3], 2)),
    ?assertEqual([[1,2,3],[1,2],[2,3],[1],[2],[3]],
        optimized_ngram_list([1, 2, 3], 3)),
    ?assertEqual([[1,2,3],[1,2],[2,3],[1],[2],[3]],
        optimized_ngram_list([1, 2, 3], 4)),
    ?assertEqual([[1,2,3],[1,2],[2,3],[1],[2],[3]],
        optimized_ngram_list([1, 2, 3], 5)),

    %% OPTIMIZED_ngram_proplist
    %% --------------------------------------
    ?assertEqual([{1,[[1],[2],[3]]}],
        optimized_ngram_proplist([1, 2, 3], 1)),
    ?assertEqual([{2,[[1,2],[2,3]]},{1,[[1],[2],[3]]}],
        optimized_ngram_proplist([1, 2, 3], 2)),
    ?assertEqual([{3,[[1,2,3]]},{2,[[1,2],[2,3]]},{1,[[1],[2],[3]]}],
        optimized_ngram_proplist([1, 2, 3], 3)),
    ?assertEqual([{4,[]},{3,[[1,2,3]]},{2,[[1,2],[2,3]]},{1,[[1],[2],[3]]}],
        optimized_ngram_proplist([1, 2, 3], 4)),
    ?assertEqual([{5,[]},{4,[]},
            {3,[[1,2,3]]},{2,[[1,2],[2,3]]},{1,[[1],[2],[3]]}],
        optimized_ngram_proplist([1, 2, 3], 5)),

    %% OPTIMIZED_NGRAM_LISTS
    %% --------------------------------------
    ?assertEqual([[[1],[2],[3]]],
        optimized_ngram_lists([1, 2, 3], 1)),
    ?assertEqual([[[1,2],[2,3]],[[1],[2],[3]]],
        optimized_ngram_lists([1, 2, 3], 2)),
    ?assertEqual([[[1,2,3]],[[1,2],[2,3]],[[1],[2],[3]]],
        optimized_ngram_lists([1, 2, 3], 3)),
    ?assertEqual([[],[[1,2,3]],[[1,2],[2,3]],[[1],[2],[3]]],
        optimized_ngram_lists([1, 2, 3], 4)),
    ?assertEqual([[],[],[[1,2,3]],[[1,2],[2,3]],[[1],[2],[3]]],
        optimized_ngram_lists([1, 2, 3], 5)),

    ok.

-define(TEST_SEQ_LEN, 100).
-define(TEST_TIMES_1, 100000).
-define(TEST_TIMES_2, 1000000).

test(speed)->
    Seq = lists:seq(1, ?TEST_SEQ_LEN),
    ?assertEqual(ngram_list(lists:seq(1, ?TEST_SEQ_LEN ), 3 ),
        optimized_ngram_list(lists:seq(1, ?TEST_SEQ_LEN ), 3 )),
    lists:map(fun(X)->
        io:format("~nTimes = ~p~n", [X]),
        tests:print_speed("optimized_ngram_list_n",
            fun() ->
                optimized_ngram_list_n(Seq, X)
            end, ?TEST_TIMES_1),

        tests:print_speed("optimized_ngram_list",
            fun() ->
                optimized_ngram_list(Seq, X)
            end, ?TEST_TIMES_1),

        tests:print_speed("ngram_list",
            fun() ->
                ngram_list(Seq, X)
            end, ?TEST_TIMES_1),

        tests:print_speed("ngram_list_n",
            fun() ->
                ngram_list_n(Seq, X)
            end, ?TEST_TIMES_1),
        ok
        end,
        lists:seq(2, 10)
    ),
    ok.