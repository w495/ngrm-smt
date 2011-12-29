-module(ngrams).
-compile(export_all).

-export([
        ngram/2,
        ngram_from_string/2,
        ngram_proplist/2,
        ngram_proplist_from_string/2,
        ngram_list/2,
        ngram_list_e1/2,
        ngram_list_from_string/2,
        ngram_lists/2,
        ngram_lists_from_string/2,
        strict_ngram/2,
        tailed_ngram/2,
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

%%% -------------------------------------------------------------------------


%%% ==========================================================================
%%%

ngram_proplist_from_string(String, Lenth) ->
    ngram_proplist(words:list(String), Lenth).

ngram_lists_from_string(String, Lenth) ->
    ngram_lists(words:list(String), Lenth).


ngram_list_from_string(String, Lenth) ->
    ngram_list(words:list(String), Lenth).

%%% -------------------------------------------------------------------------

%%% ==========================================================================
%%% Возвращает словарь списков n-грамм.
%%% Ключ --- длинна, значание --- список n-грамм соответсвующей длинны.
%%%
%%% >>> test:ngram_prop_list([1, 2, 3], 1).
%%% [{1,[[1],[2],[3]]}]
%%% >>> test:ngram_prop_list([1, 2, 3], 2).
%%% [{2,[[1,2],[2,3]]},{1,[[1],[2],[3]]}]
%%% >>> test:ngram_prop_list([1, 2, 3], 3).
%%% [{3,[[1,2,3]]},{2,[[1,2],[2,3]]},{1,[[1],[2],[3]]}]
%%% >>> test:ngram_prop_list([1, 2, 3], 4).
%%% [{4,[]},{3,[[1,2,3]]},{2,[[1,2],[2,3]]},{1,[[1],[2],[3]]}]
%%% >>> test:ngram_prop_list([1, 2, 3], 5).
%%% [{5,[]},
%%%  {4,[]},
%%%  {3,[[1,2,3]]},
%%%  {2,[[1,2],[2,3]]},
%%%  {1,[[1],[2],[3]]}]
%%%

ngram_proplist(_List, 0) -> [];
ngram_proplist(List, Lenth) when (Lenth >= 0) ->
    case ngram(List, Lenth) of
        [] ->
            [{Lenth, []} | ngram_proplist(List, Lenth-1)];
        L ->
            [{Lenth, L} |ngram_set__(?MODULE, decorator_proplist, L, Lenth)]
    end.

%%% -------------------------------------------------------------------------


%%% ==========================================================================
%%% Возвращает список списков n-грамм длинны меньше Lenth.
%%% В каждом списке находятся n-граммы заданной длинны.
%%%
%%% >>> test:ngram_lists([1, 2, 3], 1).
%%% [[[1],[2],[3]]]
%%% >>> test:ngram_lists([1, 2, 3], 2).
%%% [[[1,2],[2,3]],[[1],[2],[3]]]
%%% >>> test:ngram_lists([1, 2, 3], 3).
%%% [[[1,2,3]],[[1,2],[2,3]],[[1],[2],[3]]]
%%% >>> test:ngram_lists([1, 2, 3], 4).
%%% [[],[[1,2,3]],[[1,2],[2,3]],[[1],[2],[3]]]
%%% >>> test:ngram_lists([1, 2, 3], 5).
%%% [[],[],[[1,2,3]],[[1,2],[2,3]],[[1],[2],[3]]]
%%%

ngram_lists(_List, 0) -> [];
ngram_lists(List, Lenth) ->
    case ngram(List, Lenth) of
        [] ->
            [[]| ngram_lists(List, Lenth-1)];
        L ->
            [L | ngram_set__(?MODULE, decorator_lists, L, Lenth)]
    end.

%%% -------------------------------------------------------------------------

%%% ==========================================================================
%%% Возвращает список всех n-грамм длинны меньше Lenth.
%%%
%%% >>> test:ngram_list([1, 2, 3], 1).
%%% [[1],[2],[3]]
%%% >>> test:ngram_list([1, 2, 3], 2).
%%% [[1,2],[2,3],[1],[2],[3]]
%%% >>> test:ngram_list([1, 2, 3], 3).
%%% [[1,2,3],[1,2],[2,3],[1],[2],[3]]
%%% >>> test:ngram_list([1, 2, 3], 4).
%%% [[1,2,3],[1,2],[2,3],[1],[2],[3]]
%%% >>> test:ngram_list([1, 2, 3], 5).
%%% [[1,2,3],[1,2],[2,3],[1],[2],[3]]
%%%

ngram_list(_List, 0) -> [];
ngram_list(List, Lenth) when (Lenth >= 0) ->
    case ngram(List, Lenth) of
        [] ->
            ngram_list(List, Lenth-1);
        L ->
            lists:append(L, ngram_set__(?MODULE, decorator_list, L, Lenth))
    end.

%%% -------------------------------------------------------------------------


%%% ==========================================================================
%%% Возвращает список всех n-грамм длинны больше 1 и меньше Lenth.
%%%

ngram_list_e1(_List, 1) -> [];
ngram_list_e1(List, Lenth) when (Lenth > 1) ->
    case ngram(List, Lenth) of
        [] ->
            ngram_list_e1(List, Lenth-1);
        L ->
            lists:append(L, ngram_set__e1(?MODULE, decorator_list, L, Lenth))
    end.

%%% -------------------------------------------------------------------------

%%% ==========================================================================
%%%

decorator_proplist(Mod, Fn, {_, Res, Lenth}) ->
    [{Lenth, Res} | Mod:Fn(?MODULE, decorator_proplist, Res, Lenth)].

decorator_lists(Mod, Fn, {_, Res, Lenth}) ->
    [Res | Mod:Fn(?MODULE, decorator_lists, Res, Lenth)].

decorator_list(Mod, Fn, {_, Res, Lenth}) ->
    lists:append(Res, Mod:Fn(?MODULE, decorator_list, Res, Lenth)).

%%% -------------------------------------------------------------------------

%%% ==========================================================================
%%%

ngram_set__(_Mod, _FormFn, _InputList, 1) -> [];
ngram_set__(Mod, FormFn, [H | T] = InputList, Lenth) when (Lenth > 1) ->
    T2 = [lists:sublist(Ngram, 2, Lenth - 1) || Ngram <- T ],
    H2 = ngram(H, Lenth-1),
    Res = lists:append(H2, T2),
    Mod:FormFn(?MODULE, ngram_set__, {InputList, Res, Lenth-1}).


ngram_set__e1(_Mod, _FormFn, _InputList, 2) -> [];
ngram_set__e1(Mod, FormFn, [H | T] = InputList, Lenth) when (Lenth > 1) ->
    T2 = [lists:sublist(Ngram, 2, Lenth - 1) || Ngram <- T ],
    H2 = ngram(H, Lenth-1),
    Res = lists:append(H2, T2),
    Mod:FormFn(?MODULE, ngram_set__e1, {InputList, Res, Lenth-1}).

%%% -------------------------------------------------------------------------

%%% ==========================================================================
%%%
%%% >>> test:tailed_ngram([1, 2, 3, 4, 5, 6, 7], 1).
%%% [[1],[2],[3],[4],[5],[6],[7]]
%%%
%%% >>> test:tailed_ngram([1, 2, 3, 4, 5, 6, 7], 2).
%%% [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7],[7]]
%%%
%%% >>> test:tailed_ngram([1, 2, 3, 4, 5, 6, 7], 3).
%%% [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7],[6,7],[7]]
%%%
%%% >>> test:tailed_ngram([1, 2, 3, 4, 5, 6, 7], 4).
%%% [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7],[5,6,7],[6,7],[7]]
%%%
%%% >>> test:tailed_ngram([1, 2, 3, 4, 5, 6, 7], 5).
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
%%% >>> test:strict_ngram([1, 2, 3, 4, 5, 6, 7], 0).
%%% [[],[],[],[],[],[],[]]
%%%
%%% >>> test:strict_ngram([1, 2, 3, 4, 5, 6, 7], 1).
%%% [[1],[2],[3],[4],[5],[6],[7]]
%%%
%%% >>> test:strict_ngram([1, 2, 3, 4, 5, 6, 7], 2).
%%% [[1,2],[2,3],[3,4],[4,5],[5,6],[6,7]]
%%%
%%% >>> test:strict_ngram([1, 2, 3, 4, 5, 6, 7], 3).
%%% [[1,2,3],[2,3,4],[3,4,5],[4,5,6],[5,6,7]]
%%%
%%% >>> test:strict_ngram([1, 2, 3, 4, 5, 6, 7], 4).
%%% [[1,2,3,4],[2,3,4,5],[3,4,5,6],[4,5,6,7]]
%%%
%%% >>> test:strict_ngram([1, 2, 3, 4, 5, 6, 7], 5).
%%% [[1,2,3,4,5],[2,3,4,5,6],[3,4,5,6,7]]
%%%
%%% >>> test:strict_ngram([1, 2, 3, 4, 5, 6, 7], 6).
%%% [[1,2,3,4,5,6],[2,3,4,5,6,7]]
%%%
%%% >>> test:strict_ngram([1, 2, 3, 4, 5, 6, 7], 7).
%%% [[1,2,3,4,5,6,7]]
%%%
%%% >>> test:strict_ngram([1, 2, 3, 4, 5, 6, 7], 8).
%%% []
%%%
%%% >>> test:strict_ngram([1, 2, 3, 4, 5, 6, 7], 9).
%%% []
%%%

strict_ngram([ _ | Tail] = List, Lenth) when length(List) >=  Lenth  ->
    {Ngram, _ } = lists:split(Lenth, List),
    [Ngram | strict_ngram(Tail, Lenth)];


strict_ngram(_,_) -> [].

%strict_ngram([],_) -> [];
%strict_ngram(List, Lenth) when length(List) <  Lenth  -> [List].


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
    ok.


test(speed)->

    ok.