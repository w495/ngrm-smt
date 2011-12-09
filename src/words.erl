-module(words).
-export([
        list/1,
        list/3,
        real_list/1,
        test/0,
        test/1
    ]).

-include("../include/words.hrl").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% СЛОВА
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

list(String) ->
    list(String, ?WORDS_SEPARATOR_SRING, ?WORDS_PER_LINE_MAX_COUNT).

%%% -------------------------------------------------------------------------

real_list(String) ->
    list(String, ?WORDS_SEPARATOR_SRING).

%%% -------------------------------------------------------------------------

list(String, Sep) ->
    string:tokens(String, Sep).

%%% -------------------------------------------------------------------------

list(String, Sep, Limit) ->
    lists:sublist(string:tokens(String, Sep), Limit).

%%% -------------------------------------------------------------------------
%%% Опасный вариант,
%%%

unsave_sublist(List, Limit) ->
    {Res, _ } = lists:split(Limit, List),
    Res.

%%% -------------------------------------------------------------------------

sublist(_, 0) -> [];
sublist([], _) -> [];
sublist(List, Limit)  ->
    case catch(lists:split(Limit, List)) of
        {'EXIT', _ } ->
            List;
        {Res, _ } ->
            Res
    end.

%%% -------------------------------------------------------------------------

-include_lib("eunit/include/eunit.hrl").
test()->
    Seq_100 = lists:seq(1, 100),

    ?assertEqual(sublist(Seq_100, 0),  lists:sublist(Seq_100, 0) ),
    ?assertEqual(sublist(Seq_100, 1),  lists:sublist(Seq_100, 1) ),
    ?assertEqual(sublist(Seq_100, 40),  lists:sublist(Seq_100, 40) ),
    ?assertEqual(sublist(Seq_100, 100),  lists:sublist(Seq_100, 100) ),
    ?assertEqual(sublist(Seq_100, 101),  lists:sublist(Seq_100, 101) ),
    ?assertEqual(sublist(Seq_100, 1010),  lists:sublist(Seq_100, 1010) ),
    ok.

test(speed)->
    io:format("~nWORDS:SUBLIST VS LISTS:SUBLIST 100 ~n~n", []),
    Seq_100 = lists:seq(1, 100),

    io:format("~n1000 ~n~n", []),

    tests:print_speed("words:sublist 40",
        fun() ->
            sublist(Seq_100, 40)
        end, 1000),
    tests:print_speed("words:unsave_sublist 40",
        fun() ->
            unsave_sublist(Seq_100, 40)
        end, 1000),
    tests:print_speed("lists:sublist 40",
        fun() ->
            lists:sublist(Seq_100, 40)
        end, 1000),
    tests:print_speed("words:sublist 401",
        fun() ->
            sublist(Seq_100, 401)
        end, 1000),
    tests:print_speed("lists:sublist 401",
        fun() ->
            lists:sublist(Seq_100, 401)
        end, 1000),

    io:format("~n10000 ~n~n", []),

    tests:print_speed("words:sublist 40",
        fun() ->
            sublist(Seq_100, 40)
        end, 10000),
    tests:print_speed("words:unsave_sublist 40",
        fun() ->
            unsave_sublist(Seq_100, 40)
        end, 10000),
    tests:print_speed("lists:sublist 40",
        fun() ->
            lists:sublist(Seq_100, 40)
        end, 10000),

    tests:print_speed("words:sublist 401",
        fun() ->
            sublist(Seq_100, 401)
        end, 10000),
    tests:print_speed("lists:sublist 401",
        fun() ->
            lists:sublist(Seq_100, 401)
        end, 10000),

    io:format("~n100000 ~n~n", []),

    tests:print_speed("words:sublist 40",
        fun() ->
            sublist(Seq_100, 40)
        end, 100000),
    tests:print_speed("words:unsave_sublist 40",
        fun() ->
            unsave_sublist(Seq_100, 40)
        end, 100000),
    tests:print_speed("lists:sublist 40",
        fun() ->
            lists:sublist(Seq_100, 40)
        end, 100000),

    tests:print_speed("words:sublist 401",
        fun() ->
            sublist(Seq_100, 401)
        end, 100000),
    tests:print_speed("lists:sublist 401",
        fun() ->
            lists:sublist(Seq_100, 401)
        end, 100000),

    io:format("~nWORDS:SUBLIST VS LISTS:SUBLIST 1000 ~n~n", []),

    Seq_1000 = lists:seq(1, 1000),

    io:format("~n1000 ~n~n", []),

    tests:print_speed("words:sublist 40",
        fun() ->
            sublist(Seq_1000, 40)
        end, 1000),
    tests:print_speed("words:unsave_sublist 40",
        fun() ->
            unsave_sublist(Seq_1000, 40)
        end, 1000),
    tests:print_speed("lists:sublist 40",
        fun() ->
            lists:sublist(Seq_1000, 40)
        end, 1000),

    tests:print_speed("words:sublist 400",
        fun() ->
            sublist(Seq_1000, 400)
        end, 1000),
    tests:print_speed("words:unsave_sublist 400",
        fun() ->
            unsave_sublist(Seq_1000, 400)
        end, 1000),
    tests:print_speed("lists:sublist 400",
        fun() ->
            lists:sublist(Seq_1000, 400)
        end, 1000),

    tests:print_speed("words:sublist 4001",
        fun() ->
            sublist(Seq_1000, 4001)
        end, 1000),
    tests:print_speed("lists:sublist 4001",
        fun() ->
            lists:sublist(Seq_1000, 4001)
        end, 1000),

    io:format("~n10000 ~n~n", []),

    tests:print_speed("words:sublist 40",
        fun() ->
            sublist(Seq_1000, 40)
        end, 10000),
    tests:print_speed("words:unsave_sublist 40",
        fun() ->
            unsave_sublist(Seq_1000, 40)
        end, 10000),
    tests:print_speed("lists:sublist 40",
        fun() ->
            lists:sublist(Seq_1000, 40)
        end, 10000),

    tests:print_speed("words:sublist 400",
        fun() ->
            sublist(Seq_1000, 400)
        end, 10000),
    tests:print_speed("words:unsave_sublist 400",
        fun() ->
            unsave_sublist(Seq_1000, 400)
        end, 10000),
    tests:print_speed("lists:sublist 400",
        fun() ->
            lists:sublist(Seq_1000, 400)
        end, 10000),

    tests:print_speed("words:sublist 4001",
        fun() ->
            sublist(Seq_1000, 4001)
        end, 10000),
    tests:print_speed("lists:sublist 4001",
        fun() ->
            lists:sublist(Seq_1000, 4001)
        end, 10000),



    ok.

