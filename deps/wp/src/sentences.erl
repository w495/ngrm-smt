%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% ПРЕДЛОЖЕНИЯ
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(sentences).
-export([
            zip_words/2,
            times_words/2,
            tuple_sentences/3,
            zip_sentences/3,
            times_sentences/3,
            times_diag_sentences/4,
            times_diago_sentences/4,
            comb_sentences/4,
            test/0,
            test/1
        ]
).

-define(double(Module, Function, Ngram_List_1, Ngram_List_2),
    lists:append([
        Module:Function(Ngram_List_1, Ngram_List_2),
        Module:Function(Ngram_List_2, Ngram_List_1)
    ])).

-define(double_offset(Module, Function, Ngram_List_1, Ngram_List_2, Offset),
    lists:append([
        Module:Function(Ngram_List_1, Ngram_List_2, Offset),
        Module:Function(Ngram_List_2, Ngram_List_1, Offset)
    ])).

%%% ---------------------------------------------------------------------------

zip_words(Snt_1, Snt_2)->
    List_1 = words:list(Snt_1),
    List_2 = words:list(Snt_2),
    ?double(alists, zip, List_1 , List_2).

%%% ---------------------------------------------------------------------------

times_words(Snt_1, Snt_2)->
    List_1 = words:list(Snt_1),
    List_2 = words:list(Snt_2),
    ?double(alists, times, List_1 , List_2).

tuple_sentences(Snt_1, Snt_2, Size)->
    Ngram_List_1 = ngrams:optimized_ngram_list_from_string(Snt_1, Size),
    Ngram_List_2 = ngrams:optimized_ngram_list_from_string(Snt_2, Size),
    {lists:append([Ngram_List_1, Ngram_List_2]),lists:append([Ngram_List_2, Ngram_List_1])}.


%%% ---------------------------------------------------------------------------

zip_sentences(Snt_1, Snt_2, Size)->
    Ngram_List_1 = ngrams:optimized_ngram_list_from_string(Snt_1, Size),
    Ngram_List_2 = ngrams:optimized_ngram_list_from_string(Snt_2, Size),
    ?double(alists, zip, Ngram_List_1, Ngram_List_2).

%%% ---------------------------------------------------------------------------

times_sentences(Snt_1, Snt_2, Size)->
    Ngram_List_1 = ngrams:optimized_ngram_list_from_string(Snt_1, Size),
    Ngram_List_2 = ngrams:optimized_ngram_list_from_string(Snt_2, Size),
    ?double(alists, times, Ngram_List_1, Ngram_List_2).

%%% ---------------------------------------------------------------------------

times_diag_sentences(Snt_1, Snt_2, Size, Offset) ->
    Ngram_List_1 = ngrams:optimized_ngram_list_from_string(Snt_1, Size),
    Ngram_List_2 = ngrams:optimized_ngram_list_from_string(Snt_2, Size),
    ?double_offset(alists, times_diag, Ngram_List_1, Ngram_List_2, Offset).

times_diago_sentences(Snt_1, Snt_2, Size, Offset) ->
    Ngram_List_1 = ngrams:optimized_ngram_list_from_string(Snt_1, Size),
    Ngram_List_2 = ngrams:optimized_ngram_list_from_string(Snt_2, Size),
    ?double_offset(alists, times_diago, Ngram_List_1, Ngram_List_2, Offset).

%%% ---------------------------------------------------------------------------
%%%
%%% Декартово произведение по словам,
%%%     Диагональ по n-граммам
%%%

comb_sentences(Snt_1, Snt_2, Size, Offset)->
    List_1 = words:list(Snt_1),
    List_2 = words:list(Snt_2),
    Ngram_List_1 = ngrams:optimized_ngram_list(List_1, Size),
    Ngram_List_2 = ngrams:optimized_ngram_list(List_2, Size),
    lists:append([
        ?double(alists, times, List_1 , List_2),
        ?double_offset(alists, times_diag, Ngram_List_1, Ngram_List_2, Offset)
    ]).



%%% ===========================================================================

-include_lib("eunit/include/eunit.hrl").

test()->

    % ZIP_SENTENCES
    % ----------------------------------

    lists:merge([
        zip_sentences("Black Cat",     "Chernyi Kot",  5),
        zip_sentences("Black House",   "Chernyi Dom",  5),
        zip_sentences("White House",   "Belyiy Dom",   5),
        zip_sentences("White House1",  "Belyiy Dom",   5) % ошибка перевода
    ]),

    En_1  = "adopted at the 81st plenary meeting",

    Ru_1  = "принята на 81-м пленарном заседании",

    En_2 = "adopted at the 81st plenary meeting, on 4 december 2000, "
            "on the recommendation of the committee "
            "(a/55/602/add.2 and corr.1, para. 94),the draft resolution "
            "recommended in the report was sponsored in the committee by: "
            "bolivia, cuba, el salvador, ghana and honduras. "
            "by a recorded vote of 106 to 1, with 67 abstentions, as follows:",
    Ru_2 = "принята на 81-м пленарном заседании 4 декабря 2000 года "
            "регистрируемым голосованием 106 голосами против 1 при 67 "
            "воздержавшихся по рекомендации комитета (a/55/602/add.2, пункт 94) "
            "проект резолюции, рекомендованный в докладе, на рассмотрение "
            "в комитете внесли: боливия, гана, гондурас, куба и сальвадор.; "
            "голоса распределились следующим образом:",

    En_3 = "{{continuous simulation}} can often be satisfactorily "
            "{{approximated}} by discrete simulation with very small time "
            "intervals between steps; however, in such a case we usually "
            "have synchronous discrete simulation, in which many parts of "
            "the system are slightly altered at each discrete time interval, "
            "and such an application generally calls for a somewhat different "
            "type of program organization than the kind considered here.",

    Ru_3 = "{{непрерывное моделирование}} часто можно вполне удовлетворительно "
            "{{имитировать}} с помощью дискретного моделирования "
            "с очень малыми временными интервалами между соседними шагами."
            "но в таком случае получится синхронное дискретное моделирование, "
            "при котором многие части системы слегка изменяются "
            "на каждом дискретном временном  интервале, и такое приложение "
            "обычно нуждается в организации программы несколько иного типа, "
            "чем тот, который рассмотрен здесь.",

    % Тут возникла проблема разной длинны предложений.
    % Потому мы реализовали свою alists:zip.

    ?assertEqual(true, erlang:is_list(zip_sentences(En_1,Ru_1, 5))),
    ?assertEqual(true, erlang:is_list(zip_sentences(En_2,Ru_2, 5))),
    ?assertEqual(true, erlang:is_list(zip_sentences(En_3,Ru_3, 5))),

    % TIMES_SENTENCES
    % ----------------------------------
    ?assertEqual(
        [{["1","2"],["a","b"]},
        {["1","2"],["a"]},
        {["1","2"],["b"]},
        {["1"],["a","b"]},
        {["1"],["a"]},
        {["1"],["b"]},
        {["2"],["a","b"]},
        {["2"],["a"]},
        {["2"],["b"]},
        {["a","b"],["1","2"]},
        {["a","b"],["1"]},
        {["a","b"],["2"]},
        {["a"],["1","2"]},
        {["a"],["1"]},
        {["a"],["2"]},
        {["b"],["1","2"]},
        {["b"],["1"]},
        {["b"],["2"]}],
        sentences:times_sentences("1 2", "a b", 2)),

    % ZIP_SENTENCES vs TIMES_DIAG_SENTENCES vs TIMES_SENTENCES
    % ----------------------------------
    ?assertEqual(6, erlang:length(words:list(En_1))),
    ?assertEqual(5, erlang:length(words:list(Ru_1))),

    % !!! 30
    ?assertEqual(30, erlang:length(zip_sentences(En_1, Ru_1, 5))),
    ?assertEqual(86, erlang:length(times_diag_sentences(En_1, Ru_1, 5, 2))),
    ?assertEqual(138, erlang:length(times_diag_sentences(En_1, Ru_1, 5, 3))),
    ?assertEqual(186, erlang:length(times_diag_sentences(En_1, Ru_1, 5, 4))),
    % !!! 600
    ?assertEqual(600, erlang:length(sentences:times_sentences(En_1, Ru_1, 5))),

    % без учета обезки обрезки
    ?assertEqual(53, erlang:length(words:real_list(En_2))),
    ?assertEqual(44, erlang:length(words:real_list(Ru_2))),

    ?assertEqual(62, erlang:length(words:real_list(En_3))),
    ?assertEqual(54, erlang:length(words:real_list(Ru_3))),

    % c учетом обрезки
    ?assertEqual(40, erlang:length(words:list(En_2))),
    ?assertEqual(40, erlang:length(words:list(Ru_2))),

    ?assertEqual(40, erlang:length(words:list(En_3))),
    ?assertEqual(40, erlang:length(words:list(Ru_3))),

    ?assertEqual(380, erlang:length(zip_sentences(En_2, Ru_2, 5))),
    ?assertEqual(380, erlang:length(zip_sentences(En_3, Ru_3, 5))),

    ?assertEqual(1136, erlang:length(times_diag_sentences(En_2, Ru_2, 5, 2))),
    ?assertEqual(1136, erlang:length(times_diag_sentences(En_3, Ru_3, 5, 2))),

    ?assertEqual(3380, erlang:length(times_diag_sentences(En_2, Ru_2, 5, 5))),
    ?assertEqual(3380, erlang:length(times_diag_sentences(En_3, Ru_3, 5, 5))),

    ?assertEqual(7040, erlang:length(times_diag_sentences(En_2, Ru_2, 5, 10))),
    ?assertEqual(7040, erlang:length(times_diag_sentences(En_3, Ru_3, 5, 5))),

    ?assertEqual(14060, erlang:length(times_diag_sentences(En_2, Ru_2, 5, 20))),
    ?assertEqual(14060, erlang:length(times_diag_sentences(En_3, Ru_3, 5, 20))),

    ?assertEqual(32720, erlang:length(times_diag_sentences(En_2, Ru_2, 5, 50))),
    ?assertEqual(32720, erlang:length(times_diag_sentences(En_3, Ru_3, 5, 50))),

    ?assertEqual(55820, erlang:length(times_diag_sentences(En_2, Ru_2, 5, 100))),
    ?assertEqual(55820, erlang:length(times_diag_sentences(En_3, Ru_3, 5, 100)))

    ?assertEqual(68920, erlang:length(times_diag_sentences(En_2, Ru_2, 5, 150))),
    ?assertEqual(68920, erlang:length(times_diag_sentences(En_3, Ru_3, 5, 150)))

    ?assertEqual(72200, erlang:length(times_diag_sentences(En_2, Ru_2, 5, 190))),
    ?assertEqual(72200, erlang:length(times_diag_sentences(En_3, Ru_3, 5, 190))),

    ?assertEqual(72200, erlang:length(times_diag_sentences(En_2, Ru_2, 5, 3000000))),
    ?assertEqual(72200, erlang:length(times_diag_sentences(En_3, Ru_3, 5, 191))),

    ?assertEqual(72200, erlang:length(times_sentences(En_2, Ru_2, 5))),
    ?assertEqual(72200, erlang:length(times_sentences(En_3, Ru_3, 5))),

    ok.


test(speed)->

    io:format("~nwords:real_list VS LISTS:SUBLIST ~n~n", []),

    En_1  = "adopted at the 81st plenary meeting",
    Ru_1  = "принята на 81-м пленарном заседании",

    tests:print_speed("words:real_list(En_1)",
        fun() ->
            words:real_list(En_1)
        end, 1000),
    tests:print_speed("words:list(En_1)",
        fun() ->
            words:list(En_1)
        end, 1000),
    tests:print_speed("words:real_list(Ru_1)",
        fun() ->
            words:real_list(Ru_1)
        end, 1000),
    tests:print_speed("words:list(Ru_2)",
        fun() ->
            words:list(Ru_1)
        end, 1000),


    En_2 = "adopted at the 81st plenary meeting, on 4 december 2000, "
            "on the recommendation of the committee "
            "(a/55/602/add.2 and corr.1, para. 94),the draft resolution "
            "recommended in the report was sponsored in the committee by: "
            "bolivia, cuba, el salvador, ghana and honduras. "
            "by a recorded vote of 106 to 1, with 67 abstentions, as follows:",
    Ru_2 = "принята на 81-м пленарном заседании 4 декабря 2000 года "
            "регистрируемым голосованием 106 голосами против 1 при 67 "
            "воздержавшихся по рекомендации комитета (a/55/602/add.2, пункт 94) "
            "проект резолюции, рекомендованный в докладе, на рассмотрение "
            "в комитете внесли: боливия, гана, гондурас, куба и сальвадор.; "
            "голоса распределились следующим образом:",


    tests:print_speed("words:real_list(En_2)",
        fun() -> 
            words:real_list(En_2)
        end, 1000),
    tests:print_speed("words:list(En_2)",
        fun() -> 
            words:list(En_2)
        end, 1000),
    tests:print_speed("words:real_list(Ru_2)",
        fun() -> 
            words:real_list(Ru_2)
        end, 1000),
    tests:print_speed("words:list(Ru_2)",
        fun() -> 
            words:list(Ru_2)
        end, 1000),


    En_3 = "{{continuous simulation}} can often be satisfactorily "
            "{{approximated}} by discrete simulation with very small time "
            "intervals between steps; however, in such a case we usually "
            "have synchronous discrete simulation, in which many parts of "
            "the system are slightly altered at each discrete time interval, "
            "and such an application generally calls for a somewhat different "
            "type of program organization than the kind considered here.",

    Ru_3 = "{{непрерывное моделирование}} часто можно вполне удовлетворительно "
            "{{имитировать}} с помощью дискретного моделирования "
            "с очень малыми временными интервалами между соседними шагами."
            "но в таком случае получится синхронное дискретное моделирование, "
            "при котором многие части системы слегка изменяются "
            "на каждом дискретном временном  интервале, и такое приложение "
            "обычно нуждается в организации программы несколько иного типа, "
            "чем тот, который рассмотрен здесь.",

    tests:print_speed("words:real_list(En_3)",
        fun() ->
            words:real_list(En_3)
        end, 1000),
    tests:print_speed("words:list(En_3)",
        fun() ->
            words:list(En_3)
        end, 1000),
    tests:print_speed("words:real_list(Ru_3)",
        fun() ->
            words:real_list(Ru_3)
        end, 1000),
    tests:print_speed("words:list(Ru_3)",
        fun() ->
            words:list(Ru_3)
        end, 1000),

    io:format("~nZIP_SENTENCES VS TIMES_DIAG_SENTENCES VS TIMES_SENTENCES UUN ~n~n", []),


    tests:print_speed("zip_sentences",
        fun() ->
            zip_sentences(En_2, Ru_2, 5)
        end, 1000),
    tests:print_speed("times_sentences 10",
        fun() ->
            times_diag_sentences(En_2, Ru_2, 5, 10)
        end, 1000),
    tests:print_speed("times_sentences 20",
        fun() ->
            times_diag_sentences(En_2, Ru_2, 5, 20)
        end, 1000),
    tests:print_speed("times_sentences 50",
        fun() ->
            times_diag_sentences(En_2, Ru_2, 5, 50)
        end, 1000),
    tests:print_speed("times_sentences 100",
        fun() ->
            times_diag_sentences(En_2, Ru_2, 5, 100)
        end, 1000),
    tests:print_speed("times_sentences 190 = times_sentences",
        fun() ->
            times_diag_sentences(En_2, Ru_2, 5, 190)
        end, 1000),
    tests:print_speed("times_sentences",
        fun() ->
            times_sentences(En_2, Ru_2, 5)
        end, 1000),

    io:format("~nZIP_SENTENCES VS TIMES_DIAG_SENTENCES VS TIMES_SENTENCES D. Knuth ~n~n", []),


    tests:print_speed("zip_sentences",
        fun() ->
            zip_sentences(En_3, Ru_3, 5)
        end, 1000),
    tests:print_speed("times_sentences 10",
        fun() ->
            times_diag_sentences(En_3, Ru_3, 5, 10)
        end, 1000),
    tests:print_speed("times_sentences 20",
        fun() ->
            times_diag_sentences(En_3, Ru_3, 5, 20)
        end, 1000),
    tests:print_speed("times_sentences 50",
        fun() ->
            times_diag_sentences(En_3, Ru_3, 5, 50)
        end, 1000),
    tests:print_speed("times_sentences 100",
        fun() ->
            times_diag_sentences(En_3, Ru_3, 5, 100)
        end, 1000),
    tests:print_speed("times_sentences 190 = times_sentences",
        fun() ->
            times_diag_sentences(En_3, Ru_3, 5, 190)
        end, 1000),
    tests:print_speed("times_sentences",
        fun() ->
            times_sentences(En_3, Ru_3, 5)
        end, 1000),

    ok.