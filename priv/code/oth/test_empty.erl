-module(test_empty).
-compile(export_all).

-include("../include/common.hrl").
-include("../include/words.hrl").


translate(String, Ngram_size)->
    Ngram_list = ngrams:ngram_list_from_string(String, Ngram_size).


word_counts_empty(Rredis_client, {Filename_1, Filename_2}) ->
    init_tables(Rredis_client, {Filename_1, Filename_2}),
    em_ibm_1(Rredis_client, {50}).


% ==========================================================================

init_tables(Rredis_client, {Filename_1, Filename_2}) ->
    case
        {
            file:open(Filename_1, read),
            file:open(Filename_2, read)
        }
        of
            {
                {ok, Io_device_1},
                {ok, Io_device_2}
            } ->
                process_each_line(Rredis_client, {db_redis, init_tables, []}, {{Io_device_1, Io_device_2}, {0}});
        {{error, Reason}, _ } ->
            io:format("file 1 (~s) error:~n|~s|~n", [Filename_1, Reason]);
        {_, {error, Reason}} ->
            io:format("file 2 (~s) error:~n|~s|~n", [Filename_2, Reason])
    end,
    init_prob_table(Rredis_client).

% ==========================================================================

em_ibm_1(Rredis_client, {Steps}) ->
    em_ibm_1_step(Rredis_client, {Steps, 0}).

em_ibm_1_step(Rredis_client, {Steps_Num, Step}) ->
    case Step of
        Steps_Num -> ok;
        _ ->
            em_init(Rredis_client, {0}),
            em_m(Rredis_client, {0}),
            em_e(Rredis_client, {0}),

            em_ibm_1_step(Rredis_client, {Steps_Num, Step + 1}),
            ok
    end.

em_init(Rredis_client, {Counter}) ->
    case db_redis:sentence_init(Rredis_client, {Counter}) of
        {true, _Pairs} ->
            em_init(Rredis_client, {Counter + 1});
        _ -> ok
    end.

em_m(Rredis_client, {Counter}) ->
    case db_redis:sentence_m(Rredis_client, {Counter}) of
        {true, _Pairs} ->
            em_m(Rredis_client, {Counter + 1});
        _ -> ok
    end.

em_e(Rredis_client, {Counter}) ->
    case db_redis:sentence_e(Rredis_client, {Counter}) of
        {true, _Pairs} ->
            em_e(Rredis_client, {Counter + 1});
        _ -> ok
    end.

% ==========================================================================

process_each_line(Rredis_client, {Module, Function, Argumens}, {{Io_device_1, Io_device_2}, {Counter}}) ->
    case {io:get_line(Io_device_1, ""), io:get_line(Io_device_2, "") } of
        {eof, _ } ->
            file:close(Io_device_1),
            file:close(Io_device_2),
            Counter;
        { _, eof} ->
            file:close(Io_device_1),
            file:close(Io_device_2),
            Counter;
        {{error, Reason}, _}->
            file:close(Io_device_1),
            throw(Reason);
        {_, {error, Reason}}->
            file:close(Io_device_2),
            throw(Reason);
        {Data_1, Data_2} ->
            % Обрезаем предложения на уровне символов.
            Data_1_croped = string:strip(lists:sublist(Data_1, ?CHAR_PER_LINE_MAX_COUNT), both, $\n),
            Data_2_croped = string:strip(lists:sublist(Data_2, ?CHAR_PER_LINE_MAX_COUNT), both, $\n),

            %?NGRAM_SIZE
            case sentences:times_sentences(Data_1_croped, Data_2_croped, 3) of
                [] -> ok;
                Translation -> apply(Module, Function, [Rredis_client, {Translation, Counter}])
            end,

            ?LOG_COUNTER_REM(Module, Function, Argumens, Counter, 10),
            process_each_line(Rredis_client,  {Module, Function, Argumens}, {{Io_device_1, Io_device_2}, {Counter + 1}})
    end.

% ==========================================================================

init_prob_table(Rredis_client)->
    Norma = db_redis:norma(Rredis_client),
    init_prob_table(Rredis_client, {{Norma}, {0}}).

init_prob_table(Rredis_client, {{Norma}, {Counter}})->
    case db_redis:init_prob_table(Rredis_client, {Norma, Counter}) of
        true ->
            init_prob_table(Rredis_client, {{Norma}, {Counter + 1}});
        _ -> ok
    end.

