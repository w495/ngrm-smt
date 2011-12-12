-module(reader_worker).
-compile(export_all).

-include("../include/common.hrl").
-include("../include/db.hrl").
-include("../include/words.hrl").


-define(MEM_LIMIT_NGRAMS, 750).
-define(MEM_LIMIT_NGRAMS_BYTES, ?MEM_LIMIT_NGRAMS*1024*1024).

-define(MEM_LIMIT_PARALLEL, 0).
-define(MEM_LIMIT_PARALLEL_BYTES, ?MEM_LIMIT_PARALLEL*1024*1024).


-define(BUFF_SIZE, 10).
-define(WORKER_TIMED, 5000).


start()->
    reader() ! next,
    process_one_line({1, []}).

reader()->
    case get(reader) of
        undefined ->
            receive
                {reader, Reader} ->
                    put(reader, Reader),
                    Reader
            after ?WORKER_TIMED ->
                    ?LOG("worker timed out reader() ~n", [])
            end;
        Pid ->
            Pid
    end.

process_one_line({Counter, Buffer})->
    receive
        {pair, {Data_1, Data_2, C}} ->
            case (erlang:memory(total) < ?MEM_LIMIT_NGRAMS_BYTES) of
                true ->
                    %io:format("true", []),
                    %Translation = sentences:times_words(Data_1, Data_2)
                    Translation = sentences:times_sentences(Data_1, Data_2, ?NGRAM_SIZE)
                    %Translation = sentences:comb_sentences(Data_1, Data_2, ?NGRAM_SIZE, ?NGRAM_SIZE)
                    ;
                false ->
                    %io:format("false", []),
                    %Translation = sentences:times_words(Data_1, Data_2)
                    Translation = sentences:times_sentences(Data_1, Data_2, ?NGRAM_SIZE)
                    %Translation = sentences:comb_sentences(Data_1, Data_2, ?NGRAM_SIZE, ?NGRAM_SIZE)
            end,

            case (Counter rem ?BUFF_SIZE)of
                0 ->
                    process_buffer(Buffer),
                    New_buffer  = [Translation],
                    ?LOG("~nSave at ~p~n", [Counter]);
                _ ->
                    New_buffer = [Translation | Buffer]
            end,

            reader() ! next,

            process_one_line({Counter+1, New_buffer});
        stop ->
            process_buffer(Buffer),
            ?LOG("~nsave.~p~n", [Counter]),
            ?LOG("worker stoped~n", []),
            ok
    after ?WORKER_TIMED ->
            ?LOG("worker timed out at process_one_line~n", []),
            ok
    end.

process_buffer(Buffer)->
    case (erlang:memory(total) < ?MEM_LIMIT_PARALLEL_BYTES) of
        true ->
            io:format("~n(+)memory(total) ~p ~n", [erlang:memory(total) / 1024 / 1024]),
            spawn(model, train_p, [Buffer]);
        false ->
            io:format("~n(-)memory(total) ~p ~n", [erlang:memory(total) / 1024 / 1024]),
            model:train_s(Buffer)
    end.
