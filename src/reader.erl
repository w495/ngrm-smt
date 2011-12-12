-module(reader).
-compile(export_all).

-include("../include/common.hrl").


-include("../include/words.hrl").   %% для CHAR_PER_LINE_MAX_COUNT

-include("../include/processes.hrl").



start() ->
    receive
        {read, {Filename_1, Filename_2}} ->
            read({Filename_1, Filename_2})
    end.

read({Filename_1, Filename_2}) ->
    case{file:open(Filename_1, read), file:open(Filename_2, read)} of
        {{ok, Io_device_1}, {ok, Io_device_2}} ->
                process_each_line({{Io_device_1, Io_device_2}, {1, []}});
        {{error, Reason}, _ } ->
            io:format("file 1 (~s) error:~n|~s|~n", [Filename_1, Reason]);
        {_, {error, Reason}} ->
            io:format("file 2 (~s) error:~n|~s|~n", [Filename_2, Reason])
    end.


% ==========================================================================

process_each_line({{Io_device_1, Io_device_2}, {Counter, Buffer}}) ->
    case {io:get_line(Io_device_1, ""), io:get_line(Io_device_2, "") } of
        {eof, _ } ->
            file:close(Io_device_1),
            file:close(Io_device_2),
            worker() ! stop,
            Counter;
        { _, eof} ->
            file:close(Io_device_1),
            file:close(Io_device_2),
            worker() ! stop,
            Counter;
        {{error, Reason}, _}->
            file:close(Io_device_1),
            worker() ! stop,
            throw(Reason);
        {_, {error, Reason}}->
            file:close(Io_device_2),
            worker() ! stop,
            throw(Reason);
        {Data_1, Data_2} ->
            % Обрезаем предложения на уровне символов.
            Data_1_croped = string:strip(lists:sublist(Data_1, ?CHAR_PER_LINE_MAX_COUNT), both, $\n),
            Data_2_croped = string:strip(lists:sublist(Data_2, ?CHAR_PER_LINE_MAX_COUNT), both, $\n),

            %?NGRAM_SIZE
            %Translation = sentences:times_sentences(Data_1_croped, Data_2_croped, ?NGRAM_SIZE),

            worker() ! {pair, {Data_1_croped, Data_2_croped, Counter }},
            receive
                next ->
                    process_each_line({{Io_device_1, Io_device_2}, {Counter + 1, []}})
            after ?READER_READ_TIMEOUT ->
                    process_each_line({{Io_device_1, Io_device_2}, {Counter + 1, []}})
            end
    end.

% ==========================================================================


worker()->
    case get(worker) of
        undefined ->
            receive
                {worker, Worker} ->
                    put(worker, Worker),
                    Worker
            after ?READER_COMMAND_TIMEOUT ->
                    []
            end;
        Pid ->
            Pid
    end.
