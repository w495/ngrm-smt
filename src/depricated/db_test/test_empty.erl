-module(test_empty2).
-compile(export_all).

-define(NGRM_SIZE, 5).


word_counts_empty(Filename_1, Filename_2) ->
    case {file:open(Filename_1, read), file:open(Filename_2, read)} of
        {{ok, Io_device_1}, {ok, Io_device_2}} ->
            process_each_line_empty({Io_device_1, Io_device_2});
        {{error, Reason}, _} ->
            io:format("Filename_1 = ~s~n", [Reason]);
        {_, {error, Reason}} ->
            io:format("Filename_2 = ~s~n", [Reason])
    end.

process_each_line_empty({Io_device_1, Io_device_2}) ->
    case {io:get_line(Io_device_1, ""), io:get_line(Io_device_2, "") } of
        {eof, _ } ->
            file:close(Io_device_1), file:close(Io_device_2);
        { _, eof} ->
            file:close(Io_device_1), file:close(Io_device_2);
        {{error, Reason}, _}->
            file:close(Io_device_1), throw(Reason);
        {_, {error, Reason}}->
            file:close(Io_device_1), throw(Reason);
        {Data_1, Data_2} ->
            % средняя длинна слова в руссском языке 5.28 символов.
            % средняя длинна предложения --- 10.38 символов.
            % для английского эти цифры будут меньше.
            % 
            % далее мы будем рассматривать предложения короче 40 слов.
            % MAX = 40 * 5.28. = 211.20000000000002 < 256.
            Data_1_croped = lists:sublist(Data_1, 256),
            Data_2_croped = lists:sublist(Data_2, 256),

            Sentence_pair = sentences:zip_sentences(Data_1_croped, Data_2_croped, ?NGRM_SIZE),

            Total_s = 0,
            build_frequencies_empty_em_1(Sentence_pair, Total_s),

            process_each_line_empty({Io_device_1, Io_device_2})
    end.

%%% ==========================================================================
%%% 
%%% Работает с empty
%%%
%%%

build_frequencies_empty([], _) -> [];
build_frequencies_empty([_Word_pair|Rest], Total_s) ->
    build_frequencies_empty(Rest, Total_s).


build_frequencies_empty_em_1([], _) -> [];
build_frequencies_empty_em_1([_Word_pair|Rest], Total_s) ->
    build_frequencies_empty_em_1(Rest, Total_s).
