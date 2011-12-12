-module(simple_ebmt_decoder).

-export([decode/1]).

%% Простой фразовый декодировщик для
%% системы машинного перевода основанной на примерах

decode(Input_string) ->
    %% Разбиваем входную строку на слова.
    Word_list   = words:list(Input_string),
    %% Переводим список слов.
    Decoded_word_list = decode_word_list(Word_list, 6),
    %% Формируем из него предложение.
    make_sentence(Decoded_word_list).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 
%%% Декодирование
%%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%
%% Переводит список слов Word_list
%%     с учетом размера фразы Phrase_Size.
%%
decode_word_list(Word_list, Phrase_Size) ->
    %%  
    %%  decode_word_list(arg_1, arg_2, arg_3).
    %%      arg_1 --- входной список слов.
    %%      arg_2 --- текущий размер рассматриваемой фразы.
    %%      arg_3 --- максимальный размер рассматриваемой фразы.
    decode_word_list(Word_list, Phrase_Size, Phrase_Size).

decode_word_list([], _, _) ->
    %% Если входной список слов пуст,
    %% значит переводить больше нечего.
    [];

decode_word_list([ Unknown_word | Rest_word_list], 0, MaxSize) ->
    %% Если текущий размер рассматриваемой фразы,
    %% значит,мы не можем перевести эту фразу с начала.
    %% Попробуем начать со второго слова.
    %% А первое слово текущей фразы признаем неизвестным.
    [[Unknown_word] | decode_word_list(Rest_word_list, MaxSize, MaxSize)];

decode_word_list(Word_list, Size, MaxSize)->
    %% Разбиваем список слов на 2 части.
    %% Первая --- фраза, которую хотим перевести.
    %% Вторая --- остаток предляжения.
    case Size < erlang:length(Word_list) of
        true ->
            {First_Ngram, Rest_word_list } = lists:split(Size, Word_list);
        false ->
            First_Ngram = Word_list,
            Rest_word_list = []
    end,

    %% Пытаемcя перевести фразу.
    case try_to_translate(First_Ngram) of
        {no} ->
            %% Если не удалось, возьмем фразу поменьше
            decode_word_list(Word_list, Size-1, MaxSize);
        Val ->
            %% Если удалось, переводим дальше.
            [ Val | decode_word_list(Rest_word_list, Size, MaxSize)]
    end.

try_to_translate(Ngram) ->
    % Таблица соотвествий слов.
    case Ngram of

            ["i", "have", "a", "big", "fat", "cat"]    ->
                ["u", "menja", "est'", "bolshoj", "zhirnij", "kot"];
            ["i", "have", "a", "big", "fat", "rat"]    ->
                ["u", "menja", "est'", "bolshoj", "zhirnij", "krys"];

        % ----------------------------------
            ["i", "have"]    -> ["ja", "imeju"];
            ["have", "a"]    -> ["imet'"];
            ["a", "big"]     -> ["bolshoj"];
            ["big", "fat"]   -> ["ochen'", "zhirnij"];
            ["fat", "cat"]   -> ["zhirnij", "kot"];
        % ----------------------------------
            ["i"]     -> ["ja"];
            ["have"]  -> ["imet'"];
            ["big"]   -> ["bolshoj"];
            ["fat"]   -> ["zhirnij"];
            ["cat"]   -> ["kot"];
            ["rat"]   -> ["krysa"];
        % ----------------------------------
            Val -> {no}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Формирование предложения
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


make_sentence(List) ->
    string:join(join_phrases(List), " ").

join_phrases([]) -> [];
join_phrases([Phrase|Tail] = List) ->
    [join_phrase(Phrase) | join_phrases(Tail)].

join_phrase(Phrase) ->
    string:join(Phrase, " ").



