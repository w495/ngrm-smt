-module(simple_smt_decoder).

-export([decode/1]).

%% Простой фразовый декодировщик для
%% системы машинного перевода основанной на статистике

decode(Input_string) ->
    %% Разбиваем входную строку на слова.
    Word_list   = words:list(Input_string),
    %% Переводим список слов.
    Decoded_word_list = decode_word_list(Word_list, 5),
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
        Val_set ->
            {Prop, Value} = alists:max(Val_set),
            %% Если удалось, переводим дальше.
            [ Value | decode_word_list(Rest_word_list, Size, MaxSize)]
    end.

try_to_translate(Ngram) ->
    % Таблица соотвествий слов.
    case Ngram of
        % ----------------------------------
            ["i", "have"]    -> [
                    {0.1,                   ["ja", "obladaju"]},
                    {0.4,                   ["ja", "imeju"]},
                    {0.5,                   ["u", "menja", "est"]},
                    % ....
                    {0.0,                   []}
                ];
            ["have", "a"]    -> [
                    {0.1,                   ["imet'"]},
                    {0.1,                   ["imeju"]},
                    {0.1,                   ["imeem"]},
                    {0.1,                   ["imejut"]},
                    % ....
                    {0.0,                   []}
                ];
            ["a", "big"]     -> [
                    {0.9,                   ["bolshoj"]},
                    {0.1,                   ["malenkij"]},
                    % ....
                    {0.0,                   []}
                ];
            ["big", "fat"]   -> [
                    {0.6,                   ["ochen'", "zhirnij"]},
                    {0.5,                   ["bolshoj", "zhir"]},
                    % ....
                    {0.0,                   []}
                ];
            ["fat", "cat"]   -> [
                    {0.3,                   ["zhirnij", "kot"]},
                    {0.2,                   ["zhirnaja", "koshka"]},
                    % ....
                    {0.0,                   []}
                ];
        % ----------------------------------
            ["i"]     -> [
                    {0.1,                   ["ja"]},
                    {0.1,                   ["my"]},
                    % ....
                    {0.0,                   []}
                ];
            ["have"]  -> [
                    {0.1,                   ["imet'"]},
                    {0.1,                   ["imeju"]},
                    {0.1,                   ["imeem"]},
                    % ....
                    {0.0,                   []}
                ];
            ["big"]   -> [
                    {0.1,                   ["bolshoj"]},
                    {0.1,                   ["bolshaja"]},
                    {0.1,                   ["bolshije"]},
                    % ....
                    {0.0,                   []}
                ];
            ["fat"]   -> [
                    {0.1,                   ["zhirnij"]},
                    % ....
                    {0.0,                   []}
                ];
            ["cat"]   -> [
                    {0.8,                   ["kot"]},
                    {0.1,                   ["koshka"]},
                    % ....
                    {0.0,                   []}
                ];
            ["rat"]   -> [
                    {0.7,                   ["krysa"]},
                    {0.3,                   ["krysa"]},
                    % ....
                    {0.0,                   []}
                ];
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



