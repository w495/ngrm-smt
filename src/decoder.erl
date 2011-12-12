-module(decoder).

-include("../include/db.hrl").

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
    [Unknown_word | decode_word_list(Rest_word_list, MaxSize, MaxSize)];

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
        [] ->
            %% Если не удалось, возьмем фразу поменьше
            decode_word_list(Word_list, Size-1, MaxSize);
        Val_set ->
            {Prop, Value} = alists:max(Val_set),
            %% Если удалось, переводим дальше.
            [ Value | decode_word_list(Rest_word_list, Size, MaxSize)]
    end.



to_pairs( []) -> [];
to_pairs( [Word, Prob | Tail ]) ->
    [{erlang:list_to_float(erlang:binary_to_list(Prob)), erlang:binary_to_list(Word)} | to_pairs(Tail )].

try_to_translate(Ngram) ->
    {ok, Db} = eredis:start_link([{database, ?REVERSE_INDEX_DB}]),
    {ok, List} = eredis:q(Db, ["HGETALL", Ngram]),
    to_pairs(List).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% Формирование предложения
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

make_sentence(List) ->
    string:join(List, " ").
