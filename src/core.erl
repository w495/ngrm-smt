-module(core).
-compile(export_all).

-include("../include/common.hrl").
-include("../include/processes.hrl").
-include("../include/words.hrl").




train({Filename_1, Filename_2}) ->

    DataBase = spawn(db, start, []),


    Worker = spawn(worker, start, []),
    Reader = spawn(reader, start, []),


    Reader ! {read, {Filename_1, Filename_2}},
    Reader ! {worker, Worker},

    Worker ! {reader, Reader},

    exit(Reader, normal),
    exit(Worker, normal),
    exit(DataBase, normal),
    ok.





% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 
% 
% -define(N_SIZE, 5).
% 
% 
% decode(Source_sentence) ->
% 
%     Stack_options = [],
%     Stacks =
%         [
%             ets:new(
%                 erlang:list_to_atom("stack_" ++ erlang:integer_to_list(Name)),
%                 Stack_options
%             )
%         ||
%             Name <- lists:seq(1, ?N_SIZE)
%         ],
% 
%     decode_with_stack(Source_sentence, Stacks, ?N_SIZE),
% 
%     ok.
% 
% 
% decode_with_stacks(Source_sentence, _, 0) -> ok;
% decode_with_stacks(Source_sentence, Stacks, Size)->
%     Stack = proplists:get_value(Size, Stacks),
% 
%     decode_with_stack(Source_sentence, Stack, Size),
% 
%     decode_with_stacks(Source_sentence, Stacks, Size - 1).
% 
% decode_with_stack(Source_sentence, Stack, Size)->
% 
%     ok.

% % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % % 


% 
% decode_words(Input_string)->
%     [Fw | Rw]  = Word_List = words:list(Input_string),
%     ?LOG("~n~p~n", [Word_list]),
% 
%     
% 
%     ok.


% beam_search(Init_list)->
%     Problem_list = [
%         {Init_elem, get_estimation(Init_elem)}
%         || Init_elem <- Init_list
%     ]
% 


% decode(Ngram_proplist, Size)->
%     Ngram_list_per_size = proplists:get_value(Size, Ngram_proplist),

% 
%     [ First_ngram | Rest_ngram_list_per_size ] = Ngram_list_per_size,
% 
%     ?LOG("~n~p~n", [First_ngram]),
% 
%     % Пытамся перевести первую саму большую n-грамму
%     case try_to_translate(First_ngram) of
%         {ok, Translation} ->
%             % Если получилось
%             % --> перевели выходим из алгоритма.
%             ?LOG("Translation = ~p~n", [Translation]);
%         {no, Translation}
%             % Если не получилось
% 
%             -> ok
% 
%     end,
% 
%     ?LOG("~n~p~n", [Ngram_proplist]).
% 
% try_to_translate(Ngram)->
%     Translation = "",
%     {ok, Translation}.
% 
% 
