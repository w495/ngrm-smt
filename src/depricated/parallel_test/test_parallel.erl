%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% УСТАРЕЛО
%%%
%%%     Идея была в том, чтобы в качетве таблиц алгоритма EM
%%%     использовать словари процессов.
%%%
%%%
%%%     Этот вариант мы не используем, потому что при большом объеме данных
%%%     работа сильно замедляется.
%%%     Возможная причина в timeout сообщений, но если по времени проводить
%%%     отсечку, то на достоверности расчетов можно совсем ставить крест.
%%%     По сравнению с ets медленне в 6 раз.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(test_parallel).
-compile(export_all).

-include("../include/common.hrl").
-include("../include/db.hrl").
-include("../include/words.hrl").

-define(P_TIMED, 5000).

start(Pairs)->
    set_table(t_ef),
    set_table(s_total_e),
    set_table(total_f),
    set_table(count_ef),
    train(Pairs),
    ok.

train(Pairs)->
    Norma = 1 / lists:flatlength(Pairs),
    io:format("~n flatlength ~p~n", [lists:flatlength(Pairs)]),
    io:format("~n length ~p~n", [length(Pairs)]),
    io:format("~n Norma ~p~n", [Norma]),

    clear_table(t_ef),

    initialize_table(t_ef, Norma),
    train(Pairs, 10),
    ok.

train(_Pairs, 0) -> ok;
train(Pairs, Steps) ->

    % ! count(e|f ) = 0 for all e,
    % ! total(f ) = 0 for all f

    clear_table(count_ef),
    clear_table(total_f),

    cncc(Pairs),
    %estimate_probabilities(MiniCorp),
    train(Pairs, Steps - 1).

cncc([]) -> ok;
cncc([Sent | Rest ])->

    clear_table(s_total_e),

    compute_normalization(Sent),
    collect_counts(Sent),
    cncc(Rest).

compute_normalization([]) -> ok;
compute_normalization([{Word_1, Word_2}| Rest ]) ->

    add_value(s_total_e, Word_1, get_value(t_ef, {Word_1, Word_2})),

    compute_normalization(Rest).


collect_counts([]) -> ok;
collect_counts([{Word_1, Word_2}| Rest ]) ->

    T_ef        = get_value(t_ef, {Word_1, Word_2}),
    S_total_e   = get_value(s_total_e, Word_1),

    Res =  T_ef / S_total_e,

    add_value(count_ef, {Word_1, Word_2}, Res),
    add_value(total_f, Word_2, Res),

    collect_counts(Rest).


estimate_probabilities([]) -> ok;
estimate_probabilities([Sent | Rest ])->
    estimate_probabilities_1(Sent),
    estimate_probabilities(Rest).

estimate_probabilities_1([]) -> ok;
estimate_probabilities_1([{Word_1, Word_2}| Rest ])->

    Count_ef = get_value(count_ef, {Word_1, Word_2}),
    Total_f = get_value(total_f, Word_2),

    %?LOG("~nCount_ef = ~p, Total_f = ~p", [Count_ef, Total_f]),

    set_value(t_ef, {Word_1, Word_2}, Count_ef / Total_f),

    estimate_probabilities_1(Rest).


add_value(Table, Key, Value)->
    get_table(Table) ! {add, {Key, Value}}.

set_value(Table, Key, Value)->
    get_table(Table) ! {put_, {Key, Value}}.

get_value(Table, Key) ->
    get_value(Table, Key, 100).

get_value(Table, Key, Time)->
    get_table(Table) ! {get_, {self(), Key}},
    receive
        {value, Value} -> Value
    after Time ->
            ?LOG("worker timed out reader() ~n", [])
    end.


initialize_table(Name, Arg) ->
    get_table(t_ef) ! {initialize, Arg}.

clear_table(Name) ->
    get_table(Name) ! {erase_}.

set_table(Name) ->
    set_table(Name, spawn(Name, start, [])).

set_table(Name, Table) ->
    case get(Name) of
        undefined ->
            io:format("~nundefined Name ~p~n", [Name]),
            ok;
        Old_table ->
            io:format("~nOld_table ~p~n", [Old_table]),
            exit(Old_table, kill)
    end,
    put(Name, Table).

get_table(Name) ->
    case get(Name) of
        undefined ->
            receive
                {Name, Table} ->
                    put(Name, Table),
                    ?LOG("Table ~p~n", [Table]),
                    Table
            after ?P_TIMED ->
                    ?LOG("worker timed out at t(e|f) ~n", [])
            end;
        Table ->
            Table
    end.

