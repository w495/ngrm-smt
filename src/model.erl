-module(model).
-compile(export_all).

-include("../include/common.hrl").
-include("../include/words.hrl").




train_s(Pairs) ->
    train(Pairs).

train_p(Pairs) ->
    train(Pairs),
    exit(self(), kill).

train(Pairs)->
    mem_table:set_table(t_ef),
    mem_table:set_table(s_total_e),
    mem_table:set_table(total_f),
    mem_table:set_table(count_ef),
    mem_table:restore(t_ef, Pairs),

    train(Pairs, ?MODEL_ITER_STEPS),

    Res = ets:select(mem_table:get_table(t_ef), [{{'$1','$2'},[{'>','$2',?MODEL_CROP_LIMIT}],['$$']}]),

    ?LOG("~n~p~n", [Res]),

    mem_table:save_to_store(t_ef,
        ets:select(mem_table:get_table(t_ef),
            [{{'$1','$2'},[{'>','$2',?MODEL_CROP_LIMIT}],['$$']}])
    ),


    mem_table:drop_table(t_ef),
    mem_table:drop_table(s_total_e),
    mem_table:drop_table(total_f),
    mem_table:drop_table(count_ef),


    ?LOG("~n model stoped ~n", []).

train(_Pairs, 0) -> ok;
train(Pairs, Steps) ->

    % ! count(e|f ) = 0 for all e,
    % ! total(f ) = 0 for all f

    mem_table:clear_table(count_ef),
    mem_table:clear_table(total_f),

    cncc(Pairs),
    estimate_probabilities(Pairs),
    train(Pairs, Steps - 1).

cncc([]) -> [];
cncc([Sent | Rest ])->

    mem_table:clear_table(s_total_e),

    compute_normalization(Sent),
    collect_counts(Sent),
    cncc(Rest).


compute_normalization([]) -> ok;
compute_normalization([{Word_1, Word_2}| Rest ]) ->

    mem_table:add_value(s_total_e, Word_1,
        mem_table:get_value(t_ef, {Word_1, Word_2}, ?MODEL_TABLE_T_EF_NORMA), ?MODEL_TABLE_DEFAULT),

    compute_normalization(Rest).


collect_counts([]) -> ok;
collect_counts([{Word_1, Word_2}| Rest ]) ->

    T_ef        = mem_table:get_value(t_ef, {Word_1, Word_2}, ?MODEL_TABLE_T_EF_NORMA),
    S_total_e   = mem_table:get_value(s_total_e, Word_1),

    %?LOG("~nCount_ef = ~p, Total_f = ~p", [Count_ef, Total_f]),

    Res =  T_ef / S_total_e,

    mem_table:add_value(count_ef, {Word_1, Word_2}, Res, ?MODEL_TABLE_DEFAULT),
    mem_table:add_value(total_f, Word_2, Res, ?MODEL_TABLE_DEFAULT),

    collect_counts(Rest).


estimate_probabilities([]) -> ok;
estimate_probabilities([Sent | Rest ])->
    estimate_probabilities_1(Sent),
    estimate_probabilities(Rest).

estimate_probabilities_1([]) -> ok;
estimate_probabilities_1([{Word_1, Word_2}| Rest ])->

    Count_ef = mem_table:get_value(count_ef, {Word_1, Word_2}),
    Total_f = mem_table:get_value(total_f, Word_2),

    %?LOG("~nCount_ef = ~p, Total_f = ~p", [Count_ef, Total_f]),

    mem_table:set_value(t_ef, {Word_1, Word_2}, Count_ef / Total_f),

    estimate_probabilities_1(Rest).


