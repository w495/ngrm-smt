-module(model_new).
-compile(export_all).

-include("../include/common.hrl").
-include("../include/words.hrl").

start() ->

    ok.


train_s(Pairs) ->
    train(Pairs).

train_p(Pairs) ->
    train(Pairs),
    exit(self(), kill).

train(Pairs)->
    S_total_e_table = mem_table:new_table(),
    Count_ef_table = mem_table:new_table(),
    Total_f_table = mem_table:new_table(),
    T_ef_table = mem_table:new_table(),

    Tables = [
        {s_total_e, S_total_e_table},
        {count_ef,  Count_ef_table},
        {total_f,   Total_f_table},
        {t_ef,      T_ef_table}
    ],

    train(Tables, Pairs, ?MODEL_ITER_STEPS),

    %mem_table:save_to_store(t_ef,
    %    ets:select(mem_table:get_table(t_ef),
    %        [{{'$1','$2'},[{'>','$2',?MODEL_CROP_LIMIT}],['$$']}])
    %),

    mem_table:drop_table(S_total_e_table),
    mem_table:drop_table(Count_ef_table),
    mem_table:drop_table(Total_f_table),
    mem_table:drop_table(T_ef_table),

    ?LOG("~n model stoped ~n", []).

train(_Tables,  _Pairs, 0) -> ok;
train(Tables, Pairs, Steps) ->

    % ! count(e|f ) = 0 for all e,
    % ! total(f ) = 0 for all f

    mem_table:clear_table(proplists:get_value(count_ef, Tables)),
    mem_table:clear_table(proplists:get_value(total_f, Tables)),

    cncc(Pairs),
    estimate_probabilities(Tables, Pairs),
    train(Tables, Pairs, Steps - 1).

cncc(_Tables, []) -> [];
cncc(Tables, [Sent | Rest])->

    mem_table:clear_table(proplists:get_value(s_total_e, Tables)),

    compute_normalization(Tables, Sent),
    collect_counts(Tables, Sent),
    cncc(Tables, Rest).


compute_normalization(_Tables, []) -> ok;
compute_normalization(Tables, [{Word_1, Word_2}| Rest ]) ->
    S_total_e_table = proplists:get_value(s_total_e, Tables),
    T_ef_table = proplists:get_value(t_ef, Tables),

    mem_table:add_value(S_total_e_table, Word_1,
        mem_table:get_stored_value(T_ef_table, {Word_1, Word_2},
            ?MODEL_TABLE_T_EF_NORMA),
                ?MODEL_TABLE_DEFAULT),

    compute_normalization(Tables, Rest).


collect_counts(_Tables, []) -> ok;
collect_counts(Tables, [{Word_1, Word_2}| Rest ]) ->
    S_total_e_table = proplists:get_value(s_total_e, Tables),
    Count_ef_table = proplists:get_value(count_ef, Tables),
    Total_f_table = proplists:get_value(total_f, Tables),
    T_ef_table = proplists:get_value(t_ef, Tables),

    T_ef        = mem_table:get_stored_value(T_ef_table, {Word_1, Word_2},
        ?MODEL_TABLE_T_EF_NORMA),
    S_total_e   = mem_table:get_value(S_total_e_table, Word_1),

    Res =  T_ef / S_total_e,

    mem_table:add_value(Count_ef_table , {Word_1, Word_2}, Res,
        ?MODEL_TABLE_DEFAULT),
    mem_table:add_value(Total_f_table, Word_2, Res, ?MODEL_TABLE_DEFAULT),

    collect_counts(Rest).


estimate_probabilities(_Tables, []) -> ok;
estimate_probabilities(Tables, [Sent | Rest])->
    estimate_probabilities_1(Sent),
    estimate_probabilities(Rest).

estimate_probabilities_1(_Tables, []) -> ok;
estimate_probabilities_1(Tables, [{Word_1, Word_2}| Rest])->
    Count_ef_table = proplists:get_value(count_ef, Tables),
    Total_f_table = proplists:get_value(total_f, Tables),
    T_ef_table = proplists:get_value(t_ef, Tables),

    Count_ef = mem_table:get_value(Count_ef_table , {Word_1, Word_2}),
    Total_f = mem_table:get_value(Total_f_table, Word_2),

    mem_table:set_stored_value(T_ef_table, {Word_1, Word_2}, Count_ef / Total_f),

    estimate_probabilities_1(Rest).


