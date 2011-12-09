-module(model).
-compile(export_all).

-include("../include/common.hrl").
-include("../include/db.hrl").
-include("../include/words.hrl").


-define(NORMA,       0.5).

-define(DEFAULT,     0.0).



-define(ITER_STEPS,       10). % 5%
-define(SAVE_LIMIT,       0.05). % 5%


train_s(Pairs) ->
    train(Pairs).

train_p(Pairs) ->
    train(Pairs),
    exit(self(), kill).

train(Pairs)->
    mem_table:set_stored_table(t_ef, [{database, ?TOTAL_DB}]),
    mem_table:set_table(s_total_e),
    mem_table:set_table(total_f),
    mem_table:set_table(count_ef),

    %mem_table:restore(t_ef, Pairs),

    ?LOG("(+)train", []),

    train(Pairs, ?ITER_STEPS),

    ?LOG("(-)train", []),

    %% Full = ets:match(mem_table:get_table(t_ef), '$1'),
    Croped = ets:select(mem_table:get_table(t_ef), [{{'$1','$2'},[{'>','$2',?SAVE_LIMIT}],['$$']}]),

    ?LOG("(-)croped", []),

    %%?LOG("Full = ~p", [length(Full)]),
    %%?LOG("Croped = ~p", [length(Croped)]),

    % mem_table:save_to_store(t_ef, Croped),

    %%?LOG("Croped = ~p", [Croped]),

    mem_table:drop_table(t_ef),
    mem_table:drop_table(s_total_e),
    mem_table:drop_table(total_f),
    mem_table:drop_table(count_ef),
    ?LOG("~n model stoped~n", []),
    ok.

train(_Pairs, 0) -> ok;
train(Pairs, Steps) ->

    % ! count(e|f ) = 0 for all e,
    % ! total(f ) = 0 for all f

    mem_table:clear_table(count_ef),
    mem_table:clear_table(total_f),

    cncc(Pairs),
    estimate_probabilities(Pairs),
    train(Pairs, Steps - 1).

cncc([]) -> ok;
cncc([Sent | Rest ])->

    mem_table:clear_table(s_total_e),

    compute_normalization(Sent),
    collect_counts(Sent),
    cncc(Rest).


compute_normalization([]) -> ok;
compute_normalization([{Word_1, Word_2}| Rest ]) ->

    mem_table:add_value(s_total_e, Word_1,
        mem_table:get_value(t_ef, {Word_1, Word_2}, ?NORMA), ?DEFAULT),

    compute_normalization(Rest).


collect_counts([]) -> ok;
collect_counts([{Word_1, Word_2}| Rest ]) ->

    T_ef        = mem_table:get_value(t_ef, {Word_1, Word_2}, ?NORMA),
    S_total_e   = mem_table:get_value(s_total_e, Word_1),

    %?LOG("~nCount_ef = ~p, Total_f = ~p", [Count_ef, Total_f]),

    Res =  T_ef / S_total_e,

    mem_table:add_value(count_ef, {Word_1, Word_2}, Res, ?DEFAULT),
    mem_table:add_value(total_f, Word_2, Res, ?DEFAULT),

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


