-module(db_redis).
-compile(export_all).

-include("../include/common.hrl").
-include("../include/db.hrl").

client() ->
    eredis:start_link([{database, ?TOTAL_DB}]).

client(Args) ->
    Host           = proplists:get_value(host, Args, "127.0.0.1"),
    Port           = proplists:get_value(port, Args, 6379),
    Database       = proplists:get_value(database, Args, 0),
    Password       = proplists:get_value(password, Args, ""),
    ReconnectSleep = proplists:get_value(reconnect_sleep, Args, 100),
    {ok, Client} = eredis:start_link(Host, Port, Database, Password, ReconnectSleep),
    Client.


% INIT_TABLES
% ==========================================================================

%% Инициaлизирует параметры таблиц.
%% Тут мы передаем целиком предложение.
%% Так быстрее происходит обмен с базой.
%%
%% ВАЖНО: в /etc/redis/redis.conf timeout = 0 (отключить вообще)
%%

init_tables(Rredis_client, {Pairs, Counter})->
        Pipeline_1 = lists:concat([
            [
                ["HINCRBY", ?TOTAL_VOCABULARY_HASH_NAME, Pair, 1], % количество пар
                ["HINCRBY", ?VOCABULARY_1_HASH_NAME, Word_1, 1],   % словарь языка 1
                ["HINCRBY", ?VOCABULARY_2_HASH_NAME, Word_2, 1]    % словарь языка 2
            ]
        || {Word_1, Word_2} = Pair <- Pairs]),
        Pipeline = lists:concat([Pipeline_1, [["HSET", ?TMP_HASH_NAME, Counter, erlang:term_to_binary(Pairs)]] ]),
        %io:format("Pipeline =~p", [Pipeline]),
        eredis:qp(Rredis_client, Pipeline).

%% Инициaлизирует параметры таблиц
%% Тут мы передаем по одной паре.
%% Так надежнее.
%%
%% ВАЖНО: в /etc/redis/redis.conf timeout = 0 (отключить вообще)
%%
one_pair_init_tables({Word_1, Word_2} = Pair, Rredis_client)->
    %%spawn(fun() ->
        Pipeline = [
            ["HINCRBY", ?TOTAL_VOCABULARY_HASH_NAME, Pair, 1],
            ["HINCRBY", ?VOCABULARY_1_HASH_NAME, Word_1, 1],
            ["HINCRBY", ?VOCABULARY_2_HASH_NAME, Word_2, 1]
        ],
        try
            State = eredis:qp(Rredis_client, Pipeline),
            case State of
                [{ok, _}, {ok, _}, {ok, _}, {ok, _}] -> ok;
                Err -> io:format("~ncase Error ~p~n", [Err])
            end
         catch
            _Throw:Error ->
                io:format("~nError ~p~n", [Error])
         end.
    %%end).

%% Возвратит количество слов.
len(Rredis_client)->
    {ok, Len} = eredis:q(Rredis_client, ["HLEN", ?TOTAL_VOCABULARY_HASH_NAME]),
    utils:to_integer(Len).

norma(Rredis_client)->
    1.0 / len(Rredis_client).

init_prob_table(Rredis_client, {Norma, Counter})->
        case eredis:q(Rredis_client, ["HGET", ?TMP_HASH_NAME, Counter]) of
            {ok, undefined} -> false;
            {ok, Val} ->
                Pipeline = lists:concat([
                    [
                        ["HSET", ?T_EF, Pair, erlang:float_to_list(Norma)]
                    ]
                || Pair <- erlang:binary_to_term(Val)]),
                eredis:qp(Rredis_client, Pipeline),
                true
        end.


sentence(Rredis_client, {Counter})->
    case eredis:q(Rredis_client, ["HGET", ?TMP_HASH_NAME, Counter]) of
        {ok, undefined} -> {false, []};
        {ok, Val} -> {true, binary_to_term(Val)}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 
%% Для функций ниже возможны, более эффективные варианты.
%% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

sentence_init(Rredis_client, {Counter})->
    case eredis:q(Rredis_client, ["HGET", ?TMP_HASH_NAME, Counter]) of
        {ok, undefined} -> {false, []};
        {ok, Val} ->
            Pipeline = lists:concat([
                [
                    ["HSET", ?COUNT_EF, Pair, "0.0"],
                    ["HSET", ?TOTAL_F, Word_2, "0.0"]
                ]
            || {Word_1, Word_2} = Pair <- erlang:binary_to_term(Val)]),
            eredis:qp(Rredis_client, Pipeline),
            {true, []}
    end.

sentence_m(Rredis_client, {Counter})->
    case eredis:q(Rredis_client, ["HGET", ?TMP_HASH_NAME, Counter]) of
        {ok, undefined} -> {false, []};
        {ok, Val} ->
            Pairs = binary_to_term(Val),
            add_s_total(Rredis_client, {Pairs}),
            collect_counts(Rredis_client, {Pairs}),

            {true, Pairs}
    end.


add_s_total(_Rredis_client,  {[]}) -> ok;
add_s_total(Rredis_client, {[{Word_1, Word_2} = Pair | Rest ]}) ->
    {ok, Tef} = eredis:q(Rredis_client, ["HGET", ?T_EF, Pair]),
    case eredis:q(Rredis_client, ["HGET", ?S_TOTAL_E, Word_1]) of
        {ok, undefined} ->
            eredis:q(Rredis_client,["HSET", ?S_TOTAL_E, Word_1, Tef]);
        {ok, StotalE} ->
            New_StotalE = erlang:float_to_list(
                    utils:to_float(erlang:binary_to_list(StotalE)) +
                    utils:to_float(erlang:binary_to_list(Tef))
            ),
            eredis:q(Rredis_client,["HSET", ?S_TOTAL_E, Word_1, New_StotalE])
    end,
    add_s_total(Rredis_client, {Rest}).


collect_counts(_Rredis_client,  {[]}) -> ok;
collect_counts(Rredis_client, {[{Word_1, Word_2} = Pair | Rest ]}) ->
    {ok, Tef} = eredis:q(Rredis_client, ["HGET", ?T_EF, Pair]),
    {ok, StotalE} = eredis:q(Rredis_client, ["HGET", ?S_TOTAL_E,    Word_1]),
    Value_float = utils:to_float(erlang:binary_to_list(Tef))/
        utils:to_float(erlang:binary_to_list(StotalE)),
    case
        {
            eredis:q(Rredis_client, ["HGET", ?COUNT_EF, Pair]),
            eredis:q(Rredis_client, ["HGET", ?TOTAL_F,  Word_2])
        }
        of
            {{ok, undefined},{ok, undefined}}->
                %% ----------------------------------------------------------
                Value_list = erlang:float_to_list(Value_float),
                eredis:qp(Rredis_client, [
                    ["HSET", ?COUNT_EF, Pair,   Value_list],
                    ["HSET", ?TOTAL_F,  Word_2, Value_list]
                ]);
            {{ok, undefined},{ok, Total_f}}->
                %% Слово такое было, но перевелось иначе
                %% ----------------------------------------------------------
                Value_list = erlang:float_to_list(Value_float),
                New_Total_f =
                    erlang:float_to_list(
                        utils:to_float(erlang:binary_to_list(Total_f))
                        +  Value_float
                    ),
                eredis:qp(Rredis_client, [
                    ["HSET", ?COUNT_EF, Pair,   Value_list],
                    ["HSET", ?TOTAL_F,  Word_2, New_Total_f]
                ]);
            {{ok, Count_ef}, {ok, Total_f}} ->
                %% ----------------------------------------------------------
                New_Count_ef =
                    erlang:float_to_list(
                        utils:to_float(erlang:binary_to_list(Count_ef))
                        +  Value_float
                    ),
                New_Total_f =
                    erlang:float_to_list(
                        utils:to_float(erlang:binary_to_list(Total_f))
                        +  Value_float
                    ),
                eredis:qp(Rredis_client, [
                    ["HSET", ?COUNT_EF, Pair,   New_Count_ef],
                    ["HSET", ?TOTAL_F,  Word_2, New_Total_f]
                ])
    end,
    collect_counts(Rredis_client, {Rest}).

sentence_e(Rredis_client, {Counter})->
    case eredis:q(Rredis_client, ["HGET", ?TMP_HASH_NAME, Counter]) of
        {ok, undefined} -> {false, []};
        {ok, Val} ->
            Pipeline = lists:concat([
                [
                    ["HSET", ?T_EF, Pair, estimate_prob(Rredis_client, {Pair})]
                ]
            || Pair <- erlang:binary_to_term(Val)]),
            eredis:qp(Rredis_client, Pipeline),
            {true, []}
    end.

estimate_prob(_Rredis_client,  {[]}) -> ok;
estimate_prob(Rredis_client, {{Word_1, Word_2} = Pair}) ->

    {ok, Count_ef} = eredis:q(Rredis_client, ["HGET", ?COUNT_EF, Pair]),
    {ok, Total_f} = eredis:q(Rredis_client, ["HGET", ?TOTAL_F, Word_2]),

    Res = erlang:float_to_list(utils:to_float(erlang:binary_to_list(Count_ef)) /
        utils:to_float(erlang:binary_to_list(Total_f))),
    ?LOG("~n~p ~p = ~p~n", [Word_1, Word_2, Res]),
    Res.
