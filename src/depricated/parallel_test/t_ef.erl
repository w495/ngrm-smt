-module(t_ef).
-compile(export_all).


-include("../include/common.hrl").
-include("../include/db.hrl").
-include("../include/words.hrl").


-define(TIMED, 5000).

start() ->
    receive
        {initialize, Norma} ->
            put(norma, Norma),
            ?LOG("Norma Norma = ~p~n", [Norma]),
            start();
        {put_, {Key, Value}} ->
            put(Key, Value),
            %?LOG("Key= ~p Value ~p ~n", [Key, Value]),
            start();
        {get_, {Who, Key}} ->
            Who ! {value, get_(Key)},
            start();
        {erase_ } ->
            erase(),
            start()
    after ?TIMED ->
            %?LOG("~p timed out ... continue ~n", [?MODULE]),
            start()
    end.

get_(Key)->
    case get(Key) of
        undefined ->
            case get(norma) of
                undefined -> 1;
                Norma -> Norma
            end;
        Value ->
            Value
    end.