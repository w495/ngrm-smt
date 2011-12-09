-module(total_f).
-compile(export_all).


-include("../include/common.hrl").
-include("../include/db.hrl").
-include("../include/words.hrl").


-define(TIMED, 5000).

start() ->
    receive
        {initialize, Norma} ->
            put(norma, Norma),
            %?LOG("Norma Norma = ~p~n", [Norma]),
            start();
        {add, {Key, Value}} ->
            add(Key,Value),
            %?LOG("Key= ~p Value ~p ~n", [Key, Value]),
            start();
        {get_, {Who, Key}} ->
            Who ! {value, get_(Key)},
            start();
        {erase_ } ->
            erase(),
            start()
    after ?TIMED ->
            %?LOG("~p timed out ... continue~n", [?MODULE]),
            start()
    end.

get_(Key)->
    case get(Key) of
        undefined
            -> 0;
        Value ->
            Value
    end.


add(Key,Value)->
    put(Key, get_(Key) + Value).