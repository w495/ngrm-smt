-module(db).
-compile(export_all).

-include("../include/common.hrl").
-include("../include/db.hrl").
-include("../include/words.hrl").


start() ->
    {ok, Db} = eredis:start_link([{database, ?TOTAL_DB}]),
    Db.

start(Param) ->
    {ok, Db} = eredis:start_link(Param),
    Db.

restore(Db, Hash, Data) ->
    Keys = lists:append(Data),
    Hmget_list = ["HMGET", Hash | Keys ],
    {ok, Res_list} = eredis:q(Db, Hmget_list),
    Float_list = [
        case Res of
            undefined ->
                undefined;
            Value -> erlang:list_to_float(erlang:binary_to_list(Value))
        end
    ||
        Res <- Res_list
    ],
    lists:zip(Keys , Float_list).

h_get(Db, Hash, Key) ->
    case eredis:q(Db, ["HGET", Hash, Key]) of
        {ok, undefined } -> undefined;
        {ok, Value } ->
            ?LOG("~nValue = ~p~n", [Value]),
            erlang:list_to_float(erlang:binary_to_list(Value))
    end.

save_bulk(Db, Hash, Data) ->
    %% HMSET myhash field1 "Hello" field2 "World"
    Hmset_list = ["HMSET", Hash |
        lists:append([ [Key, erlang:float_to_list(Value)]
            || [Key, Value] <- Data])],
    eredis:q(Db, Hmset_list).

