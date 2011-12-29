-module(mem_table).

-export([
             set_table/1,
             set_named_table/1,
             set_stored_table/2,
             get_table/1,
             get_value/2,
             get_value/3,
             get_stored_value/3,
             save_to_store/2,
             restore/2,
             set_value/3,
             add_value/3,
             add_value/4,
             clear_table/1,
             drop_table/1,
             test/0,
             test/1
         ]
 ).


-include("../include/db.hrl").

get_store(Table) -> get(Table).

get_stored_value(Name, Key) ->
    db:h_get(get_store(get_table(Name)), Name, Key).

%% Связана с более долговременной памятью.
set_stored_table(Name, Param)->
    Table = ets:new(Name, [set]),
    put(Name, Table),
    put(Table, db:start(Param)),
    Table.

%% OLD
restore__(Name, Data) ->
    Objects = db:restore(get_store(get_table(Name)), Name, Data),
    ets:insert(get_table(Name), Objects).

%% OLD
save_to_store__(Name, Data) ->
    io:format("save_to_store", []),
    db:save_bulk(get_store(get_table(Name)), Name, Data).


restore(Name, Data) ->
    Db = db:start([{database, ?MODEL_DB}]),
    ets:insert(get_table(Name), db:restore(Db, Name, Data)).

save_to_store(Name, Data) ->
    Model_db = db:start([{database, ?MODEL_DB}]),
    Reverse_index_db = db:start([{database, ?REVERSE_INDEX_DB}]),
    db:save_bulk(Model_db, Name, Data),
    db:save_bulk_index(Reverse_index_db, Data).

set_table(Name)->
    Table = ets:new(Name, [set]),
    put(Name, Table),
    Table.

set_named_table(Name) ->
    case catch(ets:new(Name, [named_table, set])) of
        {'EXIT', _} ->
            ets:delete(Name),
            ets:new(Name, [named_table, set]);
        Table -> Table
    end.

get_table(Name) -> get(Name).

get_value(Name, Key) ->
    [{Key, Value}]  = ets:lookup(get_table(Name), Key),
    Value.


%% Связана с более долговременной памятью.
get_stored_value(Name, Key, Default) ->
    case catch(ets:lookup(get_table(Name), Key)) of
        {'EXIT', _} ->
            case get_stored_value(Name, Key) of
                undefined -> Default;
                Value -> Value
            end;
        [] ->
            case get_stored_value(Name, Key) of
                undefined -> Default;
                Value -> Value
            end;
        undefined ->
            case get_stored_value(Name, Key) of
                undefined -> Default;
                Value -> Value
            end;
        [{Key, undefined}]  -> Default;
        [{Key, Value}]  -> Value
    end.


get_value(Name, Key, Default) ->
    case catch(ets:lookup(get_table(Name), Key)) of
        {'EXIT', _} -> Default;
        undefined -> Default;
        [] -> Default;
        [{Key, undefined}]  -> Default;
        [{Key, Value}]  -> Value
    end.


set_value(Name, Key, Value) ->
    ets:insert(get_table(Name), {Key, Value}).

add_value(Name, Key, Value) ->
    Old_value = get_value(Name, Key),
    ets:insert(get_table(Name), {Key, Old_value + Value}).

add_value(Name, Key, Value, Default) ->
    Old_value = get_value(Name, Key, Default),
    ets:insert(get_table(Name), {Key, Old_value + Value}).

clear_table(Name)->
    ets:delete_all_objects(get_table(Name)).

drop_table(Name)->
    Table = get_table(Name),
    ets:delete(Table),
    erase(Table),
    erase(Name).


-include_lib("eunit/include/eunit.hrl").

test() -> ok.
test(1) -> ok.