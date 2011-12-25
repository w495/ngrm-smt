-module(mem_table_new).

-compile(export_all).


-include("../include/db.hrl").

start_db(OPTIONS) ->
    %Model_db = db:start([{database, ?MODEL_DB}]),
    db:start(OPTIONS).

new_table()->
    ets:new('table', [set]).

%% --------------------------------------------------------------------------

get_value({Table, Store}, Key, Default) ->
    case catch(ets:lookup(get_table(Name), Key)) of
        {'EXIT', _} ->
            case get_stored_value(Store, Key) of
                undefined -> Default;
                Value -> Value
            end;
        [] ->
            case get_stored_value(Store, Key) of
                undefined -> Default;
                Value -> Value
            end;
        undefined ->
            case get_stored_value(Store, Key) of
                undefined -> Default;
                Value -> Value
            end;
        [{Key, undefined}]  -> Default;
        [{Key, Value}]  -> Value
    end;

get_value(Table, Key, Default) ->
    case catch(ets:lookup(Table, Key)) of
        {'EXIT', _} -> Default;
        undefined -> Default;
        [] -> Default;
        [{Key, undefined}]  -> Default;
        [{Key, Value}]  -> Value
    end.

set_stored_value({Table, {Db, Name}}, Key, Value) ->
    db:h_set(Db, Name, Key, Value);

set_value(Table, Key, Value) ->
    ets:insert(Table, {Key, Value}).

add_value(Table, Key, Value, Default) ->
    Old_value = get_value(Name, Key, Default),
    set_value(Table, Key, Old_value + Value).


%% --------------------------------------------------------------------------

clear_table(Table)->
    ets:delete_all_objects(Table).

drop_table(Table)->
    ets:delete(Table).


-include_lib("eunit/include/eunit.hrl").

test() -> ok.
test(1) -> ok.