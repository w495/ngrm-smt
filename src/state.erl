-module(state).
-compile(export_all).


new(Value) ->
    new(Value, [], 0).

new(Value, Parent)->
    new(Value, Parent, 0).

new(Value, Parent, Cost)->
    [
        {value, Value},
        {parent, Parent},
        {depth, case proplists:get_value(depth, Parent) of undefined -> 1; Pd -> Pd + 1 end},
        {path_cost, case proplists:get_value(path_cost, Parent) of undefined -> Cost; Pc -> Pc + Cost end},
        {cost, Cost}
    ].


parent(State) ->
    proplists:get_value(parent, State).

path(undefined) -> [];
path(State) ->
    lists:append([path(proplists:get_value(parent, State)), [State]]).

path_values(undefined) -> [];
path_values(State) ->
    case proplists:get_value(value, State) of
        undefined -> [];
        Value ->
            lists:append([path_values(proplists:get_value(parent, State)), [Value]])
    end.

depth(State) ->
    proplists:get_value(depth, State).


path_cost(State) ->
    proplists:get_value(path_cost, State).


cost(State) ->
    proplists:get_value(cost, State).


