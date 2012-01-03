-module(smt).


fds(Source, Target) ->
    step([], Source, Target).


step(Passed, Current, [Current|Passed]) ->
    passed(Current);


step(Passed, State_1, Local_target) ->
    translate(State_1, State_2),
    not_in(State_2, Passed),
    step([State_1|Passed], State_2, Local_target).