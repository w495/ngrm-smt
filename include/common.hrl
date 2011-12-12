

-define(APP, ngrmsmt).

-define( FMT(F,P), lists:flatten(io_lib:format(F,P)) ).



-define(LOG,     io:format).

% ------------------------------------------------------------------------

-define(D_LOG_REM,     100).
-define(LOG_COUNTER(Module, Function, Args, Counter),
        case Counter rem ?D_LOG_REM of
            0 ->
                ?LOG("~n~p:~p(~p) String: ~p~n",
                    [Module, Function, Args, Counter]);
            _ ->
                ok
        end
    ).

% ------------------------------------------------------------------------

-define(LOG_COUNTER_REM(Module, Function, Args, Counter, Rem),
        case Counter rem Rem of
            0 ->
                ?LOG("~n~p:~p(~p) String: ~p~n",
                    [Module, Function, Args, Counter]);
            _ ->
                ok
        end
    ).

% ------------------------------------------------------------------------
