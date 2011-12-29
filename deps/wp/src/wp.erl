%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% FOR FUTURE
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(wp).

-behaviour(application).
-export([start/2, stop/1]).

-export([call_function_1/3, call_function_1/4, call_function_1/5]).

-export([call_function_n/3, call_function_n/4, call_function_n/5]).

start(_Type, _Args) ->
    wp_supervisor:start_link().

stop(_State) ->
    ok.

%%% ----------------------------------------------------------------------------

call_function_n(Mod, Fun, Args) ->
    %%% rpc:call(wp@localhost, wp, call_function, [ngrams, ngram, [[a, b, c, d], 2]]).
    gen_server:call(whereis(wp_gen_server), {f_n, {Mod, Fun, Args}}, 5000).

call_function_n(Server, Mod, Fun, Args) ->
    gen_server:call(Server, {f_n, {Mod, Fun, Args}}, 5000).

call_function_n(Server, Mod, Fun, Args, State) ->
    gen_server:call(Server, {f_n, {Mod, Fun, Args}}, State).

%%% ----------------------------------------------------------------------------

call_function_1(Mod, Fun, Args) ->
    gen_server:call(whereis(wp_gen_server), {f_1, {Mod, Fun, Args}}, 5000).

call_function_1(Server, Mod, Fun, Args) ->
    gen_server:call(Server, {f_1, {Mod, Fun, Args}}, 5000).

call_function_1(Server, Mod, Fun, Args, State) ->
    gen_server:call(Server, {f_1, {Mod, Fun, Args}}, State).

