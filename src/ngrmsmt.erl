-module(ngrmsmt).

-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    reloader:agressive_load_modules(),
    main:main(),
    ngrmsmt_supervisor:start_link().

stop(_State) ->
    ok.

