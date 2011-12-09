-module(ngrmsmt_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(ngrmsmt_supervisor, []).

init(_Args) ->
    {ok, {{one_for_all, 10, 6000},
          [{ngrmsmt_gen_server, {ngrmsmt_gen_server, start_link, []},
            permanent, brutal_kill, worker, [ngrmsmt_gen_server]}]}}.
