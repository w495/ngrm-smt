%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% FOR FUTURE
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(wp_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link(wp_supervisor, []).

init(_Args) ->
    {ok, {{one_for_all, 10, 6000},
          [{wp_gen_server, {wp_gen_server, start_link, []},
            permanent, brutal_kill, worker, [wp_gen_server]}]}}.
