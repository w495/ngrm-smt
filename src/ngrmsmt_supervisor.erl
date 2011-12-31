-module(ngrmsmt_supervisor).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SHUTDOWN_TIME_OUT, 2000).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_Args) ->

    Ngrmsmt_server = {ngrmsmt_server,{ngrmsmt_server, start_link,[]},
        permanent, ?SHUTDOWN_TIME_OUT, worker, [ngrmsmt_server]
    },

    Wp_server = {wp_server,
        {wp_server, start_link,[]},
        permanent, ?SHUTDOWN_TIME_OUT, worker, [wp_server]
    },

    Eredis = {eredis_client,
        {eredis_client, start_link,[local, "127.0.0.1", 6379, 0, [], 100]},
        permanent, ?SHUTDOWN_TIME_OUT, worker, [eredis_client]
    },

    {ok,{{one_for_one,5,10}, [Ngrmsmt_server, Wp_server, Eredis]}}.

