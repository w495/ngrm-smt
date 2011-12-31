-module(ngrmsmt_server).
-behaviour(gen_server).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2,
          handle_info/2, terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


init([]) ->
     {ok, []}.

terminate(_, _) ->
     ok.

handle_cast(start, _Data) ->
     {noreply, []};

handle_cast(stop, Data) ->
     {stop, normal, Data}.

handle_call(_, _From, _Data) ->
     {reply, [], []}.

handle_info(Info, Data) ->
     error_logger:info_report([{module, ?MODULE},
                              {line, ?LINE},
                              {self, self()},
                              {message, Info}]),
     {noreply, Data}.

code_change(_, Data, _) ->
     {ok, Data}.

