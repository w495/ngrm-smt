%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% FOR FUTURE
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(wp_server).
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

handle_call({f_n, {Mod, Fun, Args}}, _From, State) when is_list(Args)->
    case code:is_loaded(Mod) of
        false ->
            case code:load_file(Mod) of
                {module, _} ->
                    {reply, apply(Mod, Fun, Args), State};
                false ->
                    {noreply, []}
            end;
        _ -> {reply, apply(Mod, Fun, Args), State}
    end;

handle_call({f_1, {Mod, Fun, Arg}}, _From, State) ->
     {reply, Mod:Fun(Arg), State};



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


