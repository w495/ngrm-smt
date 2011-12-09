%% @copyright 2007 Mochi Media, Inc.
%% @author Matthew Dempsky <matthew@mochimedia.com>
%%
%% @doc Erlang module for automatically reloading modified modules
%% during development.

-module(reloader).
-author("Matthew Dempsky <matthew@mochimedia.com>").

-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).
-export([start/0, start_link/0, reload_code/0]).
-export([stop/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {last, tref}).

%% External API

%% @spec start() -> ServerRet
%% @doc Start the reloader.
start() ->
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% @spec start_link() -> ServerRet
%% @doc Start the reloader.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% @spec stop() -> ok
%% @doc Stop the reloader.
stop() ->
    gen_server:call(?MODULE, stop).

%% gen_server callbacks

%% @spec init([]) -> {ok, State}
%% @doc gen_server init, opens the server in an initial state.
init([]) ->
    {ok, TRef} = timer:send_interval(timer:seconds(1), doit),
    {ok, #state{last = stamp(), tref = TRef}}.

%% @spec handle_call(Args, From, State) -> tuple()
%% @doc gen_server callback.
handle_call(stop, _From, State) ->
    {stop, shutdown, stopped, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State}.

%% @spec handle_cast(Cast, State) -> tuple()
%% @doc gen_server callback.
handle_cast(_Req, State) ->
    {noreply, State}.

%% @spec handle_info(Info, State) -> tuple()
%% @doc gen_server callback.
handle_info(doit, State) ->
    Now = stamp(),
    doit(State#state.last, Now),
    {noreply, State#state{last = Now}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @spec terminate(Reason, State) -> ok
%% @doc gen_server termination callback.
terminate(_Reason, State) ->
    {ok, cancel} = timer:cancel(State#state.tref),
    ok.


%% @spec code_change(_OldVsn, State, _Extra) -> State
%% @doc gen_server code_change callback (trivial).
code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%% Internal API

doit(From, To) ->
    [case file:read_file_info(Filename) of
         {ok, FileInfo} when FileInfo#file_info.mtime >= From,
                             FileInfo#file_info.mtime < To ->
             reload(Module);
         {ok, _} ->
             unmodified;
         {error, enoent} ->
             %% The Erlang compiler deletes existing .beam files if
             %% recompiling fails.  Maybe it's worth spitting out a
             %% warning here, but I'd want to limit it to just once.
             gone;
         {error, Reason} ->
             io:format("Error reading ~s's file info: ~p~n",
                       [Filename, Reason]),
             error
     end || {Module, Filename} <- code:all_loaded(), is_list(Filename)].

reload(Module) ->
    io:format("Reloading ~p ...", [Module]),
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} ->
            io:format(" ok.~n"),
            case erlang:function_exported(Module, test, 0) of
                true ->
                    io:format(" - Calling ~p:test() ...", [Module]),
                    case catch Module:test() of
                        ok ->
                            io:format(" ok.~n"),
                            reload;
                        Reason ->
                            io:format(" fail: ~p.~n", [Reason]),
                            reload_but_test_failed
                    end;
                false ->
                    reload
            end;
        {error, Reason} ->
            io:format(" fail: ~p.~n", [Reason]),
            error
    end.


stamp() ->
    erlang:localtime().

%% ============================================================================
reload_code() ->
    try
      Reload = fun(M) ->
                   code:purge(M),
                   code:soft_purge(M),
                   {module, M} = code:load_file(M),
                   {ok,M}
               end,
%      Modules = [M || {M,P} <- code:all_loaded(),
%                               is_list(P) andalso
%                               string:str(P, filename:absname(""))>0],
      Modules = modified_modules(),
      {ok, lists:sort([Reload(M) || M <- Modules])}
    catch
  Cls:Why -> {error, {Cls,Why}}
    end.

modified_modules() ->
    [M || {M, P} <- code:all_loaded(),
                    is_list(P),
                    string:str(P, filename:absname(""))>0,
                    module_modified(M) == true].

module_modified(Module) ->
    case code:is_loaded(Module) of
  {file, preloaded} ->
      false;
  {file, Path} ->
      CompileOpts = proplists:get_value(compile, Module:module_info()),
      CompileTime = proplists:get_value(time, CompileOpts),
      Src = proplists:get_value(source, CompileOpts),
      module_modified(Path, CompileTime, Src);
  _ ->
      false
    end.

module_modified(Path, PrevCompileTime, PrevSrc) ->
    case find_module_file(Path) of
  false ->
      false;
  ModPath ->
      {ok, {_, [{_, CB}]}} = beam_lib:chunks(ModPath, ["CInf"]),
      CompileOpts = binary_to_term(CB),
      CompileTime = proplists:get_value(time, CompileOpts),
      Src = proplists:get_value(source, CompileOpts),
      not (CompileTime == PrevCompileTime) and (Src == PrevSrc)
    end.

find_module_file(Path) ->
    case file:read_file_info(Path) of
  {ok, _} ->
      Path;
  _ ->
      %% may be the path was changed?
      case code:where_is_file(filename:basename(Path)) of
    non_existing ->
        false;
    NewPath ->
        NewPath
      end
    end.
