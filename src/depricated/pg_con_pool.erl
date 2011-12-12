-compile(export_all).

-module(pg_con_pool).

-behaviour(gen_server).

%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("../include/common.hrl").

-include_lib("epgsql/include/pgsql.hrl").
%% ----------------------------------------------------------------------------
%% Defines
%% ----------------------------------------------------------------------------

-record(state, {connections, cPool, tasks, reconnectionTime, necessary, dbHost, dbName, dbUser, dbPassword}).
-define(RECONNECT_TIMEOUT, 5000).

%% ----------------------------------------------------------------------------
%% External exports
%% ----------------------------------------------------------------------------
-export([start_link/4, withConnection/2, withTransaction/2]).

%% --------------------------------------------------------------------
%% gen_server callbacks
%% --------------------------------------------------------------------
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).


%% ====================================================================
%% External functions
%% ====================================================================
start_link(Username, Password, DbName, DbHost)->
    Name = convert:to_atom(?FMT("pgconpool-~p", [DbName])),
    %% flog:info(?FMT("NAME: ~p", [Name])),
    Ret = gen_server:start_link({local, Name}, ?MODULE, [Username, Password, DbName, DbHost], []),
    Ret.

withConnection(Fun, DbName) ->
    Name = convert:to_atom(?FMT("pgconpool-~p", [DbName])),
    case whereis(Name) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, {con_request, self()}),
            receive
                {pgconnection, Con} -> 
                    try 
                        Res = Fun(Con),
                        gen_server:cast(Pid, {con_free, Con}),
                        Res
                    catch
                        E:R -> 
                            %%io:format("WITHCONNECTION ERROR (~p): ~p - ~p~n", [Con, E, R]),
                            pgsql:squery(Con, "rollback"),
                            %catch(pgsql:close(Con)), % ?? может его не убивать? тестовый вариант - отпускаю соединение в пул после ошибки.
                            gen_server:cast(Pid, {con_free, Con}), % ВНИМАНИЕ! может порождать ошибку
                            {pgcp_error, {E, R}}
                    end
            end;
        _ ->
            {error, {not_started, DbName}}
    end.
 
withTransaction(Fun, DbName) ->
    withConnection(
        fun(Con) ->
                pgsql:squery(Con, "begin"),
                Ret = Fun(Con),
                pgsql:squery(Con, "commit"),
                Ret
        end, DbName).


mkCast(Param, DbName) ->
    Name = convert:to_atom(?FMT("pgconpool-~p", [DbName])),
    case whereis(Name) of
        Pid when is_pid(Pid) ->
            gen_server:cast(Pid, Param);
        _ -> {error, {not_started, DbName}}
    end.


%% ====================================================================
%% Server functions
%% ====================================================================

%% --------------------------------------------------------------------
%% Function: init/1
%% Description: Initiates the server
%% Returns: {ok, State}          |
%%          {ok, State, Timeout} |
%%          ignore               |
%%          {stop, Reason}
%% --------------------------------------------------------------------

init([Username, Password, DbName, DbHost]) ->
    process_flag(trap_exit, true),
%    Username = config:get(db_user, "cff"),
%    DbName = config:get(db_name, "test1"),
%    DbHost= config:get(db_host, "localhost"),
    {ok, #state{
        connections=[], cPool = [], necessary=10, reconnectionTime=getNow(), tasks=[], 
        dbHost=DbHost, dbUser=Username, dbPassword=Password, dbName=DbName
    }, 0}.

%% --------------------------------------------------------------------
%% Function: handle_call/3
%% Description: Handling call messages
%% Returns: {reply, Reply, State}          |
%%          {reply, Reply, State, Timeout} |
%%          {noreply, State}               |
%%          {noreply, State, Timeout}      |
%%          {stop, Reason, Reply, State}   | (terminate/2 is called)
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------

handle_call(Request, _From, State) ->
    %% flog:info(?FMT("~p ~p~n", [?MODULE,{unexpected_call,Request}])),
    {NState, Timeout} = checkReconnection(State),
    Reply = {error, unexpected_call},
    {reply, Reply, NState, Timeout}.

%% --------------------------------------------------------------------
%% Function: handle_cast/2
%% Description: Handling cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_cast({con_request, Pid}, State=#state{connections=Cons, tasks=Tasks}) ->
    {RestTasks, RestCons} = execTasks(Tasks++[Pid], Cons),
    {NState, Timeout} = checkReconnection(State#state{connections=RestCons, tasks=RestTasks}),
    {noreply, NState, Timeout};
handle_cast({con_free, Con}, State=#state{tasks=Tasks, connections=Cons, necessary=N, cPool=CPool}) ->
%%%handle_cast({con_free, Con}, State=#state{tasks=Tasks, connections=Cons}) ->
%    flog:info(?FMT("confree ~p, tasks: ~p cons:~p pool:~p, necess: ~p~n", [Con, length(Tasks), length(Cons), length(CPool), N])),
    {RestTasks, RestCons} = execTasks(Tasks, [Con|Cons]),
    {NState, Timeout} = checkReconnection(State#state{connections=RestCons, tasks=RestTasks}),
    {noreply, NState, Timeout};
handle_cast(Msg, State) ->
    %% flog:info(?FMT("~p ~p~n", [?MODULE,{unexpected_cast, Msg}])),
    {NState, Timeout} = checkReconnection(State),
    {noreply, NState, Timeout}.

%% --------------------------------------------------------------------
%% Function: handle_info/2
%% Description: Handling all non call/cast messages
%% Returns: {noreply, State}          |
%%          {noreply, State, Timeout} |
%%          {stop, Reason, State}            (terminate/2 is called)
%% --------------------------------------------------------------------
handle_info(timeout, State=#state{tasks=Tasks, necessary=N}) ->
    %% flog:info(?FMT("RECONNECT TIMER ~p ... tasks: ~p~n", [N, length(Tasks)])),
    {NState, Timeout} = checkReconnection(State),
    {noreply, NState, Timeout};
handle_info({'EXIT', Con, Reason}, State=#state{connections=Cons, cPool=CPool, necessary=N, tasks=Tasks}) ->
   %%  flog:info(?FMT("Postgres connection ~p closed. Reason: ~p tasks: ~p cons:~p pool:~p, necess: ~p~n", 
   %%              [Con, Reason, length(Tasks), length(Cons), length(CPool), N])),
    case lists:member(Con, CPool) of
        true ->
            %% flog:info(?FMT("so.. reconnect ~p~n",[Con])),
            NewNecessary = N+1;
        false ->
            %% flog:info(?FMT("illegal con ~p in ~p~n",[Con, CPool])),
            NewNecessary = N
    end,

    StateStage1 = State#state{necessary=NewNecessary, cPool=CPool--[Con], connections=Cons--[Con]}, % 100% no Con in pools
    {NState, Timeout} = checkReconnection(StateStage1),
    {noreply, NState, Timeout};
handle_info({Ref, {ok, Con}}, State) when is_pid(Con), is_reference(Ref) ->
    %% flog:info(?FMT("MB Connection timeout result income: ~p.~n", [Con])),
    catch(pgsql:close(Con)),
    {NState, Timeout} = checkReconnection(State),
    {noreply, NState, Timeout};
handle_info(Info, State) ->
    %% flog:info(?FMT("~p ~p~n", [?MODULE, {unexpected_info, Info}])),
    {NState, Timeout} = checkReconnection(State),
    {noreply, NState, Timeout}.

%% --------------------------------------------------------------------
%% Function: terminate/2
%% Description: Shutdown the server
%% Returns: any (ignored by gen_server)
%%% --------------------------------------------------------------------
terminate(Reason, #state{connections=Cons}) ->
    closeConnections(Cons),
    global:unregister_name(?MODULE),
    %% flog:info(?FMT("~p ~p~n", [?MODULE, {terminate, Reason}])),
    ok.

%% --------------------------------------------------------------------
%% Func: code_change/3
%% Purpose: Convert process state when code is changed
%% Returns: {ok, NewState}
%% --------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% --------------------------------------------------------------------
%% Local
%% --------------------------------------------------------------------
getNow() ->
    {_M,S,Mi} = now(),
    S*1000000+Mi.

closeConnections([C|T]) ->
    pgsql:close(C),
    closeConnections(T);
closeConnections([]) ->
    done.

initConnectionPool(0, Ret, _DbHost, _Username, _Password, _DbName) ->
    Ret;
initConnectionPool(N, Ret, DbHost, Username, Password, DbName) ->
    case catch(pgsql:connect(DbHost, Username, Password, [{database, DbName}, {timeout, 3600000}])) of
        {ok, Con}-> 
            %% flog:info(?FMT("new connection ~p~n",[Con])),
            initConnectionPool(N-1, [Con|Ret], DbHost, Username, Password, DbName);
        {Error, Reason} -> 
            %% flog:info(?FMT("init connection error ~p : ~p~n",[Error, Reason])),
            Ret
    end.

initConnectionPool(N, DbHost, Username, Password, DbName) ->
    initConnectionPool(N, [], DbHost, Username, Password, DbName).

execTasks(Tasks, Cons=[Con|RC]) ->
    {Pid, Tasks2} = getNextTask(Tasks),
    if
        Pid =:= none ->
            {[], Cons};
        true ->
            Pid ! {pgconnection, Con},
            execTasks(Tasks2, RC)
    end;
execTasks(RT, RC) ->
    {RT, RC}.

checkReconnection(State=#state{necessary=0}) ->
    {State#state{reconnectionTime=infinity}, infinity};
checkReconnection(State=#state{reconnectionTime=infinity}) ->
    {State#state{reconnectionTime=getNow() + ?RECONNECT_TIMEOUT*1000}, ?RECONNECT_TIMEOUT};
checkReconnection(State=#state{reconnectionTime=RT, necessary=N, connections=Cons, cPool=CPool, tasks=Tasks, 
                                dbHost=DbHost, dbUser=Username, dbName=DbName, dbPassword=Password}) ->
    Now = getNow(),
    if
        Now < RT -> 
            NState = State,
            Timeout = trunc((RT - Now)/1000);
        true -> 
            NewCons = initConnectionPool(N, DbHost, Username, Password, DbName),
            {RestTasks, RestCons} = execTasks(Tasks, Cons++NewCons),
            %% flog:info(?FMT("Reconnecting: necessary - ~p, new - ~p, rest: ~p~n", [N, length(NewCons), length(RestCons)])),
            NState = State#state{necessary=N-length(NewCons), reconnectionTime=infinity, connections=RestCons, tasks=RestTasks, 
                                    cPool=CPool++NewCons},
            Timeout = infinity
    end,
    {NState, Timeout}.

getNextTask([Pid|T]) ->
    case lists:member(Pid, processes()) of
        true ->
            {Pid, T};
        false ->
            %% flog:info(?FMT("DROP INVALID PID ~p~n", [Pid])),
            getNextTask(T)
    end;
getNextTask([]) ->
    {none, []}.

