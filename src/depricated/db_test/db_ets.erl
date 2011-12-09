%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% НЕ ПРИМЕНИМО
%%%
%%%     Этот вариант мы не используем.
%%%     Нет эффективного способа держать в памяти нужный объем данных,
%%%     при этом поддерживать его актуальность.
%%% 
%%% http://www.erlang.org/doc/man/ets.html:
%%% 
%%%     Data is organized as a set of dynamic tables, which can store tuples.
%%%     Each table is created by a process. When the process terminates,
%%%     the table is automatically destroyed.
%%% 
%%%
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(db_ets).
-export([init/0]).

init() ->
    _Source_target_counter = ets:new('source_target_counter',  [named_table] ),
    % named_table --- таблица имнованная доступ по имени.
    ok.

