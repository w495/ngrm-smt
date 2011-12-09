%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% НЕ ПРИМЕНИМО
%%%
%%%     Слишком экстримальные ограничения на объем данных.
%%%     Возможно, так же позникнут проблемы аналогичные с mnesia.
%%% 
%%% http://www.erlang.org/doc/man/dets.html:
%%%
%%%     Dets is used by the Mnesia application, and is provided as is for users
%%%     who are interested in an efficient storage of Erlang terms on disk only.
%%%     Many applications just need to store some terms in a file.
%%%     Mnesia adds transactions, queries, and distribution.
%%%     The size of Dets files cannot exceed 2 GB. If larger tables are needed,
%%%     Mnesia's table fragmentation can be used.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(db_dets).

-export([init/0]).

init() ->
    ok.

