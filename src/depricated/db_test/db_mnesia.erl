%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%
%%% УСТАРЕЛО
%%% 
%%%     Этот вариант мы не используем, потому что при большом объеме данных
%%%     mnesia начинает сильно тормозить и в какой-то момент просто падает.
%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-module(db_mnesia).
-export([init/0,
        put_stc/2,
        get_stc/1,
        get_stc_counter/1,
        get_all/1]).

-include_lib("stdlib/include/qlc.hrl").

-record(source_target_counter, {source, target, counter}).

init() ->
    mnesia:create_schema([node()]),
    mnesia:start(),
    mnesia:create_table(source_target_counter, [ {disc_copies, [node()] }, {attributes, record_info(fields,source_target_counter)} ]).

%%% =========================================================================
%%%
%%% SOURCE_TARGET_COUNTER
%%%

put_stc({Src, Tgt}, Cnt) ->
    Fun = fun() ->
            mnesia:write(#source_target_counter{ source=Src, target=Tgt, counter=Cnt})
         end,
    mnesia:transaction(Fun).

get_stc({Src, Tgt}) ->
    Fun =
        fun() ->
            mnesia:match_object({source_target_counter, Src, Tgt, '_' } )
        end,
    {atomic, Results} = mnesia:transaction(Fun),
    Results.

get_stc_counter(Src_Tgt) ->
    [{source_target_counter, _Src, _Tgt, Cnt}] = get_stc(Src_Tgt),
    Cnt.

%%% =========================================================================

get_all(Db_name) ->
    mnesia:transaction(
    fun() ->
        qlc:eval( qlc:q(
            [ X || X <- mnesia:table(Db_name) ]
        ))
    end ).
