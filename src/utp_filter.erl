-module(utp_filter).
-export([start_test/1,trace_test/0]).


start_test(ExtraOptions) ->
   Options =
       [{event_order, event_ts},
        {scale, 2},
        {max_actors, 10},
        {detail_level, 90},
        {actors, [fred, bob, hank, renee]},
        {trace_pattern, {utp, max}},
        {trace_global, true},
        {title, "uTP tracer"} | ExtraOptions],
   et_viewer:start(Options).

trace_test() ->
   Events = [{fred, bob, order_food},
             {bob, hank, order_food},
             {bob, fred, serve_wine},
             {hank, bob, pickup},
             {bob, fred, serve_feed},
             {fred, renee, pay}],
   [utp:report_event(50, F, T, L, [])
    || {F,T,L} <- Events].
