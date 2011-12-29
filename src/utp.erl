-module(utp).
-export([report_event/4, report_event/5]).

report_event(DetailLevel, FromTo, Label, Contents) ->
   %% N.B External call
   ?MODULE:report_event(DetailLevel, FromTo, FromTo, Label, Contents).

report_event(_DetailLevel, _From, _To, _Label, _Contents) ->
   hopefully_traced.

