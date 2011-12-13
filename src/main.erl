-module(main).
-export([main/0]).

-include("../include/common.hrl").
-include("../include/words.hrl").

main() ->
    core:train(?CORPORA),

    io:format("~nmain~n"),
    ok.
