-module(main).
-export([main/0, main_test/0]).

-include("../include/common.hrl").
-include("../include/words.hrl").

main_test() ->
    core:train(?CORPORA).

main() ->

    % fprof:start()
    core:train(?CORPORA),

    %% core:train(?CORPORA),

    % fprof:apply(main, main_test, []).
    % fprof:profile().
    % fprof:analyse(dest, []).

    io:format("~nmain~n"),
    ok.
