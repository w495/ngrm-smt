-module(main).
-export([main/0]).

-include("../include/db.hrl").

main() ->
    %core:train({"data/en", "data/ru"}),
    %core:train({"data/knuth.en", "data/knuth.tr"}),
    %core:train({"data/knuth.en", "data/knuth.tr"}),
    core:train({"data/h-1000-un.en", "data/h-1000-un.ru"}),

    io:format("~nmain~n"),
    ok.

    %test_mnesia:word_counts_mnesia("data/un.en", "data/un.ru").
    %test_tree:word_counts_tree("data/un.en", "data/un.ru").
