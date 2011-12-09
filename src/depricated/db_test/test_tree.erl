-module(test_tree).
-compile(export_all).

-define(NGRM_SIZE, 5).


%%% ==========================================================================
%%%
%%% ------------------------------
%%% "ru":
%%%     Chernyi Kot
%%%     Chernyi Dom
%%%     Belyi Dom
%%%     Belyi Dom
%%%     Zelenyi
%%% ------------------------------
%%% "en":
%%%     Black Cat
%%%     Black House
%%%     White House
%%%     White House1
%%%     Green
%%% ------------------------------
%%%
%%% >>> test_tree:word_counts_tree("en", "ru").
%%%
%%% {["Belyi"],["White"]}: 2
%%% {["Belyi","Dom\n"],["White","House\n"]}: 1
%%% {["Belyi","Dom\n"],["White","House1\n"]}: 1
%%% {["Black"],["Chernyi"]}: 2
%%% {["Black","Cat\n"],["Chernyi","Kot\n"]}: 1
%%% {["Black","House\n"],["Chernyi","Dom\n"]}: 1
%%% {["Cat\n"],["Kot\n"]}: 1
%%% {["Chernyi"],["Black"]}: 2
%%% {["Chernyi","Dom\n"],["Black","House\n"]}: 1
%%% {["Chernyi","Kot\n"],["Black","Cat\n"]}: 1
%%% {["Dom\n"],["House\n"]}: 2
%%% {["Dom\n"],["House1\n"]}: 1
%%% {["Green\n"],["Zelenyi\n"]}: 1
%%% {["House\n"],["Dom\n"]}: 2
%%% {["House1\n"],["Dom\n"]}: 1
%%% {["Kot\n"],["Cat\n"]}: 1
%%% {["White"],["Belyi"]}: 2
%%% {["White","House\n"],["Belyi","Dom\n"]}: 1
%%% {["White","House1\n"],["Belyi","Dom\n"]}: 1
%%% {["Zelenyi\n"],["Green\n"]}: 1
%%%


word_counts_tree(Filename_1, Filename_2) ->
    case {file:open(Filename_1, read), file:open(Filename_2, read)} of
        {{ok, Io_device_1}, {ok, Io_device_2}} ->
            %Tree = process_each_line_tree({Io_device_1, Io_device_2}, gb_trees:empty());
            %print_tree(gb_trees:iterator(Tree));
            process_each_line_tree({Io_device_1, Io_device_2}, gb_trees:empty());
        {{error, Reason}, _} ->
            io:format("Filename_1 = ~s~n", [Reason]);
        {_, {error, Reason}} ->
            io:format("Filename_2 = ~s~n", [Reason])
    end.

%%% -------------------------------------------------------------------------

process_each_line_tree({Io_device_1, Io_device_2}, Tree) ->
    case {io:get_line(Io_device_1, ""), io:get_line(Io_device_2, "") } of
        {eof, _ } ->
            file:close(Io_device_1), file:close(Io_device_2),
            Tree;
        { _, eof} ->
            file:close(Io_device_1), file:close(Io_device_2),
            Tree;
        {{error, Reason}, _}->
            file:close(Io_device_1), throw(Reason);
        {_, {error, Reason}}->
            file:close(Io_device_1), throw(Reason);
        {Data_1, Data_2} ->
            NewTree = build_frequencies_tree(sentences:zip_sentences(Data_1,Data_2, ?NGRM_SIZE) , Tree),
            process_each_line_tree({Io_device_1, Io_device_2}, NewTree)
    end.

%%% -------------------------------------------------------------------------

print_tree(Iter) ->
    case gb_trees:next(Iter) of
        none -> ok;
        {Key, Val, NewIter} ->
            io:format("~p: ~w~n", [Key,Val]),
            print_tree(NewIter)
    end.

%%% -------------------------------------------------------------------------


%%% ==========================================================================
%%%
%%% Работает со сбалансированным деревом
%%%

build_frequencies_tree([], Tree) -> Tree;
build_frequencies_tree([W|R], Tree) ->
    case gb_trees:is_defined(W, Tree) of
        true ->
            Count = gb_trees:get(W, Tree),
            NewTree = gb_trees:update(W, Count + 1, Tree),
            build_frequencies_tree(R, NewTree);
        false ->
            NewTree = gb_trees:insert(W, 1, Tree),
            build_frequencies_tree(R, NewTree)
    end.

