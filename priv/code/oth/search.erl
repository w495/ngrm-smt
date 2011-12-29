-module(search).
-compile(export_all).


new(Options) ->

    Usememory = true,

    % Poll = lambda x: x.pop

    Maxdepth = false, %unlimited
    Minimize = false, %minimize rather than maximize the score function? default: no
    Keeptraversal = false,
    Goalstates = [],
    Debug = false,

    case Options of
        {} -> 1;
        Pd -> Pd + 1
    end
    .

