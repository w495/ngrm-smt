-module(core).
-compile(export_all).

-include("../include/common.hrl").
-include("../include/processes.hrl").
-include("../include/words.hrl").


train({Filename_1, Filename_2}) ->

    DataBase = spawn(db, start, []),


    Worker = spawn(worker, start, []),
    Reader = spawn(reader, start, []),


    Reader ! {read, {Filename_1, Filename_2}},
    Reader ! {worker, Worker},

    Worker ! {reader, Reader},



    exit(Reader, normal),
    exit(Worker, normal),
    exit(DataBase, normal),

    ok.

