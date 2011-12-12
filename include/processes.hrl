

%%% processes


-define(READER_WORKER_TIMEOUT, config:get('reader worker timeout', 5000)).

-define(READER_COMMAND_TIMEOUT, config:get('reader worker timeout', 5000)).

-define(READER_READ_TIMEOUT, config:get('reader worker timeout', 100)).