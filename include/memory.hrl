



-define(MEMORY_LIMIT_NGRAMS, config:get('memory limit parallel ngrams', 750)).
-define(MEMORY_LIMIT_NGRAMS_BYTES, ?MEMORY_LIMIT_NGRAMS*1024*1024).

-define(MEMORY_LIMIT_PARALLEL, config:get('memory limit parallel', 40)).
-define(MEMORY_LIMIT_PARALLEL_BYTES, ?MEMORY_LIMIT_PARALLEL*1024*1024).



-define(READER_WORKER_SENTENCES_BUFFER_SIZE, config:get('reader worker sentences buffer size', 10)).

