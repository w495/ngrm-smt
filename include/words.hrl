

%
% Корпуса текста, на которых проходит обучение
%

-define(CORPORA, config:get('corpora', {"priv/data/en", "priv/data/ru"})).


% 
% Cредняя длинна слова в русском языке 5.28 символов.
% Cредняя длинна предложения --- 10.38 символов.
% Для английского эти цифры будут меньше.
% Далее мы будем рассматривать предложения короче 40 слов.
% MAX = 40 * 5.28. = 211.20000000000002 < 256.
% Предложения (последовательность символов)
% длиннее этой величины будут обрубаться.
%

-define(CHAR_PER_LINE_MAX_COUNT, config:get('char per line max count', 256)).


%
% Cредняя длинна предложения в русском языке --- 10.38 слов.
% Возьмем следующее ограничение на длинну.
% Предложения (списки слов) длиннее этой величины будут обрубаться.
%

-define(WORDS_PER_LINE_MAX_COUNT, config:get('words per line max count', 40)).



%
% Считаем словом любую последовательность символов между пробелами
%

-define(WORDS_SEPARATOR_SRING, config:get('words separator sring', " ")).

%
% Считаем словом любую последовательность символов между пробелами
%

-define(NGRAM_SIZE, config:get('ngram size', 5)).


-define(NGRAM_DIAGONAL_OFFSET, config:get('ngram diagonal offset', 2)).


-define(ERLANG_MACHINE_EPSILON, math:pow(2, -1074)).


-define(MODEL_TABLE_T_EF_NORMA, config:get('model table t(e|f) norma', ?ERLANG_MACHINE_EPSILON)).

-define(MODEL_TABLE_DEFAULT,    config:get('model table default', 0.0)).

-define(MODEL_ITER_STEPS,       config:get('model iter steps', 10)).      % 5

-define(MODEL_CROP_LIMIT,       config:get('model crop limit', 0.005)).


