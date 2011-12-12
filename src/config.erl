
%% 
%% Config store
%%

-module(config).

-include("../include/common.hrl").


-export([get/2]).

get(Key, Default) ->
    case catch(application:get_env(?APP, Key)) of
        {'EXIT',_}  -> Default;
        {ok,Val}    -> Val;
        _           -> Default
    end.
