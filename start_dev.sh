#!/bin/sh

PATH=$PATH:/usr/local/lib/
export PATH

NODENAME="ngrmsmt@localhost"
CONFIG=app_dev

ERL_ARGS="+K true +A 128 +P 1000000"

ERL_MAX_ETS_TABLES=140000
export ERL_MAX_ETS_TABLES


erl \
    -pa ./ebin \
    -pa ./deps/*/ebin \
    -boot start_sasl \
    -config ${CONFIG} \
    -sname ${NODENAME} \
    -mnesia dir "db" \
    -s reloader \
    ${ERL_ARGS} \
    -ngrmsmt errlog_type error -eval "application:start(ngrmsmt)"
    "$@"

