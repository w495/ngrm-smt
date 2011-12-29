#!/bin/sh

PATH=$PATH:/usr/local/lib/
export PATH

NODENAME="ngrmsmt@localhost"
CONFIG=app_dev

COOKIE=2e9582e552bf7ffd812db42183426c65b5bd9baef677ff3acab7c15c0760433b

ERL_ARGS="+K true +A 128 +P 1000000"

ERL_MAX_ETS_TABLES=140000
export ERL_MAX_ETS_TABLES


erl \
    -pa ./ebin \
    -pa ./deps/*/ebin \
    -boot start_sasl \
    -config ${CONFIG} \
    -sname ${NODENAME} \
    -s reloader \
    +native \
    -setcookie ${COOKIE} \
    ${ERL_ARGS} \
    -ngrmsmt errlog_type error -eval "application:start(ngrmsmt)"
    "$@"

