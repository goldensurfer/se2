#!/bin/bash

ENV="ERL_CRASH_DUMP_SECONDS 1"

OPTS_COMMON="-pa apps/*/ebin -pa deps/*/ebin -env $ENV -boot start_sasl"

usage() {
    echo usage: `basename $0` HOST PORT ID [GAMETYPE]
}

if [ $# -lt 3 ] ; then
    usage
    exit 1
fi

if [ -z "$4" ] ; then
    echo "start 3"
    echo erl "$OPTS_COMMON" -eval "gm_client:start_link(\"$1\", $2, $3)."
else
    echo "start 4"
    echo erl "$OPTS_COMMON" -eval "gm_client:start_link(\"$1\", $2, $3, \"$4\")."
fi
