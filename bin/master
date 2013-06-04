#!/bin/bash

OPTS_COMMON="-pa apps/*/ebin -pa deps/*/ebin -env ERL_CRASH_DUMP_SECONDS 1 -boot start_sasl"

usage() {
    echo usage: `basename $0` --connect HOST:PORT
}

if [ $# -lt 2 ] ; then
    usage
    exit 1
fi

OIFS=$IFS
IFS=':'
ARRAY=($2)
HOST=${ARRAY[0]}
PORT=${ARRAY[1]}
IFS=$OIFS

erl $OPTS_COMMON -eval "gma:start_all(\"$HOST\", $PORT)."
