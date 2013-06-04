#!/bin/bash

OPTS_COMMON="-pa apps/*/ebin -pa deps/*/ebin -env ERL_CRASH_DUMP_SECONDS 1 -boot start_sasl"

usage() {
    echo usage: `basename $0` --connect_to IP:PORT --nick NICK
}

if [ $# -lt 4 ] ; then
    usage
    exit 1
fi

OIFS=$IFS
IFS=':'
ARRAY=($2)
HOST=${ARRAY[0]}
PORT=${ARRAY[1]}
IFS=$OIFS

if [ $# -eq 4 ] ; then
    erl $OPTS_COMMON -eval "gamer:start_link(\"$HOST\", $PORT, $4)."
fi
