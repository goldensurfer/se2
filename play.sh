#!/bin/bash

OPTS_COMMON="-pa apps/*/ebin -pa deps/*/ebin -env ERL_CRASH_DUMP_SECONDS 1 -boot start_sasl"

erl $OPTS_COMMON -eval "gamer:start_link(\"$1\", $2, $3)."
