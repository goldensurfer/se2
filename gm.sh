#!/bin/bash

OPTS_COMMON="-pa apps/*/ebin -pa deps/*/ebin -boot start_sasl"

erl $OPTS_COMMON -eval "gm_client:start_link(\"$1\", $2, $3, \"$4\")."
