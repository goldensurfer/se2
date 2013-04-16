#!/bin/bash

erl -eval "gamer:start_link($1, $2, $3)."
