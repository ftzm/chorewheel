#!/usr/bin/env bash

shopt -s globstar

trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

./restart_ghci &
restart_pid=$!

while sleep 0.1; do ls src/**/*.hs | entr -p -d kill -SIGINT $restart_pid; done
