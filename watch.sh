#!/usr/bin/env bash

shopt -s globstar

trap "trap - SIGTERM && kill -- -$$" SIGINT SIGTERM EXIT

(/usr/bin/env expect <<EOF
set timeout -1
# On any interrupt, reload and attempt to run the app
trap {
    send \003
    send ":r\n"
    send "import ChoreWheel\n"
    send "runApp\n"
} SIGINT
spawn cabal repl
expect Ok*
send "import ChoreWheel\n"
send "runApp\n"
expect never
EOF
) &

restart_pid=$!

while sleep 0.1; do
    ls src/**/*.hs | entr -p -d kill -SIGINT $restart_pid;
done
