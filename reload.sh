#!/usr/bin/env bash

sigint_handler()
{
  kill $PID
  exit
}

trap sigint_handler SIGINT

while true; do
  #$@ &
  cabal run chorewheel-exe &
  PID=$!
  inotifywait -e modify -e move -e create -e delete -e attrib -r src
  kill -9 $PID
  killall chorewheel
  # kill -9 $(lsof -t -i:8080) # backup
  lsof -i :8080
  sleep 5
done
