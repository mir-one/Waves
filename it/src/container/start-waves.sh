#!/bin/bash

trap 'kill -TERM $PID' TERM INT
echo Options: $MIR_OPTS
java $MIR_OPTS -jar /opt/mir/mir.jar /opt/mir/template.conf &
PID=$!
wait $PID
trap - TERM INT
wait $PID
EXIT_STATUS=$?
