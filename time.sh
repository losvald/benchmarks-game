#!/bin/sh
mkdir -f "times"
echo "./run.sh $@"
( time ( ./run.sh $@ > /dev/null 2>&1 ) 2>&1 ) | tail -n 3
