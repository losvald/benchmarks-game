#!/bin/bash
grep -vE '^($|\s*#)' $@ | sed -n -e '2~2p'| awk '{print $2}'
