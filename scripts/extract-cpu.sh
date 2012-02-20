#!/bin/bash
grep -vE '^($|\s*#)' $@ | sed -n -e '3~4p'| awk '{print $2}'
