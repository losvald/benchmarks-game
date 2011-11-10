#!/bin/sh
grep -vE '^($|\s*#)' $@ | sed -n '1~2p'
