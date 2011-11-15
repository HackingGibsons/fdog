#!/usr/bin/env bash
BASE=$(dirname $(greadlink -f $0 2> /dev/null || readlink -f $0 2> /dev/null))
export AFDOG=$BASE/$(basename $0)
/usr/bin/env sbcl --script $BASE/afdog.script
