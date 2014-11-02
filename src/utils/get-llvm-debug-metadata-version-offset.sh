#!/bin/sh
# Return the Apple-internal offset to the metadata debug info version.
# This expects the clang repository string to be passed in as an argument.
if [ $# -ne 1 ]; then
    # Empty repository string? return 0.
    echo 0 && exit 0
fi
# This replicates the code in tools/clang/lib/Basic/Targets.cpp
if echo $1 | grep -v -q '^clang.*-'; then
    echo 0 && exit 0
fi

buf=$(echo   $1 | sed -E 's/^clang.*-//')
maj=$(echo $buf | grep -o '^[0-9]\+')

buf=$(echo $buf | sed -E 's/^[0-9]+.//')
min=$(echo $buf | grep -o '^[0-9]\+')

buf=$(echo $buf | sed -E 's/^[0-9]+.//')
mic=$(echo $buf | grep -o '^[0-9]\+')

if [ -z "$maj" -o -z "$min" -o -z "$mic" ]; then
   # parse error
   echo 0 && exit 0
fi

echo $(( $maj*1000*1000 + $min*100*1000 + $mic*1000 ))
