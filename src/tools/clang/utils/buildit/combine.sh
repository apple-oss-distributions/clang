#!/bin/bash

i=0
args=()

for archive in $@
do
  ARCHS=`file $archive | awk '/.*for architecture/ { print $4}' | sed 's/).*//'`
  for arch in $ARCHS
  do
    lipo $archive -thin $arch -output $archive.$arch
    args[$i]="$archive.$arch"
    ((++i))
  done
  rm $archive
done

lipo -create ${args[@]} -o $1
rm ${args[@]}
