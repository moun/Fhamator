#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
RESULTS="$DIR/results/"
ORACLES="$DIR/oracles/"


pass=0
total=0

function test () {
  for file in `ls *.c`
  do
    ((total++))
    ../analyse "-$1" "$file" > "$RESULTS/$file.$1.out"
    if cmp -s "$RESULTS/$file.$1.out" "$ORACLES/$file.$1.out"
    then
       ((pass++))
    else
       echo "$file failed"
    fi
  done
  echo "$pass / $total examples passed"
}

test "interval"

test "card"
