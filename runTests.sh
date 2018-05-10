#!/bin/bash

dir=$(cd -P -- "$(dirname -- "$0")" && pwd -P)

runghc -i"$dir" -i"$dir"/tests tests/TestBeFS2x2.hs
runghc -i"$dir" -i"$dir"/tests tests/TestBeFS4x4.hs 2> testlog2
runghc -i"$dir" -i"$dir"/tests tests/TestBFS2x2.hs
runghc -i"$dir" -i"$dir"/tests tests/TestBFS4x4.hs
