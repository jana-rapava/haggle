#!/bin/bash

dir=$(cd -P -- "$(dirname -- "$0")" && pwd -P)

echo $dir

runghc -i"$dir" -i"$dir"/tests ./examples/BeFS2x2/testSearchFirstBeFS0c.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BeFS2x2/testSearchBeFS0c.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BeFS2x2/testSearchBeFS0n.hs

runghc -i"$dir" -i"$dir"/tests ./examples/BeFS4x4/testSearchFirstBeFS1xmanhattan_sum.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BeFS4x4/testSearchFirstBeFS1yBasic.hs 2> testlog
runghc -i"$dir" -i"$dir"/tests ./examples/BeFS4x4/testSearchFirstBeFS1ymanhattan_sum.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BeFS4x4/testSearchFirstBeFS1ymisplaced.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BeFS4x4/testSearchFirstBeFSxBasic.hs 2> testlog
runghc -i"$dir" -i"$dir"/tests ./examples/BeFS4x4/testSearchTwoBeFS1xmisplaced.hs

runghc -i"$dir" -i"$dir"/tests ./examples/BFS2x2/testSearchBFS0_11.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BFS2x2/testSearchBFS0_12.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BFS2x2/testSearchBFS0_13.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BFS2x2/testSearchBFS0Fail_5.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BFS2x2/testSearchBFS0Fail_6.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BFS2x2/testSearchBFS0.hs

runghc -i"$dir" -i"$dir"/tests ./examples/BFS4x4/testSearchFirstBFS1x_2.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BFS4x4/testSearchFirstBFS1x.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BFS4x4/testSearchFirstBFS1y_2.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BFS4x4/testSearchFirstBFS1y_4.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BFS4x4/testSearchFirstBFS1y.hs
runghc -i"$dir" -i"$dir"/tests ./examples/BFS4x4/testSearchFirstBFSx_1.hs

runghc -i"$dir" -i"$dir"/tests ./examples/combined/example01.hs
runghc -i"$dir" -i"$dir"/tests ./examples/combined/example02.hs
runghc -i"$dir" -i"$dir"/tests ./examples/combined/example03.hs
runghc -i"$dir" -i"$dir"/tests ./examples/combined/example04.hs
