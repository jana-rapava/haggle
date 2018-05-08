import Backlog
-- used algorithms
import BFS
import BeFS
-- additional modules with problem representation
import Lloyd15
import Success4x4
import TestFixtures

main =  getOneSolution $ do
          with board1y
          bfs 5
          befs manhattan_sum
