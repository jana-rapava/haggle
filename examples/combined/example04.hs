import Haggle
-- used algorithms
import BFS
import BeFS
-- additional modules with problem representation
import Lloyd15
import Success4x4
import TestFixtures

main =  getManySolutions 3 $ do
          with board1y
          bfs 5
          befs manhattan_sum
