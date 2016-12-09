module TestLloyd15 where

import Test.HUnit
import Lloyd15

testGenerateBoard1 :: Test
testGenerateBoard1 = TestCase $ assertEqual ""
        [0..boardHeight*boardWidth-1] (map fst $ generateBoard)

testGenerateBoard2 :: Test
testGenerateBoard2 = TestCase $ assertEqual ""
        cellvals (map snd $ generateBoard)

main :: IO Counts
main = runTestTT $ TestList [
        testGenerateBoard1,
        testGenerateBoard2
        ]
