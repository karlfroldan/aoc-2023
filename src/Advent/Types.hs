module Advent.Types (Problem (..), runProblem)  where

data Problem =
  Problem { problemDay :: Int
          , part1      :: [String] -> IO ()
          , part2      :: [String] -> IO ()
          }

runProblem :: Problem -> IO ()
runProblem p = do
  input <- readFile fname >>= return . lines
  putStrLn "Part 1"
  part1 p input
  putStrLn "Part 2"
  part2 p input
  where fname = concat [ "data/day", show (problemDay p) ]
