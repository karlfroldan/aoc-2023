module Main (main) where

import Advent.Types
import Advent.Days

import System.Environment (getArgs)

main :: IO ()
main =
  getArgs >>= problemIdFromArg >>=
      \x -> case problem x of
              Nothing -> putStrLn "Problem not found"
              Just p  -> runProblem p
  where
    problemIdFromArg  = return . read . head

problem :: Int -> Maybe Problem
problem n = case n of
  1 -> Just day1
  2 -> Just day2
  _ -> Nothing
