module Advent.Day1 (day1) where

import Advent.Types (Problem (..))
import Data.List (foldl', intercalate)
import Data.Char (isDigit)

formDigit :: (Integral a, Read a) => String -> a
formDigit = read . g . f
  where f = filter isDigit
        g s' = [head s', last s']

formDigit1 :: [Int] -> Int
formDigit1 x = (head x * 10) + (last x)

allDigits = reverse . flip allDigits' []

-- Returns the digits in the given string in reverse order.
allDigits' :: String -> [Int] -> [Int]
allDigits' "" acc = acc
allDigits' ss acc = case matchDigitStr ss of
  (Nothing, _)  -> allDigits' (tail ss) (f (head ss) ++ acc)
  (Just x, ss') -> allDigits' ss' (x:acc)
  where
    f c = case tryDigit [c] of
      Nothing -> []
      Just d  -> [d]

matchDigitStr :: String -> (Maybe Int, String)
matchDigitStr s = g . foldl' f Nothing $ digits
  where
    f (Just x) _ = Just x
    f Nothing ds = 
      if ds == take (length ds) s
      then Just ds
      else Nothing
    g Nothing = (Nothing, s)
    g (Just x) = (Just (strToDigit x), drop (length x) s)

digits :: [String]
digits = [ "zero"
         , "one"
         , "two"
         , "three"
         , "four"
         , "five"
         , "six"
         , "seven"
         , "eight"
         , "nine"
         ]

strToDigit :: String -> Int
strToDigit s = case s of
  "zero"  -> 0
  "one"   -> 1
  "two"   -> 2
  "three" -> 3
  "four"  -> 4
  "five"  -> 5
  "six"   -> 6
  "seven" -> 7
  "eight" -> 8
  "nine"  -> 9

tryDigit :: String -> Maybe Int
tryDigit s
  | isDigit (head s) = Just . read $ s
  | otherwise = Nothing

day1 :: Problem
day1 = Problem probId part1 part2 
  where
    probId = 1
    part1 input = print (sum . map formDigit $ input)
    part2 input = print (sum . map (formDigit1 . allDigits) $ input)
