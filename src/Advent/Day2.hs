{-# LANGUAGE RecursiveDo #-}

module Advent.Day2 where

import Data.Either (rights)
import Data.List (foldl', concat)
import Data.Void (Void)
import Advent.Types (Problem (..))
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)

import qualified Text.Megaparsec.Char as MC

data Color = Red Int | Blue Int | Green Int deriving (Eq, Show)
type Set = [Color]

type Parser = Parsec Void String

data Game =
  Game { gameId :: Int
       , sets   :: [Set]
       } deriving (Eq, Show)

day2 :: Problem
day2 = Problem 2 p1 p2

isRed (Red _) = True
isRed _       = False

isGreen (Green _) = True
isGreen _         = False

isBlue (Blue _) = True
isBlue _        = False

liftColor :: Color -> Int
liftColor (Red n) = n
liftColor (Blue n) = n
liftColor (Green n) = n

p1 :: [String] -> IO ()
p1 input = print (sumGameIds games)
  where
    inputs = rights (parser <$> input)
    games = goodGames req inputs
    req = (12, 13, 14) -- rgb

p2 :: [String] -> IO ()
p2 input = print (sum $ map (tupleMult . minimumColor) inputs)
  where
    inputs = rights (parser <$> input)

tupleMult (x, y, z) = x * y * z

minimumColor :: Game -> (Int, Int, Int)
minimumColor = f . concat . sets
  where
    f ss = (rs, gs, bs)
      where
        g h = maximum . map liftColor . filter h
        rs = g isRed ss
        gs = g isGreen ss
        bs = g isBlue ss

sumGameIds :: [Game] -> Int
sumGameIds = sum . map gameId

colorSum :: [Set] -> (Int, Int, Int)
colorSum = foldl' f (0, 0, 0) . concat
  where
    f (r, g, b) (Red n) = (r + n, g, b)
    f (r, g, b) (Green n) = (r, g + n, b)
    f (r, g, b) (Blue n) = (r, g, b + n)

goodSet :: (Int, Int, Int) -> Set -> Bool
goodSet (r, g, b) = foldl' f True
  where f a c = case c of
          Red n   -> a && n <= r
          Green n -> a && n <= g
          Blue n  -> a && n <= b

goodGame :: (Int, Int, Int) -> Game -> Bool
goodGame r g = all id $ goodSet r <$> (sets g)

goodGames r = filter (goodGame r)

-- rgb tuple
possibleGames :: (Int, Int, Int) -> [Game] -> [Game]
possibleGames req =
  filter (\g -> colorSum (sets g) `threeWayCompare` req)
  where threeWayCompare (a, b, c) (x, y, z) =
          a <= x && b <= y && c <= y

parser = parse grammar ""

grammar :: Parser Game
grammar = do
  string "Game"
  space
  gid <- decimal
  char ':'
  space
  sets <- setsParser
  return (Game gid sets)
  where
    setsParser = colors `sepBy1` string "; "
    colors = colorParser `sepBy1` string ", "

colorParser :: Parser Color
colorParser = do
  count <- read <$> some digitChar
  space
  colorName <- some letterChar
  case colorName of
    "red" -> return $ Red count
    "green" -> return $ Green count
    "blue" -> return $ Blue count
    _ -> fail "invalid color"
