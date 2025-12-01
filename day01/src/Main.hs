{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))

import Data.Maybe (fromMaybe)

import Text.Regex.Applicative ((<|>), many, (=~), sym)
import Text.Regex.Applicative.Common (decimal)

data Direction = L | R deriving (Eq, Ord, Show, Enum)
data Rotation = Rotation Direction Int deriving (Eq, Ord, Show)

type Input = [Rotation]

positions :: [Rotation] -> [Int]
positions = scanl (+) 50 . map toOffset
  where toOffset (Rotation dir magnitude) = magnitude * case dir of
          L -> -1
          R -> 1

evenHundreds :: [Int] -> Int
evenHundreds = length . filter (== 0) . map (`mod` 100)

part1 :: Input -> Int
part1 = evenHundreds . positions

part2 :: Input -> Int
part2 = sum . (zipWith zeroes <*> tail) . positions
  where zeroes x y = evenHundreds (tail [x, next..y])
          where next = x + signum (y - x)

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = many (rotation <* sym '\n')
        rotation = Rotation <$> direction <*> decimal
        direction = (L <$ sym 'L') <|> (R <$ sym 'R')
-- The straightforward approach
-- prepare = map rotation . lines
--   where rotation ('L':n) = Rotation L (read n)
--         rotation ('R':n) = Rotation R (read n)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
