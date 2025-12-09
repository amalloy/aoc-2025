{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)
import Data.Interval ((<=..<=), width, Extended(Finite))
import Data.IntervalSet (singleton, toList)

import Text.Regex.Applicative ((=~), sym, some)
import Text.Regex.Applicative.Common (decimal)

type Ingredient = Int
data Range = Range Ingredient Ingredient deriving (Eq, Ord, Show)
data Input = Input [Range] [Ingredient] deriving Show

contains :: Range -> Ingredient -> Bool
(Range lo hi) `contains` x = x >= lo && x <= hi

part1 :: Input -> Int
part1 (Input ranges available) = length . filter fresh $ available
  where fresh i = any (`contains` i) ranges

part2 :: Input -> Int
part2 (Input ranges _) = sum . map size . toList $ combinedRange
  where combinedRange = mconcat $ do
          Range lo hi <- ranges
          pure . singleton $ Finite lo <=..<= Finite hi
        size = succ . width -- Because we're counting fence posts, not fence length

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = Input <$> some range <*> (newline *> some (ingredient <* newline))
        range = Range <$> ingredient <*> (sym '-' *> ingredient <* newline)
        ingredient = decimal
        newline = sym '\n'

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
