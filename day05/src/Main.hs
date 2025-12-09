{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)

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

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = Input <$> some range <*> (newline *> some (ingredient <* newline))
        range = Range <$> ingredient <*> (sym '-' *> ingredient <* newline)
        ingredient = decimal
        newline = sym '\n'

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
