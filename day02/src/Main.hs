{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative ((=~), sym)
import Text.Regex.Applicative.Common (decimal)
import Control.Applicative.Combinators (sepBy)

data InclusiveRange = InclusiveRange Int Int deriving (Eq, Ord, Show)

type Input = [InclusiveRange]

invalid :: Int -> Bool
invalid x = front == back
  where s = show x
        mid = length s `div` 2
        (front, back) = splitAt mid s

invalids :: InclusiveRange -> [Int]
invalids (InclusiveRange lo hi) = filter invalid $ [lo..hi]

part1 :: Input -> Int
part1 = sum . (>>= invalids)

part2 :: Input -> String
part2 = show

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = (range `sepBy` sym ',') <* sym '\n'
        range = InclusiveRange <$> decimal <*> (sym '-' *> decimal)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
