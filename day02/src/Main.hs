{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.List.Split (chunksOf)
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative ((=~), sym)
import Text.Regex.Applicative.Common (decimal)
import Control.Applicative.Combinators (sepBy)

data InclusiveRange = InclusiveRange Int Int deriving (Eq, Ord, Show)

type Input = [InclusiveRange]

contents :: InclusiveRange -> [Int]
contents (InclusiveRange lo hi) = [lo..hi]

part1 :: Input -> Int
part1 = sum . (>>= filter invalid . contents)
  where invalid x = front == back
          where s = show x
                mid = length s `div` 2
                (front, back) = splitAt mid s

part2 :: Input -> Int
part2 = sum . (>>= filter invalid . contents)
  where invalid n = any go [1..len `div` 2]
          where go size = case chunksOf size s of
                  (x:more) -> all (== x) more
                  _ -> error $ "invalid: bad spiit of " <> s
                s = show n
                len = length s

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = (range `sepBy` sym ',') <* sym '\n'
        range = InclusiveRange <$> decimal <*> (sym '-' *> decimal)

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
