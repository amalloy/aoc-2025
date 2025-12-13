{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative ((=~), some, sym)
import Text.Regex.Applicative.Common (decimal)

import qualified Data.Set as S

data Coord = Coord Int Int deriving (Eq, Ord, Show)

type Input = S.Set Coord

part1 :: Input -> Int
part1 input = maximum . map (uncurry area) $ liftA2 (,) tiles tiles
  where tiles = S.toList input
        area (Coord y1 x1) (Coord y2 x2) = dim y1 y2 * dim x1 x2
        dim a b = abs (a - b) + 1

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = S.fromList <$> some (tile <* sym '\n')
        tile = Coord <$> (decimal <* sym ',') <*> decimal

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
