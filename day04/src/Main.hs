{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Control.Monad (guard)

import qualified Data.Set as S

data Coord = Coord Int Int deriving (Eq, Ord, Show)
instance Semigroup Coord where
  (Coord y1 x1) <> (Coord y2 x2) = Coord (y1 + y2) (x1 + x2)
instance Monoid Coord where
  mempty = Coord 0 0

type Input = S.Set Coord

directions :: S.Set Coord
directions = S.fromList . tail $ do
  dy <- [0, 1, -1]
  dx <- [0, 1, -1]
  pure $ Coord dy dx

accessibles :: S.Set Coord -> S.Set Coord
accessibles rolls = S.filter accessible $ rolls
  where accessible roll = length neighbors < 4
          where neighbors = S.intersection rolls . S.map (roll <>) $ directions

part1 :: Input -> Int
part1 = length . accessibles

part2 :: Input -> Int
part2 rolls = length rolls - (length . fixPoint removeAccessibles $ rolls)
  where removeAccessibles r = r S.\\ accessibles r
        fixPoint f = fst . head . dropWhile (uncurry (/=)) . (zip <*> tail) . iterate f

prepare :: String -> Input
prepare text = S.fromList $ do
  (y, row) <- zip [0..] $ lines text
  (x, c) <- zip [0..] row
  guard $ c == '@'
  pure $ Coord y x

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
