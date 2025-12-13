{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (reverse)

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Text.Regex.Applicative ((=~), some, sym)
import Text.Regex.Applicative.Common (decimal)

import qualified Data.IntMap as M
import qualified Data.Set as S

data Coord = Coord {y, x :: Int} deriving (Eq, Ord, Show)
data Rotation = Clockwise | Counterclockwise deriving (Eq, Ord, Show, Enum)
data Direction = North | East | South | West deriving (Eq, Ord, Show, Enum)

delta :: Coord -> Coord -> (Int, Int)
delta (Coord fromy fromx) (Coord toy tox) = (dy, dx)
  where dy = signum (toy - fromy)
        dx = signum (tox - fromx)

step :: Direction -> Coord -> Coord
step dir (Coord y x) = Coord (y + dy) (x + dx)
  where (dy, dx) = case dir of
                     North -> (-1, 0)
                     South -> (1, 0)
                     East -> (0, 1)
                     West -> (0, -1)

travelDirection :: Coord -> Coord -> Direction
travelDirection from to =
  case delta from to of
    (-1, 0) -> North
    (1, 0) -> South
    (0, 1) -> East
    (0, -1) -> West
    dir -> error $ "Non-orthogonal line :" <> show dir

rotation :: Direction -> Direction -> Maybe Rotation
rotation old new = case (fromEnum old - fromEnum new) `mod` 4 of
  1 -> Just Counterclockwise
  3 -> Just Clockwise
  _ -> Nothing

invert :: Rotation -> Rotation
invert Clockwise = Counterclockwise
invert Counterclockwise = Clockwise

reverse :: Direction -> Direction
reverse = toEnum . (`mod` 2) . (+ 2) . fromEnum

rotate :: Rotation -> Direction -> Direction
rotate r d = toEnum $ (fromEnum d + dd) `mod` 4
  where dd = case r of
               Clockwise -> 1
               Counterclockwise -> -1

type Input = [Coord]

area :: Coord -> Coord -> Int
area (Coord y1 x1) (Coord y2 x2) = dim y1 y2 * dim x1 x2
  where dim a b = abs (a - b) + 1

part1 :: Input -> Int
part1 input = maximum . map (uncurry area) $ liftA2 (,) input input

part2 :: Input -> Int
part2 input = maximum . map (uncurry area) . filter valid $ liftA2 (,) input input
  where orientation | fst (dimensions input) < 0 = Counterclockwise
                    | otherwise = Clockwise
        input' = input <> take 2 input
        edges = zip input' (drop 1 input')
        corners = zip3 input' (drop 1 input') (drop 2 input)
        reds = S.fromList input
        (candidateBlacks, greens) = mconcat $ do
          (from, to) <- edges
          let dir = travelDirection from to
              outsideDirection = rotate (invert orientation) dir
          do
            tile <- takeWhile (/= to) . iterate (step dir) $ from
            pure (S.singleton $ step outsideDirection tile, S.singleton tile)
        moreCandidateBlacks = S.fromList $ do
          (a, b, c) <- corners
          let dir1 = travelDirection a b
              dir2 = travelDirection b c
              turn = rotation dir1 dir2
          guard $ turn == Just orientation
          [step dir1 b, step (reverse dir2) b]
        blacks = (candidateBlacks <> moreCandidateBlacks) S.\\ reds S.\\ greens
        outside = M.fromListWith (<>) $ do
          Coord y x <- S.toList blacks
          pure (y, S.singleton x)
        valid (p@(Coord y1 x1), q@(Coord y2 x2)) = p < q && all S.null sets
          where (_, m') = M.split miny outside
                (m'', _) = M.split maxy m'
                sets = do
                  s <- M.elems m''
                  let (_, s') = S.split minx s
                      (s'', _) = S.split maxx s'
                  pure s''
                miny = min y1 y2 - 1
                minx = min x1 x2 - 1
                maxy = max y1 y2 + 1
                maxx = max x1 x2 + 1

dimensions :: Input -> (Int, Int)
dimensions input = (area, perimeter)
  where area = sum chunks `div` 2
          where chunks = do
                  i <- [1..n]
                  pure $ (y (get i)) * (x (get (i - 1)) - x (get (i + 1)))
        perimeter = sum $ zipWith distance corners (drop 1 corners)
        distance (Coord y1 x1) (Coord y2 x2) = magnitude y1 y2 + magnitude x1 x2
          where magnitude a b = abs (a - b)
        corners = input <> take 1 input
        n = length corners
        get i = corners !! ((i - 1) `mod` n)

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = some (tile <* sym '\n')
        tile = Coord <$> (decimal <* sym ',') <*> decimal

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
