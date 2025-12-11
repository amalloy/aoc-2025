{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (lookup)

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Foldable (forM_)
import Data.List (sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import Data.Containers.ListUtils (nubOrd)

import Data.Union (lookup, Node(..))
import Data.Union.ST (runUnionST, new, merge, flatten)

import Text.Regex.Applicative ((=~), sym, some)
import Text.Regex.Applicative.Common (decimal)

data Coord = Coord Int Int Int deriving (Eq, Ord, Show)

type Input = [Coord]

connect :: Int -> Input -> [(Int, Int)]
connect numConnections coords = let labeled = zip [0..] coords
                                    size = length labeled
                                    distances = do
                                      (i, p) <- labeled
                                      (j, q) <- labeled
                                      guard $ i < j
                                      pure (distance p q, (i, j))
                                    connections = take numConnections . sort $ distances
                                    connected = runUnionST $ do
                                      forest <- new size 1
                                      forM_ connections $ \(_dist, (i, j)) ->
                                        merge forest (\x y -> (x + y, ())) i j
                                      flatten forest
                                      pure forest
                                    circuits = nubOrd $ do
                                      i <- [0..size - 1]
                                      let (Node n, weight) = lookup connected (Node i)
                                      pure (n, weight)
                                in circuits
  where distance :: Coord -> Coord -> Double
        distance (Coord a b c) (Coord x y z) = sqrt . fromIntegral $ go a x + go b y + go c z
          where go n m = (n - m) ^ (2 :: Int)

part1 :: Input -> Int
part1 = product . take 3 . sortOn Down . map snd . connect 1000

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = some coord
        coord = Coord <$> (decimal <* comma) <*> (decimal <* comma) <*> (decimal <* newline)
        comma = sym ','
        newline = sym '\n'

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
