{-# LANGUAGE LambdaCase #-}

module Main where

import Prelude hiding (lookup)

import Control.Arrow ((&&&))
import Control.Monad (guard)
import Data.Either (fromRight, fromLeft)
import Data.Foldable (forM_)
import Data.List (sort, sortOn)
import Data.Maybe (fromMaybe)
import Data.Ord (Down(..))
import Data.Containers.ListUtils (nubOrd)

import Data.Union (lookup, Node(..))
import Data.Union.ST (new, merge, flatten, unsafeFreeze)
import Control.Monad.ST (runST)
import Control.Monad.Trans.Except (runExceptT, throwE)
import Control.Monad.Trans.Class (lift)

import Text.Regex.Applicative ((=~), sym, some)
import Text.Regex.Applicative.Common (decimal)

data Coord = Coord Int Int Int deriving (Eq, Ord, Show)

type Input = [Coord]

connect :: Int -> Input -> Either (Coord, Coord) [(Int, Int)]
connect numConnections coords = let labeled = zip [0..] coords
                                    size = length labeled
                                    distances = do
                                      (i, p) <- labeled
                                      (j, q) <- labeled
                                      guard $ i < j
                                      pure (distance p q, (i, j))
                                    connections = take numConnections . sort $ distances
                                    union x y = (size', size == size')
                                      where size' = x + y
                                    result = runST $ runExceptT $ do
                                      forest <- lift $ new size 1
                                      forM_ connections $ \(_dist, (i, j)) ->
                                        (lift $ merge forest union i j) >>= \case
                                          Just True -> throwE (coords !! i, coords !! j)
                                          _ -> pure ()
                                      lift $ flatten forest
                                      lift $ unsafeFreeze forest
                                    uniquify connected = nubOrd $ do
                                      i <- [0..size - 1]
                                      let (Node n, weight) = lookup connected (Node i)
                                      pure (n, weight)
                                in fmap uniquify result
  where distance :: Coord -> Coord -> Double
        distance (Coord a b c) (Coord x y z) = sqrt . fromIntegral $ go a x + go b y + go c z
          where go n m = (n - m) ^ (2 :: Int)

part1 :: Input -> Int
part1 = product . take 3 . sortOn Down . map snd . fromRight undefined . connect 1000

part2 :: Input -> Int
part2 = uncurry wallDistance . fromLeft undefined . connect 1000000
  where wallDistance (Coord x _ _) (Coord a _ _ ) = a * x

prepare :: String -> Input
prepare = fromMaybe (error "no parse") . (=~ input)
  where input = some coord
        coord = Coord <$> (decimal <* comma) <*> (decimal <* comma) <*> (decimal <* newline)
        comma = sym ','
        newline = sym '\n'

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
