{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Control.Arrow ((&&&))
import Data.Foldable (sequenceA_)

import qualified Data.Map.Strict as M
import Control.Monad.Trans.Writer.CPS (Writer, execWriter, tell)

data Coord = Coord Int Int deriving (Eq, Ord, Show)
type Intensity = Int
newtype SplitterInfo = SplitterInfo Intensity deriving (Eq, Ord, Show)

moveDown :: Coord -> Coord
moveDown (Coord y x) = Coord (y + 1) x

type Input = (Coord, M.Map Coord SplitterInfo)

part1 :: Input -> Int
part1 (start, splitters) = go splitters (M.singleton start 1)
  where (lastSplitterPos, _) = M.findMax splitters
        go splitters queue | pos > lastSplitterPos = length . filter visited . M.elems $ splitters
                           | otherwise = case M.lookup pos' splitters of
                               Nothing -> go splitters (M.insert pos' intensity queue')
                               Just (SplitterInfo i) -> go splitters' queue''
                                 where splitters' = M.insert pos' (SplitterInfo (intensity + i)) splitters
                                       queue'' = M.unionWith (+) queue' . M.fromList $ do
                                         dx <- [-1, 1]
                                         pure $ (Coord y (x + dx), intensity)
          where ((pos, intensity), queue') = M.deleteFindMin queue
                pos'@(Coord y x) = moveDown pos
                visited (SplitterInfo i) = i > 0

part2 :: Input -> ()
part2 = const ()

prepare :: String -> Input
prepare file = case parse (lines file) of
  ([start], splitters) -> (start, M.fromList $ do
                              s <- splitters
                              pure (s, SplitterInfo 0))
  (starts, _) -> error $ "bad starts: " <> show starts
  where parse rows = execWriter (sequenceA_ grid)
          where grid :: [Writer ([Coord], [Coord]) ()]
                grid = do
                  (y, row) <- zip [0..] rows
                  (x, c) <- zip [0..] row
                  let coord = [Coord y x]
                  pure $ case c of
                    '.' -> pure ()
                    'S' -> tell (coord, mempty)
                    '^' -> tell (mempty, coord)
                    e -> error $ "Unexpected token " <> show e

main :: IO ()
main = readFile "input.txt" >>= print . (part1 &&& part2) . prepare
